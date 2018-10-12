# Temporary way to link to eggshell content without changing all processes

from eggshell.ocg.utils import temp_groups
from flyingpigeon import config
from eggshell.general.utils import archive, archiveextract, check_creationtime, download, FreeMemory, download_file, \
    searchfile, local_path, make_dirs, rename_complexinputs, prepare_static_folder
from eggshell.ocg.utils import calc_grouping, has_variable, temp_groups
from eggshell.nc.utils import get_coordinates, get_values, get_time, get_variable, get_calendar, get_timerange, \
    get_index_lat, get_frequency, get_domain, sort_by_filename, sort_by_time, unrotate_pole, rename_variable
from eggshell.esgf.utils import aggregations, drs_filename, ATTRIBUTE_TO_FACETS_MAP, search_landsea_mask_by_esgf

import os
from netCDF4 import Dataset
import requests

GROUPING = temp_groups.keys()


def guess_main_variables(ncdataset):
    """Guess main variables in a NetCDF file.
    :param ncdataset: netCDF4.Dataset
    :return list: names of main variables
    Notes
    -----
    The main variables are the one with highest dimensionality and size. The
    time, lon, lat variables and variables that are defined as bounds are
    automatically ignored.
    """

    var_candidates = []
    bnds_variables = []
    for var_name in ncdataset.variables:
        if var_name in ['time', 'lon', 'lat']:
            continue
        ncvar = ncdataset.variables[var_name]
        if hasattr(ncvar, 'bounds'):
            bnds_variables.append(ncvar.bounds)
        var_candidates.append(var_name)
    var_candidates = list(set(var_candidates) - set(bnds_variables))

    # Find main variables among the candidates
    nd = -1
    size = -1
    main_variables = []
    for var_name in var_candidates:
        ncvar = ncdataset.variables[var_name]
        if len(ncvar.shape) > nd:
            main_variables = [var_name]
            nd = len(ncvar.shape)
            size = ncvar.size
        elif (len(ncvar.shape) == nd) and (ncvar.size > size):
            main_variables = [var_name]
            size = ncvar.size
        elif (len(ncvar.shape) == nd) and (ncvar.size == size):
            main_variables.append(var_name)
    return main_variables


def opendap_or_download(resource, auth_tkt_cookie={}, output_path=None,
                        max_nbytes=10000000000):
    """Check for OPEnDAP support, if not download the resource.
    :param resource: url of a NetCDF resource
    :param output_path: where to save the non-OPEnDAP resource
    :param max_nbytes: maximum file size for download, default: 1 gb
    :return str: the original url if OPEnDAP is supported or path of saved file
    """

    try:
        nc = Dataset(resource, 'r')
        nc.close()
    except Exception:
        response = requests.get(resource, cookies=auth_tkt_cookie, stream=True)
        if response.status_code == 401:
            raise Exception("Not Authorized")

        if 'Content-Length' in response.headers.keys():
            if int(response.headers['Content-Length']) > max_nbytes:
                raise IOError("File too large to download.")
        chunk_size = 16 * 1024
        if not output_path:
            output_path = os.getcwd()
        output_file = os.path.join(output_path, os.path.basename(resource))
        with open(output_file, 'wb') as f:
            for chunk in response.iter_content(chunk_size):
                if chunk:
                    f.write(chunk)
        try:
            nc = Dataset(output_file, 'r')
            nc.close()
        except:
            raise IOError("This does not appear to be a valid NetCDF file.")
        return output_file
    return resource


class CookieNetCDFTransfer:
    def __init__(self, request, opendap_hostnames=[]):
        self.request = request
        self.cookie = None
        self.daprc_fn = '.daprc'
        self.auth_cookie_fn = 'auth_cookie'
        self.opendap_hostnames = opendap_hostnames

    def __enter__(self):
        self.cookie = get_auth_cookie(self.request)

        if self.cookie:
            with open(self.daprc_fn, 'w') as f:
                f.write('HTTP.COOKIEJAR = auth_cookie')

            with open(self.auth_cookie_fn, 'w') as f:
                for opendap_hostname in self.opendap_hostnames:
                    for key, value in self.cookie.items():
                        f.write('{domain}\t{access_flag}\t{path}\t{secure}\t{expiration}\t{name}\t{value}\n'.format(
                            domain=opendap_hostname,
                            access_flag='FALSE',
                            path='/',
                            secure='FALSE',
                            expiration=0,
                            name=key,
                            value=value))

    def __exit__(self, exc_type, exc_val, exc_tb):
        if self.cookie:
            os.remove(self.daprc_fn)
            os.remove(self.auth_cookie_fn)


def get_auth_cookie(pywps_request):
    try:
        return dict(auth_tkt=pywps_request.http_request.cookies['auth_tkt'])
    except KeyError:
        # No token... will be anonymous
        return None
