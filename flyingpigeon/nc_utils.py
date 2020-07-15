from netCDF4 import Dataset, MFDataset, num2date
from datetime import datetime as dt
import logging
import os
from os import path, rename
import requests

LOGGER = logging.getLogger("PYWPS")
# from esgf_utils import ATTRIBUTE_TO_FACETS_MAP


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


def aggregations(resource):
    """
    aggregates netcdf files by experiment. Aggregation examples:
    CORDEX: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    CMIP5:
    We collect for each experiment all files on the time axis:
    200101-200512, 200601-201012, ...
    Time axis is sorted by time.
    :param resource: list of netcdf files
    :return: dictionary with key=experiment
    """
    from .nc_utils import drs_filename, sort_by_time
    aggregations = {}
    for nc in resource:
        key = drs_filename(nc, skip_timestamp=True, skip_format=True)

        # collect files of each aggregation (time axis)
        if key in aggregations:
            aggregations[key]['files'].append(nc)
        else:
            aggregations[key] = dict(files=[nc])

    # collect aggregation metadata
    for key in aggregations.keys():
        # sort files by time
        aggregations[key]['files'] = sort_by_time(aggregations[key]['files'])
        # start timestamp of first file
        start, _ = get_timerange(aggregations[key]['files'][0])
        # end timestamp of last file
        _, end = get_timerange(aggregations[key]['files'][-1])
        aggregations[key]['from_timestamp'] = start
        aggregations[key]['to_timestamp'] = end
        aggregations[key]['start_year'] = int(start[0:4])
        aggregations[key]['end_year'] = int(end[0:4])
        aggregations[key]['variable'] = get_variable(aggregations[key]['files'][0])
        aggregations[key]['filename'] = "%s_%s-%s.nc" % (key, start, end)
    return aggregations


def get_auth_cookie(pywps_request):
    try:
        return dict(auth_tkt=pywps_request.http_request.cookies['auth_tkt'])
    except KeyError:
        # No token... will be anonymous
        return None


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
        except Exception:
            raise IOError("This does not appear to be a valid NetCDF file.")
        return output_file
    return resource


def get_coordinates(resource, variable=None, unrotate=False):
    """
    reads out the coordinates of a variable
    :param resource: netCDF resource file
    :param variable: variable name
    :param unrotate: If True the coordinates will be returned for unrotated pole
    :returns list, list: latitudes , longitudes
    """
    if type(resource) != list:
        resource = [resource]

    if variable is None:
        variable = get_variable(resource)

    if unrotate is False:
        try:
            if len(resource) > 1:
                LOGGER.exception('resource is a list containing {} files. Should be only one'.format(len(resource)))
            else:
                ds = Dataset(resource[0])

            var = ds.variables[variable]
            dims = list(var.dimensions)
            if 'time' in dims:
                dims.remove('time')
            # TODO: find position of lat and long in list and replace dims[0] dims[1]
            lats = ds.variables[dims[0]][:]
            lons = ds.variables[dims[1]][:]
            ds.close()
            LOGGER.info('got coordinates without pole rotation')
        except Exception:
            msg = 'failed to extract coordinates'
            LOGGER.exception(msg)
    else:
        # lats, lons = unrotate_pole(resource)
        # LOGGER.info('got coordinates with pole rotation')
        LOGGER.error('got coordinates with pole rotation is currently switched off')
    return lats, lons


def get_index_lat(resource, variable=None):
    """
    returns the dimension index of the latiude values
    :param resource:  list of path(s) to netCDF file(s) of one Dataset
    :param variable: variable name
    :return int: index
    """

    if variable is None:
        variable = get_variable(resource)
    if type(resource) != list:
        resource = [resource]
    if len(resource) == 1:
        ds = Dataset(resource[0])
    else:
        ds = MFDataset(resource)

    var = ds.variables[variable]
    dims = list(var.dimensions)

    if 'rlat' in dims:
        index = dims.index('rlat')
    if 'lat' in dims:
        index = dims.index('lat')
    if 'latitude' in dims:
        index = dims.index('latitude')
    if 'y' in dims:
        index = dims.index('y')
    return index


def get_variable(resources):
    """Guess main variables in a NetCDF file.
    (compare nc.ocg_utils.get_variable)

    :param resources: netCDF4.Dataset

    :return list: names of main variables

    Notes
    -----
    The main variables are the one with highest dimensionality and size. The
    time, lon, lat variables and variables that are defined as bounds are
    automatically ignored.
    """

    if type(resources) == str:
        ncdataset = Dataset(resources)
    elif type(resources) == list:
        ncdataset = MFDataset(resources)
    else:
        ncdataset = resources

    # dims = [key for key in ncdataset.dimensions.keys()]
    vars = [key for key in ncdataset.variables.keys()]

    # assume that main variables are 3D or 4D
    main_vars = []
    for var in vars:
        if len(ncdataset.variables[var].dimensions) >= 3:
            main_vars.append(var)
    if len(main_vars) > 1:
        LOGGER.exception('more than one 3D or 4D variable in file')
    if len(main_vars) == 0:
        LOGGER.exception('No 3D or 4D variable detected')
    return main_vars[0]

    # var_candidates = []
    # bnds_variables = []
    # for var_name in ncdataset.variables:
    #     if var_name in ['time', 'lon', 'lat']:
    #         continue
    #     ncvar = ncdataset.variables[var_name]
    #     if hasattr(ncvar, 'bounds'):
    #         bnds_variables.append(ncvar.bounds)
    #     var_candidates.append(var_name)
    # var_candidates = list(set(var_candidates) - set(bnds_variables))
    #
    # # Find main variables among the candidates
    # nd = -1
    # size = -1
    # main_variables = []
    # for var_name in var_candidates:
    #     ncvar = ncdataset.variables[var_name]
    #     if len(ncvar.shape) > nd:
    #         main_variables = [var_name]
    #         nd = len(ncvar.shape)
    #         size = ncvar.size
    #     elif (len(ncvar.shape) == nd) and (ncvar.size > size):
    #         main_variables = [var_name]
    #         size = ncvar.size
    #     elif (len(ncvar.shape) == nd) and (ncvar.size == size):
    #         main_variables.append(var_name)
    # return main_variables


def sort_by_filename(resource, historical_concatination=False):
    """
    Sort a list of files with CORDEX-conformant file names.

    :param resource: netCDF file
    :param historical_concatination: if True (default=False), appropriate historial
                                    runs will be sorted to the rcp datasets
    :return  dictionary: {'drs_filename': [list of netCDF files]}
    """
    from os import path
    if type(resource) == str:
        resource = [resource]
    nc_datasets = {}
    tmp_dic = {}

    try:
        if len(resource) > 1:
            LOGGER.debug('sort_by_filename module start sorting %s files' % len(resource))
            # LOGGER.debug('resource is list with %s files' % len(resource))
            try:  # if len(resource) > 1:
                # collect the different experiment names
                for nc in resource:
                    # LOGGER.info('file: %s' % nc)
                    p, f = path.split(path.abspath(nc.replace('.nc', '')))
                    n = f.split('_')
                    if len([int(i) for i in n[-1].split('-') if i.isdigit()]) == 2:
                        bn = '_'.join(n[0:-1])  # skipping the date information in the filename
                        nc_datasets[bn] = []  # dictionary containing all datasets names
                    elif len([int(i) for i in n[-2].split('-') if i.isdigit()]) == 2:
                        bn = '_'.join(n[0:-2])  # skipping the date information in the filename
                        nc_datasets[bn] = []  # dictionary containing all datasets names
                    else:
                        LOGGER.exception('file is not DRS convention conform!')
                LOGGER.info('found %s datasets', len(nc_datasets.keys()))
            except Exception:
                LOGGER.exception('failed to find names of datasets!')
            LOGGER.info('check for historical/RCP datasets')
            try:
                if historical_concatination is True:
                    # select only necessary names
                    rcp_datasets = nc_datasets.copy()
                    if any("_rcp" in s for s in nc_datasets.keys()):
                        for key in nc_datasets.keys():
                            if 'historical' in key:
                                rcp_datasets.pop(key)
                        nc_datasets = rcp_datasets.copy()
                        LOGGER.info('historical data set names removed from dictionary')
                    else:
                        LOGGER.info('no RCP dataset names found in dictionary')
            except Exception:
                LOGGER.exception('failed to pop historical data set names!')
            LOGGER.info('start sorting the files')
            try:
                for key in nc_datasets:
                    try:
                        if historical_concatination is False:
                            for n in resource:
                                if '%s_' % key in n:
                                    nc_datasets[key].append(path.abspath(n))  # path.join(p, n))

                        elif historical_concatination is True:
                            key_hist = key.replace('rcp26', 'historical').\
                                replace('rcp45', 'historical').\
                                replace('rcp65', 'historical').\
                                replace('rcp85', 'historical')
                            for n in resource:
                                if '{}_'.format(key_hist) in n:
                                    nc_datasets[key].append(path.abspath(n))
                                if '{}_'.format(key) in n:
                                    nc_datasets[key].append(path.abspath(n))  # path.join(p, n))
                        else:
                            LOGGER.error('append file paths to dictionary for key %s failed' % key)
                        nc_datasets[key].sort()
                    except Exception:
                        LOGGER.exception('failed for %s ' % key)
            except Exception:
                LOGGER.exception('failed to populate the dictionary with appropriate files')
            for key in nc_datasets.keys():
                try:
                    nc_datasets[key].sort()
                    start, _ = get_timerange(nc_datasets[key][0])  # get first timestep of first file
                    _, end = get_timerange(nc_datasets[key][-1])  # get last  timestep of last file
                    newkey = key + '_' + start + '-' + end
                    tmp_dic[newkey] = nc_datasets[key]
                except Exception:
                    msg = 'failed to sort the list of resources and add dates to keyname: %s' % key
                    LOGGER.exception(msg)
                    tmp_dic[key] = nc_datasets[key]
                    # raise Exception(msg)
        elif len(resource) == 1:
            p, f = path.split(path.abspath(resource[0]))
            tmp_dic[f.replace('.nc', '')] = path.abspath(resource[0])
            LOGGER.debug('only one file! Nothing to sort, resource is passed into dictionary')
        else:
            LOGGER.debug('sort_by_filename module failed: resource is not 1 or >1')
        LOGGER.info('sort_by_filename module done: %s datasets found' % len(nc_datasets))
    except Exception:
        msg = 'failed to sort files by filename'
        LOGGER.exception(msg)
        raise Exception(msg)
    return tmp_dic


def sortssp_by_drsname(resource):
    nc_datasets = {}
    tmp_dic = {}

    try:
        for nc in resource:
            # LOGGER.info('file: %s' % nc)
            p, f = path.split(path.abspath(nc.replace('.nc', '')))
            n = f.split('_')
            if len([int(i) for i in n[-1].split('-') if i.isdigit()]) == 2:
                bn = '_'.join(n[0:-1])  # skipping the date information in the filename
                nc_datasets[bn] = []  # dictionary containing all datasets names
            elif len([int(i) for i in n[-2].split('-') if i.isdigit()]) == 2:
                bn = '_'.join(n[0:-2])  # skipping the date information in the filename
                nc_datasets[bn] = []  # dictionary containing all datasets names
            else:
                print('file is not DRS convention conform!')

            #  select only necessary names
            ssp_datasets = nc_datasets.copy()
            if any("_ssp" in s for s in nc_datasets.keys()):
                for key in nc_datasets.keys():
                    if 'historical' in key:
                        ssp_datasets.pop(key)
                nc_datasets = ssp_datasets.copy()
                print('historical data set names removed from dictionary')
            else:
                print('no SSP dataset names found in dictionary')
        print('Got dataset names for dic keys')
    except Exception as e:
        print('failed to get dataset names for dic keys {}'.format(e))

    # collect the file according to datasets
    for key in nc_datasets:
        try:
#             if historical_concatination is False:
#                 for n in resource:
#                     if '%s_' % key in n:
#                         nc_datasets[key].append(path.abspath(n))  # path.join(p, n))
#    ex = ['ssp119', 'ssp126', 'ssp245', 'ssp370',  'ssp434','ssp460','ssp585',

#             elif historical_concatination is True:
            key_hist = key.replace('ssp119', 'historical').\
                replace('ssp126', 'historical').\
                replace('ssp245', 'historical').\
                replace('ssp370', 'historical').\
                replace('ssp434', 'historical').\
                replace('ssp460', 'historical').\
                replace('ssp585', 'historical')
            for n in resource:
                if '{}_'.format(key_hist) in n:
                    nc_datasets[key].append(path.abspath(n))
                if '{}_'.format(key) in n:
                    nc_datasets[key].append(path.abspath(n))  # path.join(p, n))
#             else:
#                 LOGGER.error('append file paths to dictionary for key %s failed' % key)
            nc_datasets[key].sort()
        except Exception as e:
            print('failed for{e}'.fromat(e))
    return nc_datasets

# def get_calendar(resource, variable=None):
#     """
#     returns the calendar and units in wich the timestamps are stored
#
#     :param resource: netCDF file or files of one Dataset
#
#     :return str: calendar, unit
#     """
#
#     if type(resource) != list:
#         resource = [resource]
#
#     try:
#         if len(resource) > 1:
#             ds = MFDataset(resource)
#         else:
#             ds = Dataset(resource[0])
#         time = ds.variables['time']
#     except:
#         msg = 'failed to get time'
#         LOGGER.exception(msg)
#         raise Exception(msg)
#
#     if hasattr(time, 'units') is True:
#         unit = time.units
#     else:
#         unit = None
#
#     if hasattr(time, 'calendar') is True:
#         calendar = time.calendar
#     else:
#         calendar = None
#     return str(calendar), str(unit)

#
# def get_domain(resource):
#     """
#     returns the domain of a netCDF file
#
#     :param resource: netCDF file (metadata quality checked!)
#
#     :return str: domain
#     """
#     try:
#         ds = Dataset(resource)
#         if 'CMIP' in ds.project_id or 'EUCLEIA' in ds.project_id:
#             domain = None
#             LOGGER.debug('resource belongs to a global experiment project')
#         elif 'CORDEX' in ds.project_id:
#             domain = ds.CORDEX_domain
#             LOGGER.info('resource belongs to CORDEX')
#         else:
#             LOGGER.debug('No known project_id found in meta data')
#         ds.close()
#     except Exception as e:
#         LOGGER.debug('Could not specify domain for %s: %s' % (resource, e))
#     return domain
#
#
def get_frequency(resource):
    """
    returns the frequency as set in the metadata (see also metadata.get_frequency)

    :param resource: NetCDF file

    :return str: frequency
    """
    ds = Dataset(resource)

    try:
        frequency = ds.frequency
        LOGGER.info('frequency written in the meta data:  %s', frequency)
    except Exception as ex:
        msg = "Could not specify frequency for %s : %s" % (resource, ex)
        LOGGER.exception(msg)
        raise Exception(msg)
    else:
        ds.close()
    return frequency


def get_timerange(resource):
    """
    returns from/to timestamp of given netcdf file(s).

    :param resource: list of path(s) to netCDF file(s)

    :returns netcdf.datetime.datetime: start, end

    """
    start = end = None

    if type(resource) != list:
        resource = [resource]
    LOGGER.debug('length of recources: %s files' % len(resource))

    try:
        resource.sort()
        if len(resource) > 1:
            # ds = MFDataset(resource)
            LOGGER.error('functon expect single file, Mulitple files found {}'.format(len(resource)))
        else:
            ds = Dataset(resource[0])
            LOGGER.debug('Dataset loaded for %s file in resource:' % len(resource))
        time = ds.variables['time']

        if (hasattr(time, 'units') and hasattr(time, 'calendar')) is True:
            s = num2date(time[0], time.units, time.calendar)
            e = num2date(time[-1], time.units, time.calendar)
        elif hasattr(time, 'units'):
            s = num2date(time[0], time.units)
            e = num2date(time[-1], time.units)
        else:
            s = num2date(time[0])
            e = num2date(time[-1])

        # TODO: include frequency
        start = '%s%s%s' % (s.year, str(s.month).zfill(2), str(s.day).zfill(2))
        end = '%s%s%s' % (e.year, str(e.month).zfill(2), str(e.day).zfill(2))
        ds.close()
    except Exception:
        msg = 'failed to get time range'
        LOGGER.exception(msg)
        ds.close()
        raise Exception(msg)
    return start, end


def get_time(resource):
    """
    returns all timestamps of given netcdf file as datetime list.

    :param resource: NetCDF file(s)

    :return : list of timesteps
    """
    # :param format: if a format is provided (e.g format='%Y%d%m'), values will be converted to string

    if type(resource) != list:
        resource = [resource]

    try:
        if len(resource) > 1:
            ds = MFDataset(resource)
        else:
            ds = Dataset(resource[0])
        time = ds.variables['time']
    except Exception as ex:
        msg = 'failed to get time {}'.format(ex)
        LOGGER.exception(msg)
        raise Exception(msg)

    try:
        if (hasattr(time, 'units') and hasattr(time, 'calendar')) is True:
            timestamps = num2date(time[:], time.units, time.calendar)
        elif hasattr(time, 'units'):
            timestamps = num2date(time[:], time.units)
        else:
            timestamps = num2date(time[:])
        ds.close()
        try:
            ts = [dt.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in timestamps]

            # if date_format is not None:
            #     ts = [t.strftime(format=date_format) for t in timestamps]
            # else:
            #    ts = [dt.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in timestamps]

            # TODO give out dateformat by frequency
            # ERROR: ValueError: unconverted data remains: 12:00:00
            # from flyingpigeon.metadata import get_frequency

            # frq = get_frequency(resource)
            # if frq is 'day':
            #     ts = [dt.strptime(str(i), '%Y-%m-%d') for i in timestamps]
            # elif frq is 'mon':
            #     ts = [dt.strptime(str(i), '%Y-%m') for i in timestamps]
            # elif frq is 'sem':
            #     ts = [dt.strptime(str(i), '%Y-%m') for i in timestamps]
            # elif frq is 'yr':
            #     ts = [dt.strptime(str(i), '%Y') for i in timestamps]
            # else:
            #     ts = [dt.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in timestamps]
        except Exception as e:
            msg = 'failed to convert times to string: {}'.format(e)
            LOGGER.exception(msg)
    except Exception as e:
        msg = 'failed to convert time: {}'.format(e)
        LOGGER.exception(msg)
    return ts


def get_values(resource, variable=None, time_range=None):
    """
    returns the values for a list of files of files belonging to one dataset

    :param resource: netCDF file
    :param variable: variable to be picked from the files (if not set, variable will be detected)
    :param time_range: list[start,end] of datetime to define periode to get values

    :returs numpy.array: values
    """
    from numpy import squeeze, where, array
    if variable is None:
        variable = get_variable(resource)

    if isinstance(resource, str):
        ds = Dataset(resource)
    elif len(resource) == 1:
        ds = Dataset(resource)
    else:
        LOGGER.exception('resource is a list containing {} files. Should be only one'.format(len(resource)))
    vals = squeeze(ds.variables[variable][:])

    if time_range is not None:
        ts = array(get_time(resource))
        id_start = where(ts >= time_range[0])[0][0]
        id_end = where(ts <= time_range[1])[0][-1]
        vals = vals[id_start:id_end+1, :, :]
    return vals


# def rename_variable(resource, oldname=None, newname='newname'):
#     """
#     Change the variable name of a netCDF variable
#
#     :param resource: path to netCDF input file
#     :param oldname: variable name to be changed
#     :param newname: variable name to be given
#
#     :retunrs str: path to resource
#     """
#     try:
#         if oldname is None:
#             oldname = get_variable(resource)
#         if oldname != newname:
#             from netCDF4 import Dataset
#             ds = Dataset(resource, mode='a')
#             ds.renameVariable(oldname, newname)
#             ds.close()
#             LOGGER.info('varname %s in netCDF renamed to %s' % (oldname, newname))
#     except Exception as e:
#         msg = 'failed to rename variable in target files %s ' % e
#         LOGGER.debug(msg)
#         raise Exception(msg)
#
#
# #
# def unrotate_pole(resource, write_to_file=False):
#     """
#     Calculates the unrotatated coordinates for a rotated pole grid
#
#     :param resource: netCDF file or list of files of one datatset
#     :param write_to_file: calculated values will be written to file if True (default=False)
#
#     :return list: lats, lons
#     """
#     from numpy import reshape, repeat
#     from iris.analysis import cartography as ct
#
#     if len(resource) == 1:
#         ds = Dataset(resource[0])
#     else:
#         ds = MFDataset(resource)
#
#     # ds = MFDataset(resource)
#
#     if 'lat' in ds.variables.keys():
#         LOGGER.info('file include unrotated coordinate values')
#         lats = ds.variables['lat'][:]
#         lons = ds.variables['lon'][:]
#     else:
#         try:
#             if 'rotated_latitude_longitude' in ds.variables:
#                 rp = ds.variables['rotated_latitude_longitude']
#             elif 'rotated_pole' in ds.variables:
#                 rp = ds.variables['rotated_pole']
#             else:
#                 LOGGER.debug('rotated pole variable not found')
#             pole_lat = rp.grid_north_pole_latitude
#             pole_lon = rp.grid_north_pole_longitude
#         except:
#             LOGGER.exception('failed to find rotated_pole coordinates')
#         try:
#             if 'rlat' in ds.variables:
#                 rlats = ds.variables['rlat']
#                 rlons = ds.variables['rlon']
#
#             if 'x' in ds.variables:
#                 rlats = ds.variables['y']
#                 rlons = ds.variables['x']
#         except:
#             LOGGER.exception('failed to read in rotated coordiates')
#
#         try:
#             rlons_i = reshape(rlons, (1, len(rlons)))
#             rlats_i = reshape(rlats, (len(rlats), 1))
#             grid_rlats = repeat(rlats_i, (len(rlons)), axis=1)
#             grid_rlons = repeat(rlons_i, (len(rlats)), axis=0)
#         except:
#             LOGGER.execption('failed to repeat coordinates')
#
#         lons, lats = ct.unrotate_pole(grid_rlons, grid_rlats, pole_lon, pole_lat)
#
#     if write_to_file is True:
#         lat = ds.createVariable('lat', 'f8', ('rlat', 'rlon'))
#         lon = ds.createVariable('lon', 'f8', ('rlat', 'rlon'))
#
#         lon.standard_name = "longitude"
#         lon.long_name = "longitude coordinate"
#         lon.units = 'degrees_east'
#         lat.standard_name = "latitude"
#         lat.long_name = "latitude coordinate"
#         lat.units = 'degrees_north'
#
#         lat[:] = lats
#         lon[:] = lons
#
#     ds.close()
#
#     return lats, lons


def drs_filename(resource, skip_timestamp=False, skip_format=False,
                 variable=None, rename_file=False, add_file_path=False):
    """
    generates filename according to the data reference syntax (DRS)
    based on the metadata in the resource.
    http://cmip-pcmdi.llnl.gov/cmip5/docs/cmip5_data_reference_syntax.pdf
    https://pypi.python.org/pypi/drslib
    :param add_file_path: if add_file_path=True, path to file will be added (default=False)
    :param resource: netcdf file
    :param skip_timestamp: if True then from/to timestamp != added to the filename
                           (default: False)
    :param variable: appropriate variable for filename, if not set (default), variable will
                      be determined. For files with more than one data variable,
                      the variable parameter has to be defined (default: )
                      example: variable='tas'
    :param rename_file: rename the file. (default: False)
    :returns str: DRS filename
    """
    try:
        ds = Dataset(resource)
        if variable is None:
            variable = get_variable(resource)
        # CORDEX example: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
        cordex_pattern = "{variable}_{domain}_{driving_model}_{experiment}_{ensemble}_{model}_{version}_{frequency}"
        # CMIP5 example: tas_MPI-ESM-LR_historical_r1i1p1
        cmip5_pattern = "{variable}_{model}_{experiment}_{ensemble}"
        filename = resource
        if ds.project_id == 'CORDEX' or ds.project_id == 'EOBS':
            filename = cordex_pattern.format(
                variable=variable,
                domain=ds.CORDEX_domain,
                driving_model=ds.driving_model_id,
                experiment=ds.experiment_id,
                ensemble=ds.driving_model_ensemble_member,
                model=ds.model_id,
                version=ds.rcm_version_id,
                frequency=ds.frequency)
        elif ds.project_id == 'CMIP5':
            # TODO: attributes missing in netcdf file for name generation?
            filename = cmip5_pattern.format(
                variable=variable,
                model=ds.model_id,
                experiment=ds.experiment,
                ensemble=ds.parent_experiment_rip
            )
        else:
            raise Exception('unknown project %s' % ds.project_id)
        ds.close()
    except Exception:
        LOGGER.exception('Could not read metadata %s', resource)
    try:
        # add from/to timestamp if not skipped
        if skip_timestamp is False:
            LOGGER.debug("add timestamp")
            from_timestamp, to_timestamp = get_timerange(resource)
            LOGGER.debug("from_timestamp %s", from_timestamp)
            filename = "%s_%s-%s" % (filename, int(from_timestamp), int(to_timestamp))

        # add format extension
        if skip_format is False:
            filename = filename + '.nc'

        pf = path.dirname(resource)
        # add file path
        if add_file_path is True:
            filename = path.join(pf, filename)

        # rename the file
        if rename_file is True:
            if path.exists(path.join(resource)):
                rename(resource, path.join(pf, filename))
    except Exception:
        LOGGER.exception('Could not generate DRS filename for %s', resource)

    return filename


def sort_by_time(resource):
    """
    Sort a list of files by their time variable.

    :param resource: File path.
    :return: Sorted file list.
    """
    from ocgis.util.helpers import get_sorted_uris_by_time_dimension

    if type(resource) == list and len(resource) > 1:
        sorted_list = get_sorted_uris_by_time_dimension(resource)
    elif type(resource) == str:
        sorted_list = [resource]
    else:
        sorted_list = resource
    return sorted_list
