from flyingpigeon import utils
from datetime import datetime as dt
from datetime import timedelta

# import logging
# logger = logging.getLogger(__name__)

import logging
LOGGER = logging.getLogger("PYWPS")

_PRESSUREDATA_ = [
    'NCEP_slp', 'NCEP_z1000', 'NCEP_z925', 'NCEP_z850', 'NCEP_z700', 'NCEP_z600', 'NCEP_z500', 'NCEP_z400', 'NCEP_z300',
    'NCEP_z250', 'NCEP_z200', 'NCEP_z150', 'NCEP_z100', 'NCEP_z70', 'NCEP_z50', 'NCEP_z30', 'NCEP_z20', 'NCEP_z10',
    '20CRV2_prmsl',
    '20CRV2_z1000', '20CRV2_z950', '20CRV2_z900', '20CRV2_z850', '20CRV2_z800', '20CRV2_z750', '20CRV2_z700',
    '20CRV2_z650', '20CRV2_z600', '20CRV2_z550', '20CRV2_z500', '20CRV2_z450', '20CRV2_z400', '20CRV2_z350',
    '20CRV2_z300', '20CRV2_z250', '20CRV2_z200', '20CRV2_z150', '20CRV2_z100', '20CRV2_z70', '20CRV2_z50',
    '20CRV2_z30', '20CRV2_z20', '20CRV2_z10',
    '20CRV2c_prmsl',
    '20CRV2c_z1000', '20CRV2c_z950', '20CRV2c_z900', '20CRV2c_z850', '20CRV2c_z800', '20CRV2c_z750', '20CRV2c_z700',
    '20CRV2c_z650', '20CRV2c_z600', '20CRV2c_z550', '20CRV2c_z500', '20CRV2c_z450', '20CRV2c_z400', '20CRV2c_z350',
    '20CRV2c_z300', '20CRV2c_z250', '20CRV2c_z200', '20CRV2c_z150', '20CRV2c_z100', '20CRV2c_z70', '20CRV2c_z50',
    '20CRV2c_z30', '20CRV2c_z20', '20CRV2c_z10',
]

_EOBSVARIABLES_ = ['tg', 'tx', 'tn', 'rr']

_EODATA_ = ["PSScene3Band__visual",
            "PSScene4Band__analytic",
            "PSScene4Band__analytic_xml",
            "Sentinel2L1C__metadata_aux",
            "Sentinel2L1C__analytic_b1",
            "Sentinel2L1C__analytic_b2",  # blue
            "Sentinel2L1C__analytic_b3",  # green
            "Sentinel2L1C__analytic_b4",  # red
            "Sentinel2L1C__analytic_b8",  # nivr
            ]

# PSScene3Band	PlanetScope Scenes

# PSScene4Band	PlanetScope Scenes
# PSOrthoTile	PlanetScope OrthoTiles
# REOrthoTile	RapidEye OrthoTiles
# REScene	RapidEye Scenes (unorthorectified strips)
# SkySatScene	SkySat Scenes
# Landsat8L1G	Landsat8 Scenes
# Sentinel2L1C	Copernicus Sentinel-2 Scenes

#  "_permissions": [
#   "assets.analytic_b1:download",
#   "assets.analytic_b3:download",
#   "assets.analytic_b2:download",
#   "assets.analytic_b5:download",
#   "assets.analytic_b4:download",
#   "assets.analytic_b7:download",
#   "assets.analytic_b6:download",
#   "assets.analytic_b9:download",
#   "assets.analytic_b8:download",
#   "assets.analytic_b8a:download",
#   "assets.visual:download",
#   "assets.metadata_aux:download",
#   "assets.analytic_b10:download",
#   "assets.analytic_b11:download",
#   "assets.analytic_b12:download"
#  ],
#


def reanalyses(start=1948, end=None, variable='slp', dataset='NCEP', timres='day', getlevel=True):
    """
    Fetches the reanalysis data (NCEP, 20CR or ERA_20C) to local file system
    :param start: int for start year to fetch source data
    :param end: int for end year to fetch source data (if None, current year will be the end)
    :param variable: variable name (default='slp'), geopotential height is given as e.g. z700
    :param dataset: default='NCEP'
    :return list: list of path/files.nc
    """
    # used for NETCDF convertion
    from netCDF4 import Dataset
    from os import path, system
    from flyingpigeon.ocgis_module import call
    from shutil import move
    # used for NETCDF convertion

    try:
        from datetime import datetime as dt

        if end is None:
            end = dt.now().year
        obs_data = []

        if start is None:
            if dataset == 'NCEP':
                start = 1948
            if dataset == '20CR':
                start = 1851
        LOGGER.info('start / end date set')
    except:
        msg = "get_OBS module failed to get start end dates"
        LOGGER.exception(msg)
        raise Exception(msg)

    if 'z' in variable:
        level = variable.strip('z')
    else:
        level = None

    LOGGER.info('level: %s' % level)

    try:
        for year in range(start, end + 1):
            LOGGER.debug('fetching single file for %s year %s ' % (dataset, year))
            try:
                if dataset == 'NCEP':
                    if variable == 'slp':
                        url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/%s.%s.nc' % (variable, year)  # noqa
                    if 'z' in variable:
                        url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.%s.nc' % (year)  # noqa
                elif dataset == '20CRV2':
                    if variable == 'prmsl':
                        url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2/monolevel/prmsl.%s.nc' % year  # noqa
                    if 'z' in variable:
                        url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2/pressure/hgt.%s.nc' % (year)  # noqa
                elif dataset == '20CRV2c':
                    if variable == 'prmsl':
                        if timres == '6h':
                            url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/monolevel/prmsl.%s.nc' % year  # noqa
                        else:
                            url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/Dailies/monolevel/prmsl.%s.nc' % year  # noqa
                    if 'z' in variable:
                        if timres == '6h':
                            url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/pressure/hgt.%s.nc' % (year)  # noqa
                        else:
                            url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/Dailies/pressure/hgt.%s.nc' % (year)  # noqa
                else:
                    LOGGER.debug('Dataset %s not known' % dataset)
                LOGGER.debug('url: %s' % url)
            except:
                msg = "could not set url"
                LOGGER.exception(msg)
            try:
                df = utils.download(url, cache=True)
                LOGGER.debug('single file fetched %s ' % year)
                # convert to NETCDF4_CLASSIC
                try:
                    ds = Dataset(df)
                    df_time = ds.variables['time']
                    # Here, need to check not just calendar, but that file is ncdf_classic already...
                    if (hasattr(df_time, 'calendar')) is False:
                        p, f = path.split(path.abspath(df))
                        LOGGER.debug("path = %s , file %s " % (p, f))
                        # May be an issue if several users are working at the same time
                        move(df, f)
                        conv = call(resource=f,
                                    output_format_options={'data_model': 'NETCDF4_CLASSIC'},
                                    dir_output=p,
                                    prefix=f.replace('.nc', ''))
                        obs_data.append(conv)
                        LOGGER.debug('file %s to NETCDF4_CLASSIC converted' % conv)
                        # Cleaning, could be 50gb... for each (!) user
                        # TODO Check how links work
                        cmdrm = 'rm -f %s' % (f)
                        system(cmdrm)
                    else:
                        obs_data.append(df)
                    ds.close()
                except:
                    LOGGER.exception('failed to convert into NETCDF4_CLASSIC')
            except:
                msg = "download failed on {0}.".format(url)
                LOGGER.exception(msg)
        LOGGER.info('Reanalyses data fetched for %s files' % len(obs_data))
    except:
        msg = "get reanalyses module failed to fetch data"
        LOGGER.exception(msg)
        raise Exception(msg)

    if (level is None) or (getlevel==False):
        data = obs_data
    else:
        LOGGER.info('get level: %s' % level)
        data = get_level(obs_data, level=level)
    return data


def get_level(resource, level):
    from flyingpigeon.ocgis_module import call
    from netCDF4 import Dataset
    from flyingpigeon.utils import get_variable
    from numpy import squeeze
    from os import path

    try:
        if type(resource) == list:
            resource = sorted(resource, key=lambda i: path.splitext(path.basename(i))[0])
            # resource.sort()

        level_data = call(resource, level_range=[int(level), int(level)])
        variable = get_variable(level_data)
        LOGGER.info('found %s in file' % variable)
        ds = Dataset(level_data, mode='a')
        var = ds.variables.pop(variable)
        dims = var.dimensions
        new_var = ds.createVariable('z%s' % level, var.dtype, dimensions=(dims[0], dims[2], dims[3]))
        # i = where(var[:]==level)
        new_var[:, :, :] = squeeze(var[:, 0, :, :])

        # TODO: Here may be an error! in case of exception, dataset will not close!
        # Exception arise for example for 20CRV2 data...
        try:
            new_var.setncatts({k: var.getncattr(k) for k in var.ncattrs()})
        except:
            LOGGER.info('Could not set attributes for z%s' % level)
        ds.close()
        LOGGER.info('level %s extracted' % level)
        data = call(level_data, variable='z%s' % level)
    except:
        LOGGER.exception('failed to extract level')

    return data


def write_fileinfo(resource, filepath=False):
    """
    write path and filenames to a text file

    :param ressource: list of files to be documented
    :param filepath: if True the absolute filepath is written out as well (default = False)

    :return txt: textfile with appropriate information"""

    from os.path import basename, realpath
    from tempfile import mkstemp
    _, text_src = mkstemp(dir='.', suffix='.txt')

    try:
        with open(text_src, 'w') as fp:
            fp.write('###############################################\n')
            fp.write('#######   birdhouse process              ######\n')
            fp.write('###############################################\n')
            if filepath is False:
                fp.write('Following is a list of resource files:         \n')
                fp.write('\n')
                for f in resource:
                    fp.write('%s \n' % basename(f))
            else:
                fp.write('Following files are stored to your local discs: \n')
                fp.write('\n')
                for f in resource:
                    fp.write('%s \n' % realpath(f))
        LOGGER.info('resources filenames written to textfile')
    except:
        LOGGER.exception('failed to write file names to file')

    return text_src


def fetch_eodata(item_type, asset, token, bbox, period=[dt.today()-timedelta(days=30), dt.today()],  cloud_cover=0.5, cache=True):
    """
    search for given EO data product provided by planet.
    The search and appropriate download is limited by bbox and search period

    :param item_type: product provided by planet
    :param asset: product asset, (visible, analytic, bands)
    :param token: Authentification token generated by planet Earth Obersavation Explorer
    :param bbox: latitude longitude coordinates defining a bounding box
    :param period: [start , end] datetime objects (default last 30 days)
    :param cloud_cover: threshold for cloud_cover tolerance. 0 = 0percent cloud_cover 1=100percent cloud_cover
    :param cache: if True file (default) is stored in local cache

    return list: list of pathes for fetched products
    """

    import os
    import json
    import requests
    from requests.auth import HTTPBasicAuth
    from tempfile import mkstemp
    import shutil
    import time
    from os.path import join
    from os import path, makedirs
    from flyingpigeon.config import cache_path

    geojson_geometry = {"type": "Polygon",
                        "coordinates": [[
                                    [14.600830078125, 8.677421123289992],
                                    [14.797210693359375, 8.677421123289992],
                                    [14.797210693359375, 8.90678000752024],
                                    [14.600830078125, 8.90678000752024],
                                    [14.600830078125, 8.677421123289992]
                                    ]]}

    # get images that overlap with our AOI
    geometry_filter = {
      "type": "GeometryFilter",
      "field_name": "geometry",
      "config": geojson_geometry
    }

    start = period[0]
    end = period[1]


    LOGGER.debug("Period %s to %s " % (start, end))

    # get images acquired within a date range
    date_range_filter = {
      "type": "DateRangeFilter",
      "field_name": "acquired",
      "config": {
        "gte": "%s000Z" % (start.strftime('%Y-%m-%dT%H:%M:%S.')),
        "lte": "%s000Z" % (end.strftime('%Y-%m-%dT%H:%M:%S.')),
      }
    }

    # only get images which have <50% cloud coverage
    cloud_cover_filter = {
      "type": "RangeFilter",
      "field_name": "cloud_cover",
      "config": {
        "lte": cloud_cover
      }
    }

    # combine our geo, date, cloud filters
    combined_filter = {"type": "AndFilter",
                       "config": [geometry_filter, date_range_filter, cloud_cover_filter]}

   # API Key
    PLANET_API_KEY = token  # os.getenv('PL_API_KEY')

    # item_type = item_type, assetproducts[0]  # "PSScene4Band"
    # API request object

    search_request = {
      "interval": "day",
      "item_types": [item_type],
      "filter": combined_filter
    }

    if cache:
        DIR_archiv = cache_path()
    else:
        DIR_archiv = '.'
    DIR = join(DIR_archiv, "EO_data", item_type, asset)

    if not os.path.exists(DIR):
        makedirs(DIR)

    # fire off the POST request
    search_result = requests.post(
        'https://api.planet.com/data/v1/quick-search',
        auth=HTTPBasicAuth(PLANET_API_KEY, ''),
        json=search_request)

    LOGGER.info('Search result: %s ' % json.dumps(search_result.json(), indent=1))

    # extract image IDs only
    image_ids = [feature['id'] for feature in search_result.json()['features']]
    LOGGER.info("image IDs:  %s " % image_ids)

    resources = []

    for image_id in image_ids:

        id0 = image_id

        filename = "%s.tif" % id0
        local_file = join(DIR, filename)  # mkstemp(dir="/home/nils/data/planet/", prefix=id0, suffix='.tif')

        if os.path.exists(local_file):
            LOGGER.info('File %s in cache' % filename)
            resources.extend([local_file])
        else:
            id0_url = 'https://api.planet.com/data/v1/item-types/{}/items/{}/assets'.format(item_type, id0)

            # Returns JSON metadata for assets in this ID. Learn more: planet.com/docs/reference/data-api/items-assets/#asset
            result = requests.get(id0_url, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
            # List of asset types available for this particular satellite image
            LOGGER.debug(result.json().keys())
            # This is "inactive" if the "visual" asset has not yet been activated; otherwise 'active'
            #  if 'analytic' in result.json().keys():
            LOGGER.debug("****** down loading file ********")
            LOGGER.debug(result.json()[asset]['status'])
            # Parse out useful links
            links = result.json()[asset]["_links"]  # u"analytic"
            self_link = links["_self"]
            activation_link = links["activate"]

            # Request activation of the 'visual' asset:
            activate_result = requests.get(activation_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
            # Parse out useful links
            links = result.json()[asset]["_links"]  # u"analytic"
            self_link = links["_self"]
            activation_link = links["activate"]

            # Request activation of the 'visual' asset:
            activate_result = requests.get(activation_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
            activation_status_result = requests.get(self_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))

            while activation_status_result.json()["status"] != 'active':
                LOGGER.debug('*** File is sleeping. gently waking up ****')
                LOGGER.debug(activation_status_result.json()["status"])
                time.sleep(30)
                activation_status_result = requests.get(self_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))

            LOGGER.debug('File ready to download: %s' % (activation_status_result.json()["status"]))
            # Image can be downloaded by making a GET with your Planet API key, from here:
            download_link = activation_status_result.json()["location"]

            r = requests.get(download_link, stream=True, verify=False)
            with open(local_file, 'wb') as fp:
                shutil.copyfileobj(r.raw, fp)
                resources.extend([local_file])

    return resources
