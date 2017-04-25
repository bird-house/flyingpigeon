from flyingpigeon import utils

# import logging
# logger = logging.getLogger(__name__)

import logging
logger = logging.getLogger("PYWPS")



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


def reanalyses(start=1948, end=None, variable='slp', dataset='NCEP'):
    """
    Fetches the reanalysis data (NCEP, 20CR or ERA_20C) to local file system
    :param start: int for start year to fetch source data
    :param end: int for end year to fetch source data (if None, current year will be the end)
    :param variable: variable name (default='slp'), geopotential height is given as e.g. z700
    :param dataset: default='NCEP'
    :return list: list of path/files.nc
    """
    # used for NETCDF convertion
    from os import path
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
        logger.info('start / end date set')
    except:
        msg = "get_OBS module failed to get start end dates"
        logger.exception(msg)
        raise Exception(msg)

    if 'z' in variable:
        level = variable.strip('z')
    else:
        level = None

    logger.info('level: %s' % level)

    try:
        for year in range(start, end + 1):
            logger.debug('fetching single file for %s year %s ' % (dataset, year))
            try:
                if dataset == 'NCEP':
                    if variable == 'slp':
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/%s.%s.nc' % (variable, year)
                    if 'z' in variable:
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/pressure/hgt.%s.nc' % (year)
                elif dataset == '20CRV2':
                    if variable == 'prmsl':
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2/monolevel/prmsl.%s.nc' % year
                    if 'z' in variable:
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2/pressure/hgt.%s.nc' % (year)
                elif dataset == '20CRV2c':
                    if variable == 'prmsl':
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/monolevel/prmsl.%s.nc' % year
                    if 'z' in variable:
                        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/20thC_ReanV2c/pressure/hgt.%s.nc' % (year)
                else:
                    logger.debug('Dataset %s not known' % dataset)
                logger.debug('url: %s' % url)
            except:
                msg = "could not set url"
                logger.exception(msg)
            try:
                df = utils.download(url, cache=True)
                logger.debug('single file fetched %s ' % year)
                # convert to NETCDF4_CLASSIC
                try:
                    p, f = path.split(path.abspath(df))
                    logger.debug("path = %s , file %s " % (p, f))
                    move(df, f)
                    conv = call(resource=f,
                                output_format_options={'data_model': 'NETCDF4_CLASSIC'},
                                dir_output=p,
                                prefix=f.replace('.nc', ''))
                    obs_data.append(conv)
                    logger.debug('file %s to NETCDF4_CLASSIC converted' % conv)
                except:
                    logger.exception('failed to convert into NETCDF4_CLASSIC')
            except:
                msg = "wget failed on {0}.".format(url)
                logger.exception(msg)
        logger.info('Reanalyses data fetched for %s files' % len(obs_data))
    except:
        msg = "get reanalyses module failed to fetch data"
        logger.exception(msg)
        raise Exception(msg)

    if level is None:
        data = obs_data
    else:
        logger.info('get level: %s' % level)
        data = get_level(obs_data, level=level)
    return data


def get_level(resource, level):
    from flyingpigeon.ocgis_module import call
    from netCDF4 import Dataset
    from flyingpigeon.utils import get_variable
    from numpy import squeeze

    try:
        level_data = call(resource, level_range=[int(level), int(level)])
        if type(resource) == list:
            resource.sort()
        variable = get_variable(level_data)
        logger.info('found %s in file' % variable)
        ds = Dataset(level_data, mode='a')
        var = ds.variables.pop(variable)
        dims = var.dimensions
        new_var = ds.createVariable('z%s' % level, var.dtype, dimensions=(dims[0], dims[2], dims[3]))
        # i = where(var[:]==level)
        new_var[:, :, :] = squeeze(var[:, 0, :, :])
        ds.close()
        logger.info('level %s extracted' % level)
        data = call(level_data, variable='z%s' % level)
    except:
        logger.exception('failed to extract level')

    return data
