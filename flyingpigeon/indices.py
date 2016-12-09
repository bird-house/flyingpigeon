import os

from flyingpigeon.utils import calc_grouping, sort_by_filename, get_variable  # aggregations,
from flyingpigeon.subset import get_ugid, get_geom
from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

_INDICES_ = dict(
    TG=dict(variable='tas', description='Mean of mean temperature (tas as input files)'),
    TX=dict(variable='tasmax', description='Mean of max temperature (tasmax as input files)'),
    TN=dict(variable='tasmin', description='Mean of daily min temperature (tasmin as input files)'),
    TXn=dict(variable='tasmax', description='Min of daily min temperature (tasmax as input files)'),
    TXx=dict(variable='tasmax', description='Max of daily max temperature (tasmax as input files)'),
    TNn=dict(variable='tasmin', description='Min of daily min temperature (tasmin as input files)'),
    TNx=dict(variable='tasmin', description='Max of daily min temperature (tasmin as input files)'),
    SU=dict(variable='tasmax', description='Nr of summer days (tasmax as input files)'),
    CSU=dict(variable='tasmax', description='Nr of consecutive summer days (tasmax as input files)'),
    FD=dict(variable='tasmin', description='Nr of frost days (tasmin as input files)'),
    CFD=dict(variable='tasmin', description='Nr of consecutive frost days (tasmin as input files)'),
    TR=dict(variable='tasmin', description='Tropical nights - number of days where daily minimum temperature >= 20 \
                                            degrees.(tasmin as input files)'),
    ID=dict(variable='tasmax', description='Nr of ice days (tasmax as input files)'),
    HD17=dict(variable='tas', description='Heating degree days [sum of 17 degrees - mean temperature] (tas as input)'),
    GD4=dict(variable='tas', description='Growing degree days [sum of TG >= 4 degrees] (tas as input files)'),
    # RR=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
    PRCPTOT=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
    RR1=dict(variable='pr', description='Nr of days with precipitation > 1 mm  (pr as input files)'),
    CWD=dict(variable='pr', description='Consecutive wet days (pr as input files)'),
    CDD=dict(variable='pr', description='Consecutive dry days (pr as input files)'),
    SDII=dict(variable='pr', description='Simple daily intensity index for wet days [mm/wet day] (pr as input files)'),
    R10mm=dict(variable='pr', description='Nr of days > 10mm (pr as input files)'),
    R20mm=dict(variable='pr', description='Nr of days with precipitation >= 20 mm (pr as input files)'),
    RX1day=dict(variable='pr', description='Highest 1-day precipitation amount (pr as input files)'),
    RX5day=dict(variable='pr', description='Highest 5-day precipitation amount (pr as input files)'),
    SD=dict(variable='prsn', description='Nr of snow days (prsn as input files)'),
    SD1=dict(variable='prsn', description='Nr of days with snow >= 1cm  (prsn as input files)'),
    SD5cm=dict(variable='prsn', description='Nr of days with snow >= 5cm (prsn as input files)'),
    SD50cm=dict(variable='prsn', description='Nr of days with snow >= 50 cm (prsn as input files)'),
    )

_INDICESper_ = dict(
    TG10p=dict(variable='tas',
               description='Days with TG < 10th percentile of daily mean temperature (cold days) (days)'),
    TX10p=dict(variable='tasmax',
               description='Days with TX < 10th percentile of daily maximum temperature (cold day-times) (days)'),
    TN10p=dict(variable='tasmin',
               description='Days with TN < 10th percentile of daily minimum temperature (cold nights) (days)'),
    TG90p=dict(variable='tas',
               description='Days with TG > 90th percentile of daily mean temperature (warm days) (days)'),
    TX90p=dict(variable='tasmax',
               description='Days with TX > 90th percentile of daily maximum temperature (warm day-times) (days)'),
    TN90p=dict(variable='tasmin',
               description='Days with TN > 90th percentile of daily minimum temperature (warm nights) (days)'),
    WSDI=dict(variable='tasmax',
              description='Warm-spell duration index (days)'),
    CSDI=dict(variable='tasmin',
              description='Cold-spell duration index (days)'),
    R75p=dict(variable='pr',
              description='Days with PRCPTOT > 75th percentile of daily amounts (moderate wet days) (days)'),
    R75pTOT=dict(variable='pr',
                 description='Precipitation fraction due to moderate wet days (>75th percentile) (%)'),
    R95p=dict(variable='pr',
              description='Days with PRCPTOT > 95th percentile of daily amounts (very wet days) (days)'),
    R95pTOT=dict(variable='pr',
                 description='Precipitation fraction due to very wet days (>95th percentile) (%)'),
    R99p=dict(variable='pr',
              description='Days with PRCPTOT > 99th percentile of daily amounts (extremely wet days)(days)'),
    R99pTOT=dict(variable='pr',
                 description='recipitation fraction due to extremely wet days (>99th percentile)(%)'),
    )

_INDICEScomp_ = dict(
    CD=dict(variable=['tas', 'pr'],
            description='Days with TG < 25th percentile of daily mean temperature and\
             PRCPTOT < 25th percentile of daily precipitation sum (cold/dry days)'),
    CW=dict(variable=['tas', 'pr'],
            description='Days with TG < 25th percentile of daily mean temperature and\
             PRCPTOT > 75th percentile of daily precipitation sum (cold/wet days)'),
    WD=dict(variable=['tas', 'pr'],
            description='days with TG > 75th percentile of daily mean temperature and\
             PRCPTOT < 25th percentile of daily precipitation sum (warm/dry days)'),
    WW=dict(variable=['tas', 'pr'],
            description='Days with TG > 75th percentile of daily mean temperature and\
             PRCPTOT > 75th percentile of daily precipitation sum (warm/wet days)'),
    )

_INDICESunconventional_ = dict(
    TGx=dict(variable=['tas'], description='Max of daily mean temperature'),
    TGx5day=dict(variable=['tas'], description='max of 5-day running average of daily mean temperature'),
    TGn=dict(variable=['tas'], description='Min of daily mean temperature'),
    TGn5day=dict(variable=['tas'], description='Min of 5-day running average of daily mean temperature'),
    )


def indices():
    """
    :return: a list of all climate indices.
    """
    indices = _INDICES_.keys()
    indices.sort()
    return indices


def indices_description():
    """
    :return: a description of all climate indices.
    """
    description = ''
    for indice in indices():
        description = description + "%s: %s\n" % (indice, _INDICES_[indice]['description'])
    return description


def indice_description(indice):
    """
    :return: a description of given climate indices.
    """
    desc = None
    try:
        desc = _INDICES_[indice]['description']
    except:
        logger.error('unknown indice %s', indice)
    return desc


def indice_variable(indice):
    """
    :return: variable (tasmax, tas, ...) which can be used for the climate indice.
    """
    variable = None
    try:
        variable = _INDICES_[indice]['variable']
    except:
        logger.error('unknown indice %s', indice)
    return variable


def calc_indice_simple(resource=[], variable=None, prefix=None, indices=None,
                       polygons=None, mosaic=False, groupings='yr', dir_output=None,
                       dimension_map=None, memory_limit=None):
    """
    Calculates given simple indices for suitable files in the appropriate time grouping and polygon.

    :param resource: list of filenames in data reference syntax (DRS) convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param indices: list of indices (default ='SU')
    :param polygons: list of polgons (default ='FRA')
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir.
    """
    from os.path import join, dirname, exists
    from flyingpigeon import ocgis_module
    from flyingpigeon.subset import clipping
    import uuid

    if type(resource) != list:
        resource = list([resource])
    if type(indices) != list:
        indices = list([indices])
    if type(polygons) != list and polygons is not None:
        polygons = list([polygons])
    if type(groupings) != list:
        groupings = list([groupings])

    if dir_output is not None:
        if not exists(dir_output):
            makedirs(dir_output)

    # from flyingpigeon.subset import select_ugid
    #    tile_dim = 25
    output = None

    experiments = sort_by_filename(resource)
    outputs = []

    for key in experiments:
        if variable is None:
            variable = get_variable(experiments[key][0])
        # variable = key.split('_')[0]
        try:
            # if variable == 'pr':
            #     calc = 'pr=pr*86400'
            #     ncs = ocgis_module.call(resource=experiments[key],
            #                             variable=variable,
            #                             dimension_map=dimension_map,
            #                             calc=calc,
            #                             memory_limit=memory_limit,
            #                             # calc_grouping= calc_group,
            #                             prefix=str(uuid.uuid4()),
            #                             dir_output=dir_output,
            #                             output_format='nc')
            # else:
            #     ncs = experiments[key]
            for indice in indices:
                logger.info('indice: %s' % indice)
                try:
                    calc = [{'func': 'icclim_' + indice, 'name': indice}]
                    logger.info('calc: %s' % calc)
                    for grouping in groupings:
                        logger.info('grouping: %s' % grouping)
                        try:
                            calc_group = calc_grouping(grouping)
                            logger.info('calc_group: %s' % calc_group)
                            if polygons is None:
                                try:
                                    prefix = key.replace(variable, indice).replace('_day_', '_%s_' % grouping)
                                    tmp = ocgis_module.call(resource=ncs,
                                                            variable=variable,
                                                            dimension_map=dimension_map,
                                                            calc=calc,
                                                            calc_grouping=calc_group,
                                                            prefix=prefix,
                                                            dir_output=dir_output,
                                                            output_format='nc')
                                    outputs.append(tmp)
                                except Exception as e:
                                    msg = 'could not calc indice %s for domain in %s' % (indice, key)
                                    logger.debug(msg)
                                    # raise Exception(msg)
                            else:
                                try:
                                    prefix = key.replace(variable, indice).replace('_day_', '_%s_' % grouping)
                                    tmp = clipping(resource=ncs,
                                                   variable=variable,
                                                   dimension_map=dimension_map,
                                                   calc=calc,
                                                   calc_grouping=calc_group,
                                                   prefix=prefix,
                                                   polygons=polygons,
                                                   mosaic=mosaic,
                                                   dir_output=dir_output,
                                                   output_format='nc')
                                    outputs.append(tmp)
                                except Exception as e:
                                    msg = 'could not calc indice %s for domain in %s' % (indice, key)
                                    logger.debug(msg)
                                    # raise Exception(msg)
                            logger.info('indice file calculated: %s' % tmp)
                        except Exception as e:
                            msg = 'could not calc indice %s for key %s and grouping %s' % (indice, key, grouping)
                            logger.debug(msg)
                            # raise Exception(msg)
                except Exception as e:
                    msg = 'could not calc indice %s for key %s' % (indice, key)
                    logger.debug(msg)
                    # raise Exception(msg)
        except Exception as e:
            msg = 'could not calc key %s' % key
            logger.debug(msg)
            # raise Exception(msg)
    logger.info('indice outputs %s ' % outputs)
    return outputs


def calc_indice_percentile(resources=[], variable=None,
                           prefix=None, indices='TG90p', refperiod=None,
                           groupings='yr', polygons=None, percentile=90, mosaic=False,
                           dir_output=None, dimension_map=None):
    """
    Calculates given indices for suitable files in the appropriate time grouping and polygon.

    :param resource: list of filenames in data reference syntax (DRS) convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param indices: list of indices (default ='TG90p')
    :param prefix: filename prefix
    :param refperiod: reference period tuple = (start,end)
    :param grouping: indices time aggregation (default='yr')
    :param dir_output: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir.
    """
    from os.path import join, dirname, exists
    from os import remove
    import uuid
    from numpy import ma
    from datetime import datetime as dt

    from flyingpigeon.ocgis_module import call
    from flyingpigeon.subset import clipping
    from flyingpigeon.utils import get_values, get_time

    if type(resources) != list:
        resources = list([resources])
    if type(indices) != list:
        indices = list([indices])

    if type(groupings) != list:
        groupings = list([groupings])

    if type(refperiod) == list:
        refperiod = refperiod[0]

    if refperiod is not None:
        start = dt.strptime(refperiod.split('-')[0], '%Y%m%d')
        end = dt.strptime(refperiod.split('-')[1], '%Y%m%d')
        time_range = [start, end]
    else:
        time_range = None

    if dir_output is not None:
        if not exists(dir_output):
            makedirs(dir_output)

    ################################################
    # Compute a custom percentile basis using ICCLIM
    ################################################

    from ocgis.contrib import library_icclim as lic
    nc_indices = []
    nc_dic = sort_by_filename(resources)

    for grouping in groupings:
        calc_group = calc_grouping(grouping)
        for key in nc_dic.keys():
            resource = nc_dic[key]
            if variable is None:
                variable = get_variable(resource)
            if polygons is None:
                nc_reference = call(resource=resource,
                                    prefix=str(uuid.uuid4()),
                                    time_range=time_range,
                                    output_format='nc',
                                    dir_output=dir_output)
            else:
                nc_reference = clipping(resource=resource,
                                        prefix=str(uuid.uuid4()),
                                        time_range=time_range,
                                        output_format='nc',
                                        polygons=polygons,
                                        dir_output=dir_output,
                                        mosaic=mosaic)

            arr = get_values(resource=nc_reference)
            dt_arr = get_time(resource=nc_reference)
            arr = ma.masked_array(arr)
            dt_arr = ma.masked_array(dt_arr)
            percentile = percentile
            window_width = 5

            for indice in indices:
                name = indice.replace('_', str(percentile))
                var = indice.split('_')[0]

                operation = None
                if 'T' in var:
                    if percentile >= 50:
                        operation = 'Icclim%s90p' % var
                        func = 'icclim_%s90p' % var  # icclim_TG90p
                else:
                    operation = 'Icclim%s10p' % var
                    func = 'icclim_%s10p' % var

                ################################
                # load the appropriate operation
                ################################

                ops = [op for op in dir(lic) if operation in op]
                if len(ops) == 0:
                    raise Exception("operator does not exist %s", operation)
                exec "percentile_dict = lic.%s.get_percentile_dict(arr, dt_arr, percentile, window_width)" % ops[0]
                calc = [{'func': func, 'name': name, 'kwds': {'percentile_dict': percentile_dict}}]

                if polygons is None:
                    nc_indices.append(call(resource=resource,
                                           prefix=key.replace(variable, name).replace('_day_', '_%s_' % grouping),
                                           calc=calc,
                                           calc_grouping=calc_group,
                                           output_format='nc',
                                           dir_output=dir_output))
                else:
                    nc_indices.extend(clipping(resource=resource,
                                               prefix=key.replace(variable, name).replace('_day_', '_%s_' % grouping),
                                               calc=calc,
                                               calc_grouping=calc_group,
                                               output_format='nc',
                                               dir_output=dir_output,
                                               polygons=polygons,
                                               mosaic=mosaic,
                                               ))
    return nc_indices
