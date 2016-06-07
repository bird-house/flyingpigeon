import os

from flyingpigeon.utils import calc_grouping, sort_by_filename, get_variable # aggregations, 
from flyingpigeon.subset import get_ugid, get_geom
from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

_INDICES_ = dict(
    TG=dict(variable='tas', description='Mean of mean temperatur (tas as input files)'),
    TX=dict(variable='tasmax', description='Mean of max temperatur (tasmax as input files)'),
    TN=dict(variable='tasmin', description='Mean of daily min temperatur (tasmin as input files)'),
    TXn=dict(variable='tasmax', description='Min of daily min temperatur (tasmax as input files)'),
    TXx=dict(variable='tasmax', description='Max of daily max temperatur (tasmax as input files)'),
    TNn=dict(variable='tasmin', description='Min of daily min temperatur (tasmin as input files)'),
    TNx=dict(variable='tasmin', description='Max of daily min temperatur (tasmin as input files)'),
    SU=dict(variable='tasmax', description='Nr of summer days (tasmax as input files)'),
    CSU=dict(variable='tasmax', description='Nr of consecutive summer days (tasmax as input files)'),
    FD=dict(variable='tasmin', description='Nr of frost days (tasmin as input files)'),
    CFD=dict(variable='tasmin', description='Nr of consecutive frost days (tasmin as input files)'),
    TR=dict(variable='tasmin', description='Tropical nights - number of days where daily minimum temperature >= 20 degrees.(tasmin as input files)'),
    ID=dict(variable='tasmax', description='Nr of Ice days (tasmax as input files)'),
    HD17=dict(variable='tas', description='Heating degree days [sum of 17 degrees - mean temperature] (tas as input files)'),
    GD4=dict(variable='tas', description='Growing degree days [sum of TG >= 4 degrees] (tas as input files)'),
    #RR=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
    PRCPTOT=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
    RR1=dict(variable='pr', description='Nr of days with precipitation > 1 mm  (pr as input files)'),
    CWD=dict(variable='pr', description='Consecutive wet days (pr as input files)'),
    CDD=dict(variable='pr', description='Consecutive dry days (pr as input files)'),
    SDII=dict(variable='pr', description='Simple daily intensity index for wet days [mm/wet day] (pr as input files)'),
    R10mm=dict(variable='pr', description='Nr of days >10mm (pr as input files)'),
    R20mm=dict(variable='pr', description='Nr of days with precipitation >= 20 mm (pr as input files)'),
    RX1day=dict(variable='pr', description='Highest 1-day precipitation amount (pr as input files)'),
    RX5day=dict(variable='pr', description='Highest 5-day precipitation amount (pr as input files)'),
    SD=dict(variable='prsn', description='Nr of snow days (prsn as input files)'),
    SD1=dict(variable='prsn', description='Nr of days with snow >= 1cm  (prsn as input files)'),
    SD5cm=dict(variable='prsn', description='Nr of days with snow >= 5cm (prsn as input files)'),
    SD50cm=dict(variable='prsn', description='Nr of days with snow >= 50 cm (prsn as input files)'),
    )

_INDICESper_ = dict(
    TG10p=dict(variable='tas', description='Days with TG < 10th percentile of daily mean temperature (cold days) (days)'),
    TX10p=dict(variable='tasmax', description='Days with TX < 10th percentile of daily maximum temperature (cold day-times) (days)'),
    TN10p=dict(variable='tasmin', description='Days with TN < 10th percentile of daily minimum temperature (cold nights) (days)'),
    TG90p=dict(variable='tas', description='Days with TG > 90th percentile of daily mean temperature (warm days) (days)'),
    TX90p=dict(variable='tasmax', description='Days with TX > 90th percentile of daily maximum temperature (warm day-times) (days)'),
    TN90p=dict(variable='tasmin', description='Days with TN > 90th percentile of daily minimum temperature (warm nights) (days)'),
    WSDI=dict(variable='tasmax', description='Warm-spell duration index (days)'),
    CSDI=dict(variable='tasmin', description='Cold-spell duration index (days)'),
    R75p=dict(variable='pr', description= 'Days with PRCPTOT > 75th percentile of daily amounts (moderate wet days) (days)'),
    R75pTOT=dict(variable='pr', description= 'Precipitation fraction due to moderate wet days (>75th percentile) (%)'),
    R95p=dict(variable='pr', description= 'Days with PRCPTOT > 95th percentile of daily amounts (very wet days) (days)'),
    R95pTOT=dict(variable='pr', description= 'Precipitation fraction due to very wet days (>95th percentile) (%)'),
    R99p=dict(variable='pr', description= 'Days with PRCPTOT > 99th percentile of daily amounts (extremely wet days)(days)'),
    R99pTOT=dict(variable='pr', description= 'recipitation fraction due to extremely wet days (>99th percentile)(%)'),
    )   

_INDICEScomp_ = dict(
    CD=dict(variable=['tas','pr'], description='Days with TG < 25th percentile of daily mean temperature and PRCPTOT < 25th percentile of daily precipitation sum (cold/dry days)'),
    CW=dict(variable=['tas','pr'], description='Days with TG < 25th percentile of daily mean temperature and PRCPTOT > 75th percentile of daily precipitation sum (cold/wet days)'),
    WD=dict(variable=['tas','pr'], description='days with TG > 75th percentile of daily mean temperature and PRCPTOT < 25th percentile of daily precipitation sum (warm/dry days)'),
    WW=dict(variable=['tas','pr'], description='Days with TG > 75th percentile of daily mean temperature and PRCPTOT > 75th percentile of daily precipitation sum (warm/wet days)'),
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
    :return: a discription of all climate indices.
    """
    description = ''
    for indice in indices():
        description = description + "%s: %s\n" % (indice, _INDICES_[indice]['description'])
    return description

def indice_description(indice):
    """
    :return: a discription of given climate indices.
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

def calc_indice_single(resource=[], variable=None, prefix=None,indices=None,
    polygons=None, groupings=None, dir_output=None, dimension_map = None, memory_limit=None):
    """
    Calculates given indices for suitable files in the appopriate time grouping and polygon.

    :param resource: list of filenames in drs convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param indices: list of indices (default ='SU')
    :param polygons: list of polgons (default ='FRA')
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir
    """
    from os.path import join, dirname, exists
    from flyingpigeon import ocgis_module
    from flyingpigeon.subset import clipping
    import uuid

    #DIR_SHP = config.shapefiles_dir()
    #env.DIR_SHPCABINET = DIR_SHP
    #env.OVERWRITE = True

    if type(resource) != list: 
      resource = list([resource])
    if type(indices) != list: 
      indices = list([indices])
    if type(polygons) != list and polygons != None:
      polygons = list([polygons])
    if type(groupings) != list:
      groupings = list([groupings])
    
    if dir_output != None:
      if not exists(dir_output): 
        makedirs(dir_output)
    
    #from flyingpigeon.subset import select_ugid
    #    tile_dim = 25
    output = None


    experiments = sort_by_filename(resource)
    outputs = []
    
    for key in experiments:
      if variable == None: 
        variable = get_variable(experiments[key][0])
        #variable = key.split('_')[0]
      try: 
        
        if variable == 'pr': 
          calc = 'pr=pr*86400'
          ncs = ocgis_module.call(resource=experiments[key],
                     variable=variable,
                     dimension_map=dimension_map, 
                     calc=calc,
                     memory_limit=memory_limit,
                     #alc_grouping= calc_group, 
                     prefix=str(uuid.uuid4()), 
                     dir_output=dir_output,
                     output_format='nc')

        else:
          ncs = experiments[key]
          
        for indice in indices:
          logger.info('indice: %s' % indice)
          try: 
            calc = [{'func' : 'icclim_' + indice, 'name' : indice}]
            logger.info('calc: %s' % calc)
            for grouping in groupings:
              logger.info('grouping: %s' % grouping)
              try:
                calc_group = calc_grouping(grouping)
                logger.info('calc_group: %s' % calc_group)
                if polygons == None:
                  try:
                    if prefix == None:   
                      prefix = key.replace(variable, indice).replace('_day_','_%s_' % grouping )
                    tmp = ocgis_module.call(resource=ncs,
                     variable=variable,
                     dimension_map=dimension_map, 
                     calc=calc,
                     calc_grouping= calc_group, 
                     prefix=prefix, 
                     dir_output=dir_output,
                     output_format='nc')
                    outputs.extend( [tmp] )
                  except Exception as e: 
                    logger.exception('could not calc indice %s for domain in %s' %( indice, key) )   
                else: 
                  for polygon in polygons:
                    try:
                      domain = key.split('_')[1].split('-')[0]
                      if prefix == None: 
                        prefix = key.replace(variable, indice).replace('_day_','_%s_' % grouping ).replace(domain,polygon)
                      tmp = clipping(resource=ncs, 
                        variable=variable, 
                        dimension_map=dimension_map, 
                        calc=calc,  
                        calc_grouping= calc_group, 
                        prefix=prefix,
                        polygons=polygon, 
                        mosaik=False, 
                        dir_output=dir_output)
                      outputs.extend( tmp )
                    except Exception as e:
                      logger.exception('could not calc indice %s for key %s and grouping %s' % ( indice, key, grouping))  
                logger.info('indice file calculated')      
              except Exception as e:
                logger.exception('could not calc indice %s for key %s and grouping %s' %  (indice, key, grouping))  
          except Exception as e:
            logger.exception('could not calc indice %s for key %s' % ( indice, key))        
      except Exception as e:
        logger.exception('could not calc key %s' % key)
    return outputs

def calc_indice_percentile(resources=[], variable='tas', prefix=None, indices='TG90p', , period=None,
    groupings='yr', percentile=90, dir_output=None, dimension_map = None):
    """
    Calculates given indices for suitable files in the appopriate time grouping and polygon.

    :param resource: list of filenames in drs convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param indices: list of indices (default ='SU')
    :param period: reference period
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir
    """
    from os.path import join, dirname, exists
    from os import remove
    import uuid
    from numpy import ma 

    from flyingpigeon.ocgis_module import call
    from flyingpigeon.utils import get_values, get_time
    
    if type(resources) != list: 
      resources = list([resources])
    if type(indices) != list: 
      indices = list([indices])
      
    if type(period) == list: 
      period = period[0]
      
    if period == 'all':
      time_region = None
    else:
      start, end = int(period.split('-'))
      years = range(start, end)
      time_region = {'year': years}

    nc_indices = []
    
    if dir_output != None:
      if not exists(dir_output): 
        makedirs(dir_output)
    
    ########################################################################################################################
    # Compute a custom percentile basis using ICCLIM. ######################################################################
    ########################################################################################################################

    from ocgis.contrib.library_icclim import IcclimTG90p
    
    nc_dic = sort_by_filename(resources)
    for key in nc_dic.keys():
      resource = nc_dic[key]
      nc_reference = call(resource=resource, prefix='nc_reference',time_region=time_region, output_format='nc')
      arr = get_values(nc_files=nc_reference)
      dt_arr = get_time(nc_file=nc_reference)
      arr = ma.masked_array(arr)
      dt_arr = ma.masked_array(dt_arr)
      percentile = percentile
      window_width = 5
      percentile_dict = IcclimTG90p.get_percentile_dict(arr, dt_arr, percentile, window_width)
      for indice in indices: 
        calc = [{'func': 'icclim_%s' % indice, 'name': indice.replace('90',percentile), 'kwds': {'percentile_dict': percentile_dict}}]
        calc_grouping = 'year'
        nc_indices.append(call(resource=resource, prefix=key.replace(variable,indice), 
                            calc=calc, calc_grouping=calc_grouping, output_format='nc'))
    return nc_indices

def calc_indice_unconventional(resource=[], variable=None, prefix=None,
  indices=None, polygons=None,  groupings=None, 
  dir_output=None, dimension_map = None):
    """
    Calculates given indices for suitable files in the appopriate time grouping and polygon.

    :param resource: list of filenames in drs convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param indices: list of indices (default ='TGx')
    :param polygons: list of polgons (default =None)
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into dir_output
    """
    
    from os.path import join, dirname, exists
    from os import remove
    import uuid
    from flyingpigeon import ocgis_module
    from flyingpigeon.subset import get_ugid, get_geom

    if type(resource) != list: 
      resource = list([resource])
    if type(indices) != list: 
      indices = list([indices])
    if type(polygons) != list and polygons != None:
      polygons = list([polygons])
    elif polygons == None:
      polygons = [None]
    else: 
      logger.error('Polygons not found')
    if type(groupings) != list:
      groupings = list([groupings])
    
    if dir_output != None:
      if not exists(dir_output): 
        makedirs(dir_output)
    
    experiments = sort_by_filename(resource)
    outputs = []

    # print('environment for calc_indice_unconventional set')
    logger.info('environment for calc_indice_unconventional set')
    
    for key in experiments:
      if variable == None:
        variable = get_variable(experiments[key][0])
      try: 
        ncs = experiments[key]
        for indice in indices:
          logger.info('indice: %s' % indice)
          try: 
            for grouping in groupings:
              logger.info('grouping: %s' % grouping)
              try:
                calc_group = calc_grouping(grouping)
                logger.info('calc_group: %s' % calc_group)
                for polygon in polygons:  
                  try:
                    domain = key.split('_')[1].split('-')[0]
                    if polygon == None:
                      if prefix == None: 
                        prefix = key.replace(variable, indice).replace('_day_','_%s_' % grouping )
                      geom = None
                      ugid = None
                    else:
                      if prefix == None: 
                        prefix = key.replace(variable, indice).replace('_day_','_%s_' % grouping ).replace(domain,polygon)
                      geom = get_geom(polygon=polygon)
                      ugid = get_ugid(polygons=polygon, geom=geom)
                    if indice == 'TGx':
                      calc=[{'func': 'max', 'name': 'TGx'}]
                      tmp = ocgis_module.call(resource=ncs,# conform_units_to='celcius',
                                              variable=variable, dimension_map=dimension_map, 
                                              calc=calc, calc_grouping=calc_group, prefix=prefix,
                                              dir_output=dir_output, geom=geom, select_ugid=ugid)
                    elif indice == 'TGn':
                      calc=[{'func': 'min', 'name': 'TGn'}]
                      tmp = ocgis_module.call(resource=ncs, #conform_units_to='celcius',
                                              variable=variable, dimension_map=dimension_map, 
                                              calc=calc, calc_grouping= calc_group, prefix=prefix,
                                               dir_output=dir_output, geom=geom, select_ugid = ugid)
                    elif indice == 'TGx5day':
                      calc = [{'func': 'moving_window', 'name': 'TGx5day', 'kwds': {'k': 5, 'operation': 'mean', 'mode': 'same' }}]
                      tmp2 = ocgis_module.call(resource=ncs, #conform_units_to='celcius',
                                              variable=variable, dimension_map=dimension_map, 
                                              calc=calc, prefix=str(uuid.uuid4()),
                                              geom=geom, select_ugid = ugid)
                      calc=[{'func': 'max', 'name': 'TGx5day'}]
                      logger.info('moving window calculated : %s' % tmp2)
                      tmp = ocgis_module.call(resource=tmp2,
                                              variable=indice, dimension_map=dimension_map, 
                                              calc=calc, calc_grouping=calc_group, prefix=prefix,
                                              dir_output=dir_output)
                      remove(tmp2)
                    elif indice == 'TGn5day':
                      calc = [{'func': 'moving_window', 'name': 'TGn5day', 'kwds': {'k': 5, 'operation': 'mean', 'mode': 'same' }}]
                      tmp2 = ocgis_module.call(resource=ncs, #conform_units_to='celcius',
                                              variable=variable, dimension_map=dimension_map, 
                                              calc=calc, prefix=str(uuid.uuid4()),
                                              geom=geom, select_ugid = ugid)
                      calc=[{'func': 'min', 'name': 'TGn5day'}]
                      
                      logger.info('moving window calculated : %s' % tmp2)
                      
                      tmp = ocgis_module.call(resource=tmp2,
                                              variable=indice, dimension_map=dimension_map, 
                                              calc=calc, calc_grouping=calc_group, prefix=prefix,
                                              dir_output=dir_output)
                      remove(tmp2)
                    else: 
                      logger.error('Indice %s is not a known inidce' % (indice))
                    outputs.append(tmp)
                    logger.info('indice file calcualted %s ' % (tmp))
                  except Exception as e:
                    logger.exception('could not calc indice %s for key %s, polygon %s and calc_grouping %s : %s' %  (indice, key, polygon, grouping, e ))
              except Exception as e:
                logger.exception('could not calc indice %s for key %s and calc_grouping %s : %s' % ( indice, key, polygon, e ))
          except Exception as e:
            logger.exception('could not calc indice %s for key %s: %s'%  (indice, key, e ))
      except Exception as e:
        logger.exception('could not calc key %s: %s' % (key, e))
    return outputs
