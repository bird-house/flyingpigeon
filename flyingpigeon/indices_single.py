
import tempfile

from .exceptions import CalculationException

from flyingpigeon.utils import calc_grouping, sort_by_filename # aggregations, 
from flyingpigeon.subset import get_ugid, get_geom

from malleefowl import wpslogging as logging
import os
#import logging
logger = logging.getLogger(__name__)

#logfile = os.path.basename(__file__).replace('.py','.log') #  'several_nodes.log' # 
#fh = logging.FileHandler(os.path.join('/home/estimr2/nhempelmann/data/test/'+ logfile))
#fh.setLevel(logging.DEBUG)
# create console handler with a higher log level
#ch = logging.StreamHandler()
#ch.setLevel(logging.ERROR)
# create formatter and add it to the handlers
#formatter = logging.Formatter('%(asctime)s - %(name)s - %(levelname)s - %(message)s')
#ch.setFormatter(formatter)
#fh.setFormatter(formatter)
# add the handlers to logger
#logger.addHandler(ch)
#logger.addHandler(fh)

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
    RR=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
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
    R75p=dict(variable='pr', description= 'Days with RR > 75th percentile of daily amounts (moderate wet days) (days)'),
    R75pTOT=dict(variable='pr', description= 'Precipitation fraction due to moderate wet days (>75th percentile) (%)'),
    R95p=dict(variable='pr', description= 'Days with RR > 95th percentile of daily amounts (very wet days) (days)'),
    R95pTOT=dict(variable='pr', description= 'Precipitation fraction due to very wet days (>95th percentile) (%)'),
    R99p=dict(variable='pr', description= 'Days with RR > 99th percentile of daily amounts (extremely wet days)(days)'),
    R99pTOT=dict(variable='pr', description= 'recipitation fraction due to extremely wet days (>99th percentile)(%)'),
    )   

_INDICEScomp_ = dict(
    CD=dict(variable=['tas','pr'], description='Days with TG < 25th percentile of daily mean temperature and RR < 25th percentile of daily precipitation sum (cold/dry days)'),
    CW=dict(variable=['tas','pr'], description='Days with TG < 25th percentile of daily mean temperature and RR > 75th percentile of daily precipitation sum (cold/wet days)'),
    WD=dict(variable=['tas','pr'], description='days with TG > 75th percentile of daily mean temperature and RR < 25th percentile of daily precipitation sum (warm/dry days)'),
    WW=dict(variable=['tas','pr'], description='Days with TG > 75th percentile of daily mean temperature and RR > 75th percentile of daily precipitation sum (warm/wet days)'),
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

def calc_indice_single(resource=[], variable=None, prefix=None,
  indices="SU", polygons=None,  groupings="yr", 
  out_dir=None, dimension_map = None):
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

    from os.path import join, dirname


    #DIR_SHP = join(dirname(__file__),'flyingpigeon', 'processes', 'shapefiles')
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
    
    from flyingpigeon.subset import clipping
    #from flyingpigeon.subset import select_ugid
    #    tile_dim = 25
    output = None
    
    experiments = sort_by_filename(resource)
    outputs = []
    
    for key in experiments:
      try: 
        ncs = experiments[key]
        for indice in indices:
          try: 
            calc = [{'func' : 'icclim_' + indice, 'name' : indice}]
            for grouping in groupings:
              try:
                calc_group = calc_grouping(grouping)
                if polygons == None:
                  try:  
                    from ocgis import RequestDataset , OcgOperations
                    from ocgis.util.large_array import compute
                    from os.path import getsize
                    from numpy import sqrt
                    rd = RequestDataset(uri=ncs, variable=variable, dimension_map=dimension_map)
                    ops = OcgOperations(
                      dataset=rd,
                      calc=calc,
                      calc_grouping=calc_group,
                      prefix=prefix,
                      output_format='nc',
                      dir_output=out_dir,
                      add_auxiliary_files=False)
                    # swith beween chunking and 'en block' computation  
                    fsize = 0 
                    limit_memory_mb = 475
                    for nc in ncs: 
                      fsize = fsize + getsize(nc)
                    if fsize / 1000000 <= 500:          
                      tmp = ops.execute()
                      outputs.append( tmp )
                    else: 
                    # calcultion of chunk size
                      size = ops.get_base_request_size()
                      nb_time_coordinates_rd = size['variables'][variable]['temporal']['shape'][0]
                      element_in_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
                      element_in_mb = element_in_kb*0.001
                      tile_dim = sqrt(limit_memory_mb/(element_in_mb*nb_time_coordinates_rd))
                      geom_file = compute(ops, tile_dimension=int(tile_dim), verbose=True)
                      geom_files.append( geom_file )
                      tmp = compute(ops, tile_dimension=tile_dim, verbose=True)
                      outputs.append(tmp)
                  except Exception as e: 
                    logger.exception('could not calc indice %s for domain in %s: %s', indice, key, e )   
                else: 
                  for polygon in polygons:
                    try:
                      tmp = clipping(resource=ncs, variable=variable, 
                        dimension_map=dimension_map, 
                        calc=calc,  
                        calc_grouping= calc_group, 
                        prefix=prefix, 
                        polygons=polygons, 
                        dir_output=out_dir)
                      for t in tmp: 
                        outputs.append( t ) 
                    except Exception as e:
                      logger.exception('could not calc indice %s for key %s, polygon %s and calc_grouping %s : %s', indice, key, polygon, grouping, e )  
              except Exception as e:
                logger.exception('could not calc indice %s for key %s and calc_grouping %s : %s', indice, key, polygon, e )  
          except Exception as e:
            logger.exception('could not calc indice %s for key %s: %s', indice, key, e )        
      except Exception as e:
        logger.exception('could not calc key %s: %s', key, e)
    return outputs

def calc_indice_percentil(resource=[], variable=None, time_range_ref=None, prefix=None,
  indices="R95p", polygons='FRA',  groupings="yr", 
  out_dir=None, dimension_map = None):
    """
    Calculates given indices (percentile based ones) for suitable files in the appopriate time grouping and polygon.

    :param resource: list of filenames in drs convention (netcdf)
    :param variable: variable name to be selected in the in netcdf file (default=None)
    :param time_range_ref: list with two datetime objectes defining start and end of the reverence period. 
    If time_range_ref=None (default) 01.01.1961-31.12.1990 will be used. 
    :param indices: list of indices (default ='SU')
    :param polygons: list of polgons (default ='FRA')
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir
    """

    from os.path import join, dirname
    import datetime as dt
    from ocgis import OcgOperations, RequestDataset , env
 
    from ocgis.calc.library.index.dynamic_kernel_percentile import DynamicDailyKernelPercentileThreshold

    DIR_SHP = join(dirname(__file__),'flyingpigeon', 'processes', 'shapefiles')
    env.DIR_SHPCABINET = DIR_SHP
    env.OVERWRITE = True

    if type(resource) != list: 
      resource = list([resource])
    if type(indices) != list: 
      indices = list([indices])
    if type(polygons) != list:
      polygons = list([polygons])
    if type(groupings) != list:
      groupings = list([groupings])

    if time_range_ref==None:
        dt1_ref = dt.datetime(1961, 01, 01)
        dt2_ref = dt.datetime(1990, 12, 31)
        time_range_ref = [dt1_ref, dt2_ref]

    experiments = sort_by_filename(resource)
    outputs = []    

    for key in experiments:
      try: 
        ncs = experiments[key]
        for indice in indices:
          try: 
            for polygon in polygons:
              try:

                geom = get_geom(polygon=polygon)
                ugid = get_ugid(polygons=polygon, geom=geom)
                
                for grouping in groupings:
                  try:
                    rd = RequestDataset(uri=ncs, variable=variable, time_range=time_range_ref)
                    basis_indice = rd.get()
                    # get reference:
                    rd_ref = RequestDataset(uri=ncs, variable=variable, time_range=time_range_ref)
                    basis_ref = rd_ref.get()
                    # calculation of percentil based indice:
                    values_ref = basis_ref.variables[variable].value
                    print values_ref
                    temporal = basis_ref.temporal.value_datetime
                    percentile = 95
                    width = 5 # 5-day window
                    daily_percentile = DynamicDailyKernelPercentileThreshold.get_daily_percentile(values_ref,temporal,percentile,width)
                    indice = indices
                    calc_group =  calc_grouping(grouping) # ['year', 'month'] # or other
                    kwds = {'percentile':percentile,'width':width, 'operation':'lt', 'daily_percentile':daily_percentile} # operation: lt = "less then", beacause we count the number of days < 10th percentile  calc = [{'func':'dynamic_kernel_percentile_threshold','name':'TG10p','kwds':kwds}]
                    calc = [{'func':'dynamic_kernel_percentile_threshold','name':indice,'kwds':kwds}]
                    ops = OcgOperations(dataset=rd,calc_grouping=calc_group,calc=calc, output_format='nc', prefix='indiceTG10p_test', add_auxiliary_files=False)
                    outputs.append( ops.execute() )
                  except Exception as e:
                    logger.exception('could not calc indice %s for key %s, polygon %s and calc_grouping %s : %s', indice, key, polygon, grouping, e )  
              except Exception as e:
                logger.exception('could not calc indice %s for key %s and polygon%s : %s', indice, key, polygon, e )  
          except Exception as e:
            logger.exception('could not calc indice %s for key %s: %s', indice, key, e )        
      except Exception as e:
        logger.exception('could not calc key %s: %s', key, e)
    return outputs
