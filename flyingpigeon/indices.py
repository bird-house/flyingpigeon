from ocgis import OcgOperations, RequestDataset , env
from cdo import Cdo
import tempfile

from .exceptions import CalculationException
from malleefowl import wpslogging as logging
#import logging
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

def calc_indice_simple(resource=[], variable=None, prefix=None,
  indices="SU", polygons='FRA',  groupings="yr", 
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

    from flyingpigeon.utils import calc_grouping, sort_by_filename # aggregations, 
    from flyingpigeon.subset import select_ugid
    output = None
    
    experiments = sort_by_filename(resource)
    outputs = []
    
    for key in experiments:
      try: 
        ncs = experiments[key]
        for indice in indices:
          try: 
            calc = [{'func' : 'icclim_' + indice, 'name' : indice}]
            for polygon in polygons:
              try:
                if len(polygon) == 4: 
                  geom = 'NUTS2'
                elif len(polygon) == 3:
                  geom = '50m_country'
                elif  len(polygon) == 5 and polygon[2] == '.': 
                  geom = 'extremoscope'
                else: 
                  logger.error('unknown polygon %s', polygon)
                ugid = select_ugid(polygons=polygon, geom=geom)
                for grouping in groupings:
                  try:
                    #prefix = key.replace('_day_', grouping)
                    calc_group = calc_grouping(grouping)
                    rd = RequestDataset(uri=ncs, variable=variable, dimension_map=dimension_map)
                    ops = OcgOperations(
                                    dataset=rd,
                                    calc=calc,
                                    geom=geom,
                                    select_ugid= ugid, 
                                    calc_grouping=calc_group,
                                    prefix=prefix,
                                    output_format='nc',
                                    dir_output=out_dir,
                                    add_auxiliary_files=False)
                    outputs.append( ops.execute() )
                  except Exception as e:
                    logger.exception('could not calc indice %s for key %s, polygon %s and calc_grouping %s : %s', indice, key, polygon, grouping, e )  
              except Exception as e:
                logger.exception('could not calc indice %s for key %s and polygon%s : %s', indice, key, polygon, e )  
          except Exception as e:
            logger.exception('could not calc indice %s for key %s: %s', indice, key, e )        
      except Exception as e:
        logger.exception('could not calc key %s: %s', key, e)

<<<<<<< HEAD
    return outputs
=======
    return outputs


def multipro_indice_simple(resource=[], indices="SU", polygons='FRA',  groupings="yr", out_dir=None, dimension_map = None, variable=None):
    """
    Calculates given indices for suitable files in the appopriate time grouping and polygon.

    :param resource: list of filenames in drs convention (netcdf)
    :param indices: list of indices (default ='SU'). Indices must be calcualteable with the input resouce variable
    :param polygons: list of polgons (default ='FRA')
    :param grouping: indices time aggregation (default='yr')
    :param out_dir: output directory for result file (netcdf)
    :param dimension_map: optional dimension map if different to standard (default=None)

    :return: list of netcdf files with calculated indices. Files are saved into out_dir
    """
    
    from flyingpigeon.utils import calc_grouping, sort_by_filename # aggregations, 
    from flyingpigeon.subset import select_ugid
    import multiprocessing
    ncpu = multiprocessing.cpu_count()

    if type(resource) != list: 
      resource = list([resource])
    if type(indices) != list: 
      indices = list([indices])
    if type(polygons) != list:
      polygons = list([polygons])
    if type(groupings) != list:
      groupings = list([groupings])
      
    experiments = sort_by_filename(resource)  

    def indice_simple_worker(w_ncs, w_dimension_map, w_variable, w_calc, w_calc_group, w_geom, w_ugid, w_prefix, w_out_dir):
        """mulitporcessing worker function for indices_simple"""
        
        from datetime import datetime
        try: 
          print '%s start processing for %s' % ( datetime.strftime(datetime.now(),format='%Y.%m.%d %H:%M:%S' ), w_prefix)
          rd = RequestDataset(uri=w_ncs, variable=w_variable, dimension_map=w_dimension_map)
          ops = OcgOperations(dataset=rd,
                              calc=w_calc,
                              geom=w_geom,
                              select_ugid= w_ugid, 
                              calc_grouping=w_calc_group,
                              prefix=w_prefix,
                              output_format='nc',
                              dir_output=w_out_dir,
                              add_auxiliary_files=False)
          output= ops.execute()
          print '%s exit processing for %s' % (datetime.strftime(datetime.now(),format='%Y.%m.%d %H:%M:%S' ), w_prefix)
        except Exception as e:
          logger.exception('indices_simple_worker failed: %s', e)
    
    env.OVERWRITE = True
    output = None
    outputs = []
    
    if __name__ == 'flyingpigeon.indices':
        print 'flyingpigeon.indices runnning'
        pool = multiprocessing.Pool(ncpu) #use all available cores, otherwise specify the number you want as an argument
        for key in experiments:
          try: 
            ncs = experiments[key]
            
            for indice in indices:
              try: 
                calc = [{'func' : 'icclim_' + indice, 'name' : indice}]
                for polygon in polygons:
                  try:
                    if len(polygon) == 4: 
                      geom = 'NUTS2'
                    elif len(polygon) == 3:
                      geom = '50m_country'
                    else: 
                      logger.error('unknown polygon %s', polygon)
                    ugid = select_ugid(polygon=polygon, geom=geom)
                    for grouping in groupings:
                      try:
                        if variable == None: 
                          variable = key.split('_')[0]
                        prefix = key.replace('day', grouping).replace('EUR', polygon).replace(variable,indice)
                        calc_group = calc_grouping(grouping)
                        #for i in xrange(0, 512):
                          # f, args=(i,))
                        
                        jobs = []
                        p = pool.apply_async(indice_simple_worker,
                                            args=(ncs, dimension_map, variable, calc, calc_group, geom, ugid, prefix, out_dir, ))
                        jobs.append(p)
                        #p.start()
                        #outputs.append( )
                        
                      except Exception as e:
                        logger.exception('could not calc indice %s for key %s, polygon %s and calc_grouping %s : %s', indice, key, polygon, grouping, e )  
                  except Exception as e:
                    logger.exception('could not calc indice %s for key %s and polygon%s : %s', indice, key, polygon, e )  
              except Exception as e:
                logger.exception('could not calc indice %s for key %s: %s', indice, key, e )        
          except Exception as e:
            logger.exception('could not calc key %s: %s', key, e)

        pool.close()
        pool.join()
    return jobs # outputs
>>>>>>> f64fae462f0dc061398aadb98763545e0be23b47
