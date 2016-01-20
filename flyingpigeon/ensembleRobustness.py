from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

def worker(resource=[], start=None, end=None, timeslice=10 ):
  """retuns the result
  
  :param resource: list of pathes to netCDF files
  """
  
  #validation of arguments
  # from flyingpigeon import utils 
  # try:
  #   # 
  #   yr_min = set()
  #   yr_max = set()
  #   ensemble = utils.sort_by_filename(resource, historical_concatination=True)
  #   if start == None or end == None:
  #     for key in ensemble.keys():
  #       yr_min.update(key.split('_')[-1].split('-')[0][0:4])
  #       yr_max.update(key.split('_')[-1].split('-')[1][0:4])
  #     start = int(max(yr_min))
  #     end = int(min(yr_max))
  #   logger.info('start and end set to %s - %s '% (start, end))            
  # except Exception as e:
  #   logger.error('failed to validate arguments: %s' % e )
  
  
  from cdo import Cdo
  cdo = Cdo()
  
  try: 
    # ensemble mean 
    nc_ensmean = cdo.ensmean(input = resource , output = 'nc_ensmean.nc')
    logger.info('ensemble mean calculation done')
  except Exception as e: 
    logger.error('ensemble mean failed: %s ' % e )
  
  try: 
    # ensemble mean 
    nc_ensstd = cdo.ensstd(input = resource , output = 'nc_ensstd.nc')
    logger.info('ensemble std calculation done')
  except Exception as e: 
    logger.error('ensemble std failed: %s ' % e )
  
  # get the get the signal as difference from the beginning (mean over 10 years) and end  (mean over 10 years) of the period:
  try:
    selyearstart = cdo.selyear('1960/1970', input = nc_ensmean, output = 'selyearstart.nc' ) 
    selyearend = cdo.selyear('2003/2013', input = nc_ensmean, output = 'selyearend.nc' )
    meanyearst = cdo.timmean(input = selyearstart, output= 'meanyearst.nc')
    meanyearend = cdo.timmean(input = selyearend, output= 'meanyearend.nc')
    signal = cdo.sub(input=[meanyearend, meanyearst], output = 'signal.nc')
  except Exception as e:
    logger.error('calculation of signal failed: %s ' % e )

  # get the intermodel standard deviation (mean over whole period)
  try:

    ims = cdo.timmean(input = nc_ensstd, output = 'ims.nc')

  except Exception as e:
    logger.error('calculation of internal model std failed: %s ' % e ) 

  try:
    ratio = cdo.div(input=[ims, signal], output='ratio.nc')
  except Exception as e:
    logger.error('calculation ofrobustness mask failed: %s ' % e ) 
  
  try:
    mask = cdo.gtc('1',  input=ratio, output='mask.nc')
  except Exception as e:
    logger.error('calculation ofrobustness mask failed: %s ' % e ) 

  return signal , mask