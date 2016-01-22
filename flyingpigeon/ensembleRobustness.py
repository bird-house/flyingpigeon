from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

def worker(resource=[], start=None, end=None, timeslice=20 ):
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
  
  start1 = start
  start2 = start + timeslice - 1 
  end1 = end - timeslice + 1
  end2 = end

  from cdo import Cdo
  cdo = Cdo()
  
  cdo.forceOutput = True 

  try: 
    # ensemble mean 
    nc_ensmean = cdo.ensmean(input = resource , output = 'nc_ensmean.nc')
    logger.info('ensemble mean calculation done')
  except Exception as e: 
    logger.error('ensemble mean failed: %s ' % e )
  
  try: 
    # ensemble std 
    nc_ensstd  = cdo.ensstd(input = resource , output = 'nc_ensstd.nc')
    logger.info('ensemble std and calculation done')
  except Exception as e: 
    logger.error('ensemble std or failed: %s ' % e )
  
  # get the get the signal as difference from the beginning (mean over 10 years) and end  (mean over 10 years) of the period:
  try:
    selyearstart = cdo.selyear('%s/%s' % (start1,start2), input = nc_ensmean, output = 'selyearstart.nc' ) 
    selyearend = cdo.selyear('%s/%s' % (end1,end2), input = nc_ensmean, output = 'selyearend.nc' )
    meanyearst = cdo.timmean(input = selyearstart, output= 'meanyearst.nc')
    meanyearend = cdo.timmean(input = selyearend, output= 'meanyearend.nc')
    signal = cdo.sub(input=[meanyearst, meanyearend], output = 'signal.nc')
    logger.info('Signal calculation done')
  except Exception as e:
    logger.error('calculation of signal failed: %s ' % e )

  # get the intermodel standard deviation (mean over whole period)
  try:
    std_selyear = cdo.selyear('%s/%s' % (end1,end2), input=nc_ensstd, output='std_selyear.nc')
    std = cdo.timmean(input = std_selyear, output = 'std.nc')
    std2 = cdo.mulc('2', input = std, output = 'std2.nc')
    logger.info('calculation of internal model std for time period done')
  except Exception as e:
    logger.error('calculation of internal model std failed: %s ' % e ) 

  try:
    high_agreement_mask = cdo.gt(input=[std2, signal], output= 'large_change_with_high_model_agreement.nc')
    low_agreement_mask = cdo.lt(input=[std, signal], output= 'small_signal_or_low_agreement_of_models.nc')
    #ratio = cdo.div(input=[ims, signal], output='ratio.nc')
    logger.info('high and low mask done')
  except Exception as e:
    logger.error('calculation ofrobustness mask failed: %s ' % e ) 
  
  # try: 
  #   from flyingpigeon.visualisation import map_ensembleRobustness
  #   from flyingpigeon.utils import get_variable
  #   variable = get_variable(signal)
  #   graphic = map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, 
  #             variable=variable, 
  #             cmap='seismic', 
  #             title='Change of %s %s-%s to %s-%s' % (variable, start1, start2, end1, end2))
  # except Exception as e:
  #   logger.error('graphic generation failed: %s ' % e )

  return signal , low_agreement_mask , high_agreement_mask #, graphic