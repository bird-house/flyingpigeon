import logging
logger = logging.getLogger(__name__)

def worker(resource=[], start=None, end=None, timeslice=20, variable=None ):
  """retuns the result
  
  :param resource: list of pathes to netCDF files
  """
  from cdo import Cdo
  cdo = Cdo()
  
  cdo.forceOutput = True 
  
  try: 
    # preparing the resource
    from flyingpigeon.utils import sort_by_filename
#    from flyingpigeon.ocgis_module import call
    
    file_dic = sort_by_filename(resource)
    files = []
    for key in file_dic.keys(): 
      files.append(cdo.mergetime(input=file_dic[key], output=key+'.nc' ))
  except Exception as e: 
    logger.error('failed to sort and merge the input files')
  
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
  
  start1 = int(start)
  start2 = start1 + int(timeslice) - 1 
  
  end2 = int(end)
  end1 = end2 - int(timeslice) + 1

  
  try: 
    # ensemble mean 
    nc_ensmean = cdo.ensmean(input = files , output = 'nc_ensmean.nc')
    logger.info('ensemble mean calculation done')
  except Exception as e: 
    logger.exception('ensemble mean failed')
    raise
  
  try: 
    # ensemble std 
    nc_ensstd  = cdo.ensstd(input = files , output = 'nc_ensstd.nc')
    logger.info('ensemble std and calculation done')
  except Exception as e: 
    logger.exception('ensemble std or failed')
    raise
  
  # get the get the signal as difference from the beginning (mean over 10 years) and end  (mean over 10 years) of the period:
  try:
    selyearstart = cdo.selyear('%s/%s' % (start1,start2), input = nc_ensmean, output = 'selyearstart.nc' ) 
    selyearend = cdo.selyear('%s/%s' % (end1,end2), input = nc_ensmean, output = 'selyearend.nc' )
    meanyearst = cdo.timmean(input = selyearstart, output= 'meanyearst.nc')
    meanyearend = cdo.timmean(input = selyearend, output= 'meanyearend.nc')
    signal = cdo.sub(input=[meanyearend, meanyearst], output = 'signal.nc')
    logger.info('Signal calculation done')
  except Exception as e:
    logger.exception('calculation of signal failed')
    raise
  
  # get the intermodel standard deviation (mean over whole period)
  try:
    std_selyear = cdo.selyear('%s/%s' % (end1,end2), input=nc_ensstd, output='std_selyear.nc')
    std = cdo.timmean(input = std_selyear, output = 'std.nc')
    std2 = cdo.mulc('2', input = std, output = 'std2.nc')
    logger.info('calculation of internal model std for time period done')
  except Exception as e:
    logger.exception('calculation of internal model std failed') 
    raise
  try:
    high_agreement_mask = cdo.gt(input=[std2, signal], output= 'large_change_with_high_model_agreement.nc')
    low_agreement_mask = cdo.lt(input=[std, signal], output= 'small_signal_or_low_agreement_of_models.nc')
    #ratio = cdo.div(input=[ims, signal], output='ratio.nc')
    logger.info('high and low mask done')
  except Exception as e:
    logger.exception('calculation ofrobustness mask failed')
    raise 
  
  try: 
    from flyingpigeon.visualisation import map_ensembleRobustness
    from flyingpigeon.utils import get_variable
    
    if variable == None: 
      variable = get_variable(signal)
    
    logger.info('variable to be plotted: %s' % variable)
    
    graphic = map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, 
              variable=variable, 
              cmap='seismic', 
              title='Change of %s %s-%s to %s-%s' % (variable, start1, start2, end1, end2))
  except Exception as e:
    logger.error('graphic generation failed: %s ' % e )

  return signal, low_agreement_mask, high_agreement_mask, graphic
