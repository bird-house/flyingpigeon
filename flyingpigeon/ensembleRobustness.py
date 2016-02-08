import logging
logger = logging.getLogger(__name__)

def worker(resource=[], start=None, end=None, timeslice=20, variable=None, title=None, cmap='seismic' ):
  """retuns the result
  
  :param resource: list of pathes to netCDF files
  :param start: beginning of reference period (if None (default), the firs year of consistent ensemble will be detected)
  :param end: end of comparison period (if None (default), the last year of the consistent ensemble will be detected)
  :param timeslice: period lenght for mean calculation of reference and comparison period
  :param variable: variable name to be detected in the netCDF file. If not set (not recommended) the variable name will be detected
  :param title: str to be used as title for the signal mal
  :param cmap: define the color sceem for signal map plotting 

  :return: signal.nc, low_agreement_mask.nc, high_agreement_mask.nc, graphic.png
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
  

  # configure reference and compare period
  try: 
    from flyingpigeon.utils import get_time

    if start == None:
      st_set = set()
      en_set = set()
      for f in files: 
        times = get_time(f)
        st_set.update([times[0].year])
        if end == None: 
          en_set.update([times[-1].year])
      start = max(st_set)
      if end == None:
        end = min(en_set)
    logger.info('Start and End: %s - %s ' % (start, end))      
  except Exception as e:
    logger.error('failed to detect start and end times of the ensemble: %s' % e)


  # set the periodes: 
  try: 
    start = int(start)
    end = int(end)

    if timeslice == None: 
      timeslice = int((end - start) / 3)
      if timeslice == 0: 
        timeslice = 1
    else: 
      timeslice = int(timeslice)

    start1 = start
    start2 = start1 + timeslice - 1 
    
    end1 = end - timeslice + 1
    end2 = end

  except Exception as e:
    logger.error('failed to set the periodes: %s' % e)

  
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
  
  # get the get the signal as difference from the beginning (first years) and end period (last years), :
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
    logger.exception('calculation of robustness mask failed')
    raise 
  
  try: 
    from flyingpigeon.visualisation import map_ensembleRobustness
    from flyingpigeon.utils import get_variable
    
    if variable == None: 
      variable = get_variable(signal)

    if title == None: 
      title='Change of %s (difference of mean %s-%s to %s-%s)' % (variable, end1, end2, start1, start2)  
    
    logger.info('variable to be plotted: %s' % variable)
    
    graphic = map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, 
              variable=variable, 
              cmap=cmap,
              title = title)

  except Exception as e:
    logger.error('graphic generation failed: %s ' % e )
  return signal, low_agreement_mask, high_agreement_mask, graphic
