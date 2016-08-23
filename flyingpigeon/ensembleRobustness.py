from flyingpigeon.visualisation import map_ensembleRobustness
from flyingpigeon.utils import get_variable
from flyingpigeon.utils import sort_by_filename
from flyingpigeon.utils import get_time
    
import logging
logger = logging.getLogger(__name__)

def worker(resource=[], start=None, end=None, timeslice=20, 
  variable=None, title=None, cmap='seismic' ):
  """retuns the result
  
  :param resource: list of pathes to netCDF files
  :param start: beginning of reference period (if None (default), the firs year of consistent ensemble will be detected)
  :param end: end of comparison period (if None (default), the last year of the consistent ensemble will be detected)
  :param timeslice: period lenght for mean calculation of reference and comparison period
  :param variable: variable name to be detected in the netCDF file. If not set (not recommended) the variable name will be detected
  :param title: str to be used as title for the signal mal
  :param cmap: define the color sceem for signal map plotting 

  :return: signal.nc, low_agreement_mask.nc, high_agreement_mask.nc, graphic.png, text.txt
  """
  from os.path import split
  from cdo import Cdo
  cdo = Cdo()
  cdo.forceOutput = True 
  
  try: 
    # preparing the resource
#    from flyingpigeon.ocgis_module import call
    file_dic = sort_by_filename(resource, historical_concatination = True)
    #print file_dic
    logger.info('file names sorted experimets: %s' % len(file_dic.keys()))
  except Exception as e:
    msg = 'failed to sort the input files'
    logger.exception(msg)
    raise Exception(msg)

  try:
    mergefiles = []
    for key in file_dic.keys():
      
      if type(file_dic[key]) == list and len(file_dic[key]) > 1:
        input = []
        for i in file_dic[key]:
          print i 
          input.extend([i.replace(' ','\\\ ')])
        mergefiles.append(cdo.mergetime(input=input, output=key+'_mergetime.nc'))
      else:
        mergefiles.extend(file_dic[key])
#      files.append(cdo.selyear('%s/%s' % (start1,end2), input = tmpfile , output =  key+'.nc' )) #python version
    logger.info('datasets merged %s ' % mergefiles)
  except Exception as e:
    msg = 'seltime and mergetime failed %s' % e
    logger.exception(msg)
    raise Exception(e)    
  
  try: 
    text_src = open('infiles.txt', 'a')
    for key in file_dic.keys():
      text_src.write(key + '\n')
    text_src.close()
  except Exception as e:
    msg = 'failed to write source textfile'
    logger.exception(msg)
    raise Exception(msg)
    
  # configure reference and compare period
  try: 
    if start == None:
      st_set = set()
      en_set = set()
      for f in mergefiles:
        print f
        times = get_time(f)
        st_set.update([times[0].year])
        if end == None: 
          en_set.update([times[-1].year])
      start = max(st_set)
      if end == None:
        end = min(en_set)
    logger.info('Start and End: %s - %s ' % (start, end))
    if start >= end: 
      logger.error('ensemble is inconsistent!!! start year is later than end year')
  except Exception as e:
    msg = 'failed to detect start and end times of the ensemble'
    logger.exception(msg)
    raise Exception(msg)

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
    logger.info('timeslice and periodes set')
  except Exception as e:
    msg = 'failed to set the periodes'
    logger.exception(msg)
    raise Exception(msg)

  try:
    files = []
    for i, mf in enumerate(mergefiles):
      files.append(cdo.selyear('{0}/{1}'.format(start1,end2), input=[mf.replace(' ','\ ')] , output='file_{0}_.nc'.format(i) )) #python version
    logger.info('timeseries selected from defined start to end year')
  except Exception as e:
    msg = 'seltime and mergetime failed'
    logger.exception(msg)
    raise Exception(msg)    

  try: 
    # ensemble mean 
    nc_ensmean = cdo.ensmean(input=files , output='nc_ensmean.nc')
    logger.info('ensemble mean calculation done')
  except Exception as e:
    msg = 'ensemble mean failed'
    logger.exception(msg)
    raise Exception(msg)
  
  try: 
    # ensemble std 
    nc_ensstd  = cdo.ensstd(input=files , output='nc_ensstd.nc')
    logger.info('ensemble std and calculation done')
  except Exception as e:
    msg = 'ensemble std or failed'
    logger.exception(msg)
    raise Exception(msg)
  
  # get the get the signal as difference from the beginning (first years) and end period (last years), :
  try:
    selyearstart = cdo.selyear('%s/%s' % (start1,start2), input = nc_ensmean, output = 'selyearstart.nc' ) 
    selyearend = cdo.selyear('%s/%s' % (end1,end2), input = nc_ensmean, output = 'selyearend.nc' )
    meanyearst = cdo.timmean(input = selyearstart, output= 'meanyearst.nc')
    meanyearend = cdo.timmean(input = selyearend, output= 'meanyearend.nc')
    signal = cdo.sub(input=[meanyearend, meanyearst], output = 'signal.nc')
    logger.info('Signal calculation done')
  except Exception as e:
    msg = 'calculation of signal failed'
    logger.exception(msg)
    raise Exception(msg)
  
  # get the intermodel standard deviation (mean over whole period)
  try:
    #std_selyear = cdo.selyear('%s/%s' % (end1,end2), input=nc_ensstd, output='std_selyear.nc')
    #std = cdo.timmean(input = std_selyear, output = 'std.nc')
    
    std = cdo.timmean(input = nc_ensstd, output = 'std.nc')
    std2 = cdo.mulc('2', input = std, output = 'std2.nc')
    logger.info('calculation of internal model std for time period done')
  except Exception as e:
    msg = 'calculation of internal model std failed'
    logger.exception(msg) 
    raise Exception(msg)
  try:
    absolut = cdo.abs(input=signal, output='absolut_signal.nc')
    high_agreement_mask = cdo.gt(input=[absolut,std2],  output= 'large_change_with_high_model_agreement.nc')
    low_agreement_mask = cdo.lt(input=[absolut,std], output= 'small_signal_or_low_agreement_of_models.nc')
    logger.info('high and low mask done')
  except Exception as e:
    msg = 'calculation of robustness mask failed'
    logger.exception(msg)
    raise Exception(msg)
  
  try: 
    
    if variable == None: 
      variable = get_variable(signal)

    if title == None: 
      title='Change of %s (difference of mean %s-%s to %s-%s)' % (variable, end1, end2, start1, start2)  
    
    logger.info('variable to be plotted: %s' % variable)
    graphic = None
    graphic = map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, 
              variable=variable, 
              cmap=cmap,
              title = title)

  except Exception as e:
    print 'graphic generation failed'
    #msg = 'graphic generation failed'
    #logger.exception(msg)
    #raise Exception(msg)
  return signal, low_agreement_mask, high_agreement_mask, graphic, text_src
