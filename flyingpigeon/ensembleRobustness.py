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

  #  cdo timmean ensstd.nc ims.nc

  # get the values over or above 1 as value of robustness
  # cdo div ims.nc magnitude.nc robustness.nc




  # mean + sigma as a mask 
  # sigma1 = cdo.fldstd(input = nc_ensmean, output = 'nc_sigma1.nc')
  # mask 
  
  # mean + 2* sigma as a mask
  
  # merge to on result netCDF
  
  
    #try: 
    ## ensemble mean and magnitude
    #nc_ensmean = cdo.ensmean(input = resource_qc, output = 'nc_ensmean.nc')    
    #logger.info('ensmean calculations done')
  #except Exception as e: 
    #logger.error('ensmean calculations failed: %s ' % e )

  #try:     
    ##nc_delta = nc_ensmean(lastpt) - nc_ensmean(firstpt)    
    #nc_laststep = cdo.seltimestep('-1', input = nc_ensmean, output = 'nc_laststep.nc')
    #nc_firststep = cdo.seltimestep(1, input = nc_ensmean, output = 'nc_firststep.nc')
    #nc_delta = cdo.sub(input = [nc_laststep, nc_firststep], output = 'nc_delta.nc')
    #nc_absdelta = cdo.abs(input = nc_delta, output = 'nc_absdelta.nc')
    #logger.info('delta calculation done')
  #except Exception as e: 
    #logger.error('delta calculation failed: %s ' % e )  

  #try: 
    ## ensemble std    
    #nc_ensstd = cdo.ensstd(input = resource_qc, output = 'nc_ensstd.nc')
    #logger.info('std calculation done')
  #except Exception as e: 
    #logger.error('std calculation failed: %s ' % e )

  #try:
    ## compute mask: if nc_absdelta > nc_enssstd
    #nc_level = cdo.mulc(1, input = nc_ensstd, output = 'nc_level.nc')
    #nc_binmask = cdo.gt(input = [nc_absdelta, nc_level], output = 'nc_binmask.nc')
    #logger.info('calculated mask')
  #except Exception as e: 
    #logger.error('mask calculation failed: %s ' % e )

  
  ##if > const
  ##cdo.gtc('arg', input=infile, ontput=outfile)

  ## mean + sigma as a mask 
  ## sigma1 = cdo.fldstd(input = nc_ensmean, output = 'nc_sigma1.nc')
  ## mask 
  
  ## mean + 2* sigma as a mask
  
  ## merge to on result netCDF
  ## cdo.merge(input=[file1, file2], output='result.nc')

  #result = nc_ensmean    #ensemble mean
  #result2 = nc_ensstd    #ensemble std
  #result3 = nc_absdelta  #magnitude of model change  
  #result4 = nc_binmask   #absdelta > std
  
  return signal , nc_ensstd 
  
  