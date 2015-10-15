from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


def modelUncertaintyWorker(resource):
  """retuns the result
  
  :param resource: list of pathes to netCDF files
  """
  from cdo import Cdo
  cdo = Cdo()
  from flyingpigeon.utils import check_timestepps
  
  # check resource of consintency 
  resource_qc =  check_timestepps(resource)
  # for nc in resource
  
  try: 
    # ensemble mean 
    nc_ensmean = cdo.ensmean(input = resource_qc , output = 'nc_ensmean.nc')
    logger.info('ensemble calculation done')
  except Exception as e: 
    logger.error('ensemble mean failed: %s ' % e )
  # mean + sigma as a mask 
  # sigma1 = cdo.fldstd(input = nc_ensmean, output = 'nc_sigma1.nc')
  # mask 
  
  # mean + 2* sigma as a mask
  
  # merge to on result netCDF
  
  result = nc_ensmean
  return result