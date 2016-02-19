import logging
logger = logging.getLogger(__name__)

from netCDF4 import Dataset

def set_basic_md(resource):
  """
  basis meta data
  :param resource: netCDF file where basic meta data should be set
  """
  import sys

  py_version = sys.version

  md_basic = {
     'activity': 'birdhouse project',
     'contact':'ehbrecht@dkrz.de',
     'software':'flyingpigeon v 0.1',
     'contact_2':'nils.hempelmann@lsce.ipsl.fr', 
     'software_platform': py_version,
     'software_project': 'birdhouse'}
  return(resource)

def set_dynamic_md(resource):
  """
  Dynamic meta data like time frequency, spatial extend, start end time etc.
  :param resource: netCDF file where basic meta data should be set
  """
  time_frequency = get_frequency(resource)

  md_dynamic = {
     'activity': 'birdhouse project',
     'software': 'flyingpigeon v 0.1',
     'software_platform': py_version,
     'software_project': 'birdhouse'}
  return(resource)


def get_frequency(resource):
  """
  returns the frequency of the time stamps
  :param resource: NetCDF file
  :return: frequency
  """
  from netCDF4 import num2date
  from numpy import mean

  ds = Dataset(resource)
  time = ds.variables['time']

  dates = num2date(time[:], time.units, time.calendar)

  diffs = []
  m = len(time)
  if m > 100: 
    m = 100 

  for i, date in enumerate(dates):
    if i > 0 and i < m: 
      diff = date - dates[i-1] 
      diffs.append(diff.days)

  fqz = mean(diffs)

  if (350 < fqz < 370):
    frequency = 'yr'
  if (80 < fqz < 100):
    frequency = 'sem'
  if (25 < fqz < 35):
    frequency = 'mon'
  if (0.8 < fqz < 1.2):
    frequency = 'day'
  if (0.4 < fqz < 0.6):
    frequency = '12h'
  if (0.22 < fqz < 0.27):
    frequency = '6h'
  if (0.013 < fqz < 0.014):
    frequency = '3h'

  return frequency

def get_extent(resource):
  """
  returns the spatial extention of the values 
  :param resource: NetCDF file
  :return: min_lat, max_lat, min_lon, max_lon
  """

  ds = Dataset(resource)
  
  lats = ds.variables['lat']
  lons = ds.variables['lon']

  if len(lats.shape) == 1:
    min_lat = min(lats) 
    max_lat = max(lats)
    min_lon = min(lons) 
    max_lon = max(lons)
  else: 
    logger.error('latitude variable not found!!')  

  return lats, lons #min_lat, max_lat, min_lon, max_lon




def set_metadata_segetalflora(resource):
  """
  :param resources: imput files 
  """
  from netCDF4 import Dataset
  
  dic_segetalflora = dic_basis
  dic_segetalflora['keywords'] = 'Segetalflora'
  ds = Dataset(resource, mode='a')
  ds.setncatts(dic_segetalflora)
  ds.close()
 
  return resource



def set_metadata(resources):
  
  if type(resources) == str:
    resources = [resources]
    
  for resource in resources:
    ds = Dateset
    ds.attncatts(dic_segetalflora)
    ds.close()
    
  return resources