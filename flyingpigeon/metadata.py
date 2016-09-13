import logging
logger = logging.getLogger(__name__)

from netCDF4 import Dataset

def set_basic_md(resource):
  """
  basis meta data
  :param resource: netCDF file where basic meta data should be set
  """
  import sys
  from datetime import datetime as dt 
  
  py_version = sys.version
  creation_date = dt.strftime( dt.now(), format='%Y-%m-%dT%H:%M:%S')
  
  md_basic = {
     'activity': 'birdhouse project',
     'software':'flyingpigeon v 0.1', 
     'software_project': 'birdhouse',
     'software_reference':'https://github.com/bird-house/',
     'software_platform': 'PYTHON %s' % py_version,
     'contact_mail_1':'ehbrecht@dkrz.de',
     'contact_mail_2':'nils.hempelmann@lsce.ipsl.fr',
     'creation_date': creation_date ,
     }
  
  ds = Dataset(resource, mode='a')
  ds.setncatts(md_basic)
  ds.close()
  
  return(resource)

def set_dynamic_md(resource):
  """
  Dynamic meta data like time frequency, spatial extend, start end time etc.
  :param resource: netCDF file where basic meta data should be set
  """
  from flyingpigeon.utils import get_timerange, get_time
  frequency = get_frequency(resource)
  
  time_coverage_start, time_coverage_end = get_timerange(resource)
  time_number_steps = len(get_time(resource))
  
  # max_lat, min_lat, max_lon, min_lat = get_extent(resource)
  
  ds = Dataset(resource, mode='a')
  
  try:
    driving_experiment = ds.driving_experiment
    ds.delncattr('driving_experiment')
  except Exception as e: 
    logger.error(e)
    driving_experiment = ''

  try:
    driving_experiment_name = ds.driving_experiment_name
    ds.delncattr('driving_experiment_name')
  except Exception as e: 
    logger.error(e)
    driving_experiment_name = ''

  try:
    driving_model_ensemble_member = ds.driving_model_ensemble_member
    ds.delncattr('driving_model_ensemble_member')
  except Exception as e: 
    logger.error(e)
    driving_model_ensemble_member = ''    
    
  try:
    experiment = ds.experiment
    ds.delncattr('experiment')
  except Exception as e: 
    logger.error(e)
    experiment = ''
  try:
    tracking_id = ds.tracking_id
    ds.delncattr('tracking_id')
  except Exception as e: 
    logger.error(e)
    tracking_id = ''    
    
  try:
    experiment_id = ds.experiment_id
    ds.delncattr('experiment_id')
  except Exception as e: 
    logger.error(e)
    experiment_id = ''
  
  try:
    project_id = ds.project_id
    ds.delncattr('project_id')
  except Exception as e: 
    logger.error(e)
    project_id = ''
    
  try:
    institution_id = ds.institution_id
    ds.delncattr('institution_id')
  except Exception as e: 
    logger.error(e)
    institution_id = ''
 
  try:
    model_version_id = ds.model_version_id
    ds.delncattr('model_version_id')
  except Exception as e: 
    logger.error(e)
    model_version_id = ''
    
  try:
    driving_model_id = ds.driving_model_id
    ds.delncattr('driving_model_id')
  except Exception as e: 
    logger.error(e)
    driving_model_id = ''

  try:
    driving_ensemble_member = ds.driving_ensemble_member
    ds.delncattr('driving_ensemble_member')
  except Exception as e: 
    logger.error(e)
    driving_ensemble_member = '' 
    
  try:
    driving_model_id = ds.driving_model_id
    ds.delncattr('driving_model_id')
  except Exception as e: 
    logger.error(e)
    driving_model_id = ''
  
  try:
    model_id = ds.model_id
    ds.delncattr('model_id')
  except Exception as e: 
    logger.error(e)
    driving_model_id =''
    
    
  try:
    contact = ds.contact
    ds.delncattr('contact')
  except Exception as e: 
    logger.error(e)
    contact = ''
  try:
    driving_experiment_id = ds.driving_experiment_id
    ds.delncattr('driving_experiment_id')
  except Exception as e: 
    logger.error(e)
    driving_experiment_id = ''
    
  try:
    domain = ds.CORDEX_domain
  except Exception as e: 
    logger.error(e)
    domain = ''
  ds.close()
  
  min_lat, max_lat, min_lon, max_lon = get_extent(resource)
  geospatial_increment = get_geospatial_increment(resource)
  
  try: 
    md_dynamic = {
      'in_var_driving_experiment' :driving_experiment,
      'in_var_driving_experiment_name': driving_experiment_name,
      'in_var_driving_model_ensemble_member' : driving_model_ensemble_member,
      'in_var_experiment': experiment,
      'in_var_experiment_id': experiment_id,    
      'in_var_project_id': project_id,
      'in_var_contact': contact,
      'in_var_institution_id':institution_id,  
      'in_var_model_version_id': model_version_id, 
      'in_var_driving_model_id': driving_model_id,
      'in_var_model_id': model_id,
      'in_var_driving_ensemble_member':driving_ensemble_member, 
      'in_var_driving_experiment_id': driving_experiment_id, 
      'in_var_domain': domain, 
      'in_var_tracking_id' : tracking_id,
      'frequency': frequency,
      'time_coverage_start': time_coverage_start,
      'time_coverage_end':time_coverage_end,
      'time_number_steps':time_number_steps,
      #'time_number_gaps': '',
      #'cdm_datatype':'' ,
      'domain':'%s_subset' % domain ,
      'geospatial_increment': geospatial_increment,
      'geospatial_lat_min':min_lat ,
      'geospatial_lat_max':max_lat ,
      'geospatial_lon_min':min_lon ,
      'geospatial_lon_max':max_lon ,
      }
    
    #:product = "output" ;
    #:rcm_version_id = "v1" ;
    #:references = "http://www.smhi.se/en/Research/Research-departments/climate-research-rossby-centre" ;
                
    
  except Exception as e: 
    logger.error('failed to populate dynamic metadata dictionay')
    
  try:
    ds = Dataset(resource, mode='a')
    ds.setncatts(md_dynamic)
    ds.close()
  except Exception as e:
    logger.error(e)
    
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
  if 'lat' in ds.variables:
    lats = ds.variables['lat']
    lons = ds.variables['lon']
    ds.close()
  else: 
    ds.close()
    from flyingpigeon import utils 
    lats, lons = utils.unrotate_pole(resource)

  min_lat = lats[:].min() 
  max_lat = lats[:].max()
  min_lon = lons[:].min() 
  max_lon = lons[:].max()
  
  return min_lat, max_lat, min_lon, max_lon

def get_geospatial_increment(resource):
  ds = Dataset(resource)
  if 'rlat' in ds.variables: 
    x = ds.variables['rlon']
  if 'x'  in ds.variables: 
    x = ds.variables['x']
  
  geospatial_increment = round((x[1] - x[0]),2)
  ds.close()
  return geospatial_increment 


def set_metadata_segetalflora(resource):
  """
  :param resources: imput files 
  """
  # gather the set_metadata
  
  dic_segetalflora = {
    'keywords' : 'Segetalflora', 
    'tier': '2',
    'in_var' : 'tas',
    'description':'Number of European segetalflora species', 
    'method':'regression equation',
    'institution':'Julius Kuehn-Institut (JKI) Federal Research Centre for Cultivated Plants', 
    'institution_url':'www.jki.bund.de',
    'institute_id' : "JKI",
    'contact_mail_3':'Joerg.Hoffmann@jki.bund.de',
    'version' : '1.0',
     }
  
  dic_climatetype = {
    '1' : 'cold northern species group', 
    '2' : 'warm northern species group',
    '3' : 'moderate warm-toned species group',
    '4' : 'moderate warm-toned to mediterranean species group',
    '5' : 'mediterranean species group',
    '6' : 'climate indiffernet species',
    '7' : 'climate undefinable species',
    'all' : 'species of all climate types'
      }
  
  try:
    set_basic_md(resource)
  except Exception as e: 
    logger.error(e)
  
  try:
    set_dynamic_md(resource)
  except Exception as e: 
    logger.error(e)
  
  #set the segetalflora specific metadata
  try:
    ds = Dataset(resource, mode='a')
    ds.setncatts(dic_segetalflora)
    ds.close()
  except Exception as e: 
    logger.error(e)
    # set the variable attributes: 
  from flyingpigeon.utils import get_variable
  
  try:
    ds = Dataset(resource, mode='a')
    var = get_variable(resource)
    if 'all' in var: 
      climat_type = 'all'
    else: 
      climat_type = var[-1]

    culture_type = var.strip('sf').strip(climat_type)  
    
    sf = ds.variables[var]
    sf.setncattr('units',1)
    sf.setncattr('standard_name', 'sf%s%s' % (culture_type, climat_type))  
    sf.setncattr('long_name', 'Segetal flora %s land use for %s' % (culture_type, dic_climatetype['%s' % climat_type]))
    ds.close()
  except Exception as e: 
    logger.error('failed to set sf attributes %s ' % e)
  # sort the attributes: 
  try:
    ds = Dataset(resource, mode='a')
    att = ds.ncattrs()
    att.sort()
    for a in att: 
      entry = ds.getncattr(a)
      ds.setncattr(a,entry)
    history = '%s , Segetalflora Impact Model V1.0' % (ds.history) 
    ds.setncattr('history',history)
    ds.close()
  except Exception as e: 
    logger.error('failed to sort attributes %s ' % e)
  
  return resource