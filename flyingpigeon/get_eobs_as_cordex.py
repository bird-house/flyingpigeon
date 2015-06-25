from .exceptions import CalculationException

from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

import datetime as dt
import tempfile

EOBS_VARIABLES = ['tn', 'tx' , 'tn', 'rr'] #, 'pp'

att_dict = {
      'Conventions' : "CF-1.4" ,
      'contact' : "beta Version" ,
      'experiment' : "Observation run" ,
      'experiment_id' : "observation" ,
      'realization' : "1" ,
      'driving_experiment' : "EOBS,r1i1p1" ,
      'driving_model_id' : "EOBS" ,
      'driving_model_ensemble_member' : "r1i1p1" ,
      'driving_experiment_name' : "observation" ,
      'institution' : "beta-Version" ,
      'institute_id' : "beta-Version" ,
      'model_id' : "beta-Version" ,
      'rcm_version_id' : "v11.0" ,
      #'references' : "http//www.knmi.nl/research/regional_climate" ,
      'project_id' : "EOBS" ,
      'CORDEX_domain' : "EUR-22" ,
      'product' : "output" ,
      'frequency' : "day" ,
      #'knmi_global_comment' : "" ,
      #'knmi_model_comment' : "RACMO22E baseline physics from ECMWF CY31r1, modifications include HTESSEL CY33r1, patch K-diffusion CY32r3, moist Turbulent Kinetic Energy, satellite inferred Leaf Area Index" ,
      #'knmi_version_comment' : "v1 reference version for Europe and other midlatitude regions" ,
      #'knmi_grib_path' : "mos.knmi.nl/climreg/CXEUR12/eCS6-v441-fECEARTH-mei1/GRIB_data" ,
      'creation_date' : '%s' % dt.datetime.now() ,
      #'tracking_ID' : "0a1da8e9-9e49-4384-b503-bfd488149626",
      }
#def set_varname(resource, varname): 

def get_url(variable):
  url = 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/%s_0.22deg_rot_v11.0.nc' % (variable)
  return url  
    
def set_attributes(resource, variable):
  if variable == 'tg':
    new_variable = 'tas'
  elif variable == 'tn':
    new_variable = 'tasmin'
  elif variable == 'tx':
    new_variable = 'tasmax'
  elif variable == 'rr':
    new_variable = 'pr'
        
  from netCDF4 import Dataset
  ds = Dataset(resource, 'a')
  ds.renameVariable(variable,'tas')
  #ds.setncatts(att_dict)
  ds.close()  
  return resource
  
def get_data(variable, 
             polygon=None, 
             dir_output=None, 
             start = 1950,
             end = 2014):
  
  import ocgis
  from os import rename, path, makedirs
  from flyingpigeon import utils
  from flyingpigeon import subsetting as sb
  from flyingpigeon import clipping
  
  ocgis.env.OVERWRITE=True
  ocgis.env.DIR_SHPCABINET = path.join(path.dirname(__file__), 'processes', 'shapefiles')
  geoms = '50m_country'
  # sci = ShpCabinetIterator(geoms)
  
  if dir_output != None and path.exists(dir_output) == False: 
    makedirs(dir_output)
  
  if polygon != None:
    ugid = clipping.select_ugid(polygon)
  else:
    ugid = None
  
  url = get_url(variable)
  dimension_map = sb.get_dimension_map(url)
  time_region = {'year':range(start,end+1)} 
  
  
  
  if variable == 'tg':
      var = 'tas'
      unit = 'K'
  elif variable == 'tn':
      var = 'tasmin'
      unit = 'K'
  elif variable == 'tx':
      var = 'tasmax'
      unit = 'K'
  elif variable == 'rr':
      var = 'pr'
      unit = 'kg m-2 s-1'
      
  if variable == 'rr':
    rd = ocgis.RequestDataset(url, 
                              variable,
                              dimension_map = dimension_map,
                              time_region = time_region)
    
    calc = 'pr=rr/84600'#
    EOBS_file = ocgis.OcgOperations(dataset=rd, 
                        calc=calc,
                        geom=geoms,
                        select_ugid=ugid, 
                        output_format='nc',
                        dir_output=dir_output,
                        add_auxiliary_files=False
                        ).execute()
    print EOBS_file
  else:
    unit = 'K'
    rd = ocgis.RequestDataset(url,
                  variable,
                  conform_units_to=unit,
                  dimension_map = dimension_map,
                  time_region = time_region)
    
    EOBS_file = ocgis.OcgOperations(dataset=rd, 
                        geom=geoms,
                        select_ugid=ugid,
                        output_format='nc',
                        dir_output=dir_output,
                        add_auxiliary_files=False
                        ).execute()
    
    if polygon == None:
      domain =  att_dict['CORDEX_domain']
    else: 
      domain = att_dict['CORDEX_domain'].replace('EUR', polygon)
    
    EOBS_filename = '%s_%s_%s_%s_%s_%s_%s_%s_%s-%s.nc' % (var, 
                                          domain,
                                          att_dict['driving_model_id'],
                                          att_dict['experiment_id'],
                                          att_dict['driving_model_ensemble_member'],
                                          att_dict['model_id'],
                                          att_dict['rcm_version_id'],
                                          att_dict['frequency'], 
                                          start,
                                          end)
    
    fpath, basename = path.split(EOBS_file)
    set_attributes(EOBS_file, variable)
    rename(EOBS_file, path.join(fpath, EOBS_filename))
    
  return path.join(fpath, EOBS_filename)
  


  
  
  
  