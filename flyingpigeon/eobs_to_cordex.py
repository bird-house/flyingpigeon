from .exceptions import CalculationException

from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

import datetime as dt
import tempfile

EOBS_VARIABLES = ['tn', 'tx' , 'tn' , 'rr'] #, 'pp'

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
      'CORDEX_domain' : "EOBS-22" ,
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
  
    
def set_attributes(resource):
  from netCDF4 import Dataset
  ds = Dataset(resource, 'w')
  ds.setncatts(att_dict)
  ds.close()  
  return resource
  
def convert(url, variable):
  import ocgis
  from os import rename
  from flyingpigeon import utils
  
  ocgis.env.OVERWRITE=True
  
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
  
  start = 1950
  end = 2014
  
  EOBS_files = []
  
  for year in range(start,end+1, 5):
    time_region = {'year':range(year,year+5)}
    if variable != 'rr':
      rd = ocgis.RequestDataset(url,
                                variable,
                                conform_units_to=unit,
                                time_region = time_region 
                                )
      nc = ocgis.OcgOperations(dataset= rd,
                              output_format='nc',
                              dir_output= '.',
                              add_auxiliary_files=False
                              ).execute()  
    else:
      rd = ocgis.RequestDataset(url,
                                variable,
                              # conform_units_to=unit,
                                time_region = time_region # {'year':['%s:%s' % (year,year+4)]}
                                )
      calc = 'pr=rr/84600'
      nc = ocgis.OcgOperations(dataset= rd,
                              calc=calc,
                              output_format='nc',
                              dir_output= '.',
                              add_auxiliary_files=False
                              ).execute()
      
    EOBS_file = '%s_%s_%s_%s_%s_%s_%s_%s_%s-%s.nc' % (var, 
                                          att_dict['CORDEX_domain'],
                                          att_dict['driving_model_id'],
                                          att_dict['experiment_id'],
                                          att_dict['driving_model_ensemble_member'],
                                          att_dict['model_id'],
                                          att_dict['rcm_version_id'],
                                          att_dict['frequency'], 
                                            year,
                                            year+4)
    
    rename(nc, EOBS_file)
    EOBS_files.append(EOBS_file)
     
  return EOBS_files
  
  
  
  
  