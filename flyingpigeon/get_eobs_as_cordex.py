import datetime as dt
import tempfile

from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

EOBS_VARIABLES = ['tg', 'tx' , 'tn', 'rr'] #, 'pp'

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
      'rcm_version_id' : "v12" ,
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
  try:     
    #http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/rr_0.22deg_rot_v12.0.nc
    #http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/rr_0.22deg_rot_v12.0.nc.html
    url = 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/%s_0.22deg_rot_v12.0.nc' % (variable)
    logger.info('URL: %s', url)
  except Exception as e: 
    logger.error('could not create EOBS url for variable %s', variable)
  return url  
    
def set_attributes(resource, variable):
  from netCDF4 import Dataset
  if variable == 'tg':
    new_variable = 'tas'
  elif variable == 'tn':
    new_variable = 'tasmin'
  elif variable == 'tx':
    new_variable = 'tasmax'
  elif variable == 'rr':
    new_variable = 'pr'
       
  try : 
    ds = Dataset(resource, 'a')
    ds.renameDimension('Actual_latitude', 'latitude' )
    ds.renameDimension('Actual_longitude', 'longitude' )
    ds.renameVariable(variable, new_variable)
    # ds.setncatts(att_dict)
    ds.close()
  except Exception as e: 
    logger.error('could not set attributes in resouce %s', resource)    
    
  return resource
  
def get_data(variable,
             resource = None,  
             polygons=None, 
             dir_output=None, 
             start = 1950,
             end = 2014):
  
  from os import rename, path, makedirs
  from flyingpigeon import utils
  from flyingpigeon import subset as sb
  from flyingpigeon import ocgis_module as om
  
  try: 
   # ocgis.env.OVERWRITE=True
   # ocgis.env.DIR_SHPCABINET = config.shapefiles_dir()
   # geoms = sb.get_geom()
   # sci = ShpCabinetIterator(geoms)
    
    if dir_output != None and path.exists(dir_output) == False: 
      makedirs(dir_output)
    
    if polygons != None:
      geom = sb.get_geom(polygon=polygons[0:1])
      ugid = sb.get_ugid(polygons=polygons, geom= geom)
    else:
      ugid = None
      geom = None
      
    if resource == None:
      resource = get_url(variable)

    dimension_map = {'X': {'variable': 'Actual_longitude', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'Actual_latitude', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

    time_region = {'year': range(start,end+1)} 

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
    prefix = path.split(resource)[1].replace(variable,var).replace('.nc', '')    
    
    logger.info('processing variable %s' % (var))
  except Exception as e: 
    logger.exception('could not set processing environment')
    raise      

  if variable == 'rr':
    try: 
      
      calc = 'rr=rr/84600'
      
      EOBS_file = om.call(resource=resource, variable=variable, memory_limit=450,
                          dimension_map=dimension_map, prefix=prefix, calc=calc,
                          geom=geom, select_ugid=ugid, 
                          dir_output=dir_output, time_region = time_region)
    except Exception as e: 
      logger.error('ocgis failed for rr with url : %s' %(resource))
  else:
    try:
      unit = 'K'
      EOBS_file = om.call(resource=resource, variable=variable, memory_limit=450,
                          dimension_map=dimension_map, conform_units_to=unit , prefix=prefix, 
                          geom=geom, select_ugid=ugid,
                          dir_output=dir_output, time_region = time_region)  
    except Exception as e: 
      logger.exception('ocgis failed for tg, tx or tn')   

  try: 
    if polygons == None:
      domain =  att_dict['CORDEX_domain']
    else: 
      domain = att_dict['CORDEX_domain'].replace('EUR', polygons)
    
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
      
  except Exception as e: 
    logger.exception('attributes not set for : %s' % EOBS_file)
  return path.join(fpath, EOBS_filename)
