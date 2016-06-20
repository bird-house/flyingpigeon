import datetime as dt
import tempfile

from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

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
  try: 
    url = 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/%s_0.22deg_rot_v11.0.nc' % (variable)
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
   # ds.renameDimension('Actual_latitude', 'latitude' )
   # ds.renameDimension('Actual_longitude', 'longitude' )
    ds.renameVariable(variable, new_variable)
    #ds.setncatts(att_dict)
    ds.close()
  except Exception as e: 
    logger.error('could not set attributes in resouce %s', resource)    
    
  return resource

def func_june_july(value, bounds=None):
    months = [6, 7]
    indices = []
    for ii, dt in enumerate(value.flat):
        if dt.month in months:
            if dt.month == 6 and dt.day >= 15:
                indices.append(ii)
            elif dt.month == 7 and dt.day <= 15:
                indices.append(ii)
    return indices
  
def get_data(variable, 
             polygon=None, 
             dir_output=None, 
             start = 1950,
             end = 2014
             ):
  
  import ocgis
  from os import rename, path, makedirs
  from flyingpigeon import utils
  from flyingpigeon import subset as sb
  from flyingpigeon import clipping

  try: 
    ocgis.env.OVERWRITE=True
    ocgis.env.DIR_SHPCABINET = config.shapefiles_dir()
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

  except Exception as e: 
    logger.error('could not set processing environment')      

      
  if variable == 'rr':
    try: 
      rd = ocgis.RequestDataset(url, 
                              variable,
                              dimension_map = dimension_map,
                              time_region = time_region)
    
      calc = 'rr=rr/84600'#
      EOBS_file = ocgis.OcgOperations(dataset=rd, 
                        calc=calc,
                        geom=geoms,
                        select_ugid=ugid, 
                        output_format='nc',
                        dir_output=dir_output,
                        add_auxiliary_files=False,
                       # time_subset_func=func_june_july
                        ).execute()
    except Exception as e: 
      logger.error('ocgis failed for rr with url : %s' %(url))      


  else:
    try:
      unit = 'K'
      rd = ocgis.RequestDataset(url,
                  variable,
                  conform_units_to=unit,
                  dimension_map = dimension_map,
                  time_region = time_region)
      
      calc = [{'func': 'moving_window', 'name': 'ma', 'kwds': {'k': 3, 'operation': 'mean'}}]
      EOBS_file = ocgis.OcgOperations(dataset=rd, 
                        geom=geoms,
                        select_ugid=ugid,
                        output_format='nc',
                        calc=calc,
                        dir_output=dir_output,
                        add_auxiliary_files=False,
                        time_subset_func=func_june_july
                        ).execute()
    except Exception as e: 
      logger.error('ocgis failed for tg, tx or tn with url : %s' %(url))      

  try: 
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
    # set_attributes(EOBS_file, variable)
    rename(EOBS_file, path.join(fpath, EOBS_filename))
      
  except Exception as e: 
    logger.error('attributes not set for : %s' %(EOBS_file))
  return path.join(fpath, EOBS_filename)

def get_data_worker(variable='tg', polygons=['FRA','DEU'], dir_output=None, start = 1950, end = 2014):
  
  def worker(variable, polygon, dir_output, start, end, que):
    try: 
      EOBS_file = get_data(variable=variable, polygon=polygon, 
             dir_output=dir_output, start = start, end = end)
      que.put(EOBS_file)
    except Exception as e: 
      logger.error('get Data module failed for: %s' %(variable))  
    return
  
  from multiprocessing import Process, Queue
  
  #if __name__ == '__main__':
  q = Queue()
  jobs = []
  files = []
  for i, polygon in enumerate(polygons): #for i in range(5):
    p = Process(target=worker, args=(variable, polygon, dir_output,
                                      start, end , q))
    jobs.append(p)
    p.start()
    p.join()
    files.append('%s' %(q.get()))
    
  return files
