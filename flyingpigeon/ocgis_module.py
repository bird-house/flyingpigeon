from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from os.path import join, abspath, dirname, getsize

DIR_SHP = join(abspath(dirname(__file__)), 'processes', 'shapefiles')

def call(resource=[], variable=None, dimension_map=None, calc=None,  
  calc_grouping= None, conform_units_to=None, prefix=None, 
  geom=None, select_ugid=None, time_region=None,
  dir_output=None, output_format='nc'): 
  logger.info('Start ocgis module call function')
  from ocgis import OcgOperations, RequestDataset , env
  from ocgis.util.large_array import compute
  
  # prepare the environment 
  env.DIR_SHPCABINET = DIR_SHP
  env.OVERWRITE = True
  env.DIR_OUTPUT = dir_output
  env.PREFIX = prefix

  output_format_options={'data_model': 'NETCDF4_CLASSIC'}
  
  if type(resource) != list: 
    resource = list([resource])

  # execute ocgis 
  logger.info('Execute ocgis module call function')
  
  try: 
    rd = RequestDataset(resource, variable=variable, 
      dimension_map=dimension_map, conform_units_to=conform_units_to, 
      time_region=time_region)
    
    ops = OcgOperations(dataset=rd,
        output_format_options=output_format_options,
        calc=calc, 
        calc_grouping=calc_grouping,
        output_format=output_format, # 'nc' is necessary for chunked execution  
        select_ugid=select_ugid, 
        geom=geom,
        add_auxiliary_files=False)
    logger.info('OcgOperations set')
  except Exception as e: 
    logger.error('failed to setup OcgOperations: %s' % (e))  
  
  # check memory load
  from numpy import sqrt 
  from flyingpigeon.utils import FreeMemory
  
  f = FreeMemory()
  mem_kb = f.user_free 
  mem_mb = mem_kb / 1024.

  mem_limit = mem_mb / 2. # set limit to half of the free memory
  if mem_limit >= 1024. * 4: 
    mem_limit = 1024. * 4
    # 475.0 MB for openDAP 
  
  data_kb = ops.get_base_request_size()['total']
  data_mb = data_kb / 1024.

  if variable == None: 
    variable = rd.variable
    logger.info('%s as variable dedected' % (variable))

  #data_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
  logger.info('data_mb  = %s ; memory_limit = %s ' % (data_mb  , mem_limit  ))
  if data_mb <= mem_limit :  # input is smaler than the half of free memory size
    logger.info('ocgis module call as ops.execute()')
    try: 
      geom_file = ops.execute()
    except Exception as e: 
      logger.error('failed to execute ocgis operation: %s' % e)  
  else:
    size = ops.get_base_request_size()
    nb_time_coordinates_rd = size['variables'][variable]['temporal']['shape'][0]
    element_in_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
    element_in_mb = element_in_kb / 1024.

    tile_dim = sqrt(mem_limit/(element_in_mb*nb_time_coordinates_rd)) # maximum chunk size 
    # calcultion of chunk size
    
    try:
      logger.info('tile_dim = : %s' % tile_dim)

      geom_file = compute(ops, tile_dimension=int(50) , verbose=True)
    except Exception as e: 
      logger.error('failed to compute ocgis operation: %s' % e)  
  
  logger.info('Succeeded with ocgis module call function')
  return geom_file