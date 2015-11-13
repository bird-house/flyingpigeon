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
  
  if type(resource) != list: 
    resource = list([resource])

  # execute ocgis 
  logger.info('Execute ocgis module call function')
  
  rd = RequestDataset(resource, variable=variable, 
    dimension_map=dimension_map, conform_units_to=conform_units_to, 
    time_region=time_region)
  
  ops = OcgOperations(dataset=rd, 
        calc=calc, 
        calc_grouping=calc_grouping,
        output_format=output_format, # 'nc' is necessary for chunked execution  
        select_ugid=select_ugid, 
        geom=geom,
        add_auxiliary_files=False)
  
  # check memory load
  from numpy import sqrt 
  from flyingpigeon.utils import FreeMemory
  
  f = FreeMemory()
  mem_kb = f.user_free 
  mem_mb = mem_kb / 1024.

  mem_limit = mem_mb / 2. # set limit to half of the free memory
  
  data_kb = ops.get_base_request_size()['total']
  data_mb = data_kb / 1024.

  # for nc in resource: 
  #   fsize = fsize + getsize(nc) # in bytes
  #   fsize_mb = fsize / 1024. # in MB 
  #   #fsize_gib = fsize / 1024.**3 # in GB 

  #size = ops.get_base_request_size()
  #
  #data_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])

  if data_mb <= mem_limit :  # input is smaler than the half of free memory size
    logger.info('ocgis module call as ops.execute()')
    try: 
      geom_file = ops.execute()
    except Exception as e: 
      logger.error('failed to execute ocgis operation: %s' % e)  

  else: 
    # calcultion of chunk size
    logger.info('ocgis module call as compute(ops) ')
    logger.info('input (MB) = %s ; free_memory (MB): %s ' % (fsize_mb , mem_mb ))

    # 475.0 #MB # to avoid MemoryError the, calculation is perfored in chunks by big data_input 
    try: 
      timesteps_nr = size['variables'][variable]['temporal']['shape'][0]
      tile_dim = sqrt(mem_limit / data_mb * timesteps_nr)

      logger.info('tile_dim %s ' % tile_dim)

      geom_file = compute(ops, tile_dimension=int(25) , verbose=True)
    except Exception as e: 
      logger.error('failed to compute ocgis operation: %s' % e)  
  
  logger.info('Succeeded with ocgis module call function')
  return geom_file
