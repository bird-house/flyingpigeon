from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from os.path import join, abspath, dirname, getsize


DIR_SHP = join(abspath(dirname(__file__)), 'processes', 'shapefiles')


def call(resource=[], variable=None, dimension_map=None, calc=None,  
  calc_grouping= None, prefix=None, geom=None, select_ugid=None, dir_output=None, output_format='nc'): 
  logger.info('Start ocgis module call function')
  from ocgis import OcgOperations, RequestDataset , env
  from ocgis.util.large_array import compute
  from numpy import sqrt

  # prepare the environment 
  env.DIR_SHPCABINET = DIR_SHP
  env.OVERWRITE = True
  env.DIR_OUTPUT = dir_output

  if type(resource) != list: 
    resource = list([resource])

  # check memory load 
  limit_memory_mb = 475.0 # to reduce the load of the memory, calculation is perfored in chunks
  fsize = 0 
  for nc in resource: 
    fsize = fsize + getsize(nc)
  
  # execute ocgis 
  logger.info('Execute ocgis module call function')
  rd = RequestDataset(resource, variable=variable, dimension_map=dimension_map)
  env.PREFIX = prefix    
  ops = OcgOperations(dataset=rd, 
        calc=calc, 
        calc_grouping=calc_grouping,
        output_format=output_format, # 'nc' is necessary for chunked execution  
        select_ugid=select_ugid, 
        geom=geom, 
        add_auxiliary_files=False)

  if fsize / 1000000 <= 500:          
    geom_file = ops.execute()

  else: 
    # calcultion of chunk size
    size = ops.get_base_request_size()
    nb_time_coordinates_rd = size['variables'][variable]['temporal']['shape'][0]
    element_in_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
    element_in_mb = element_in_kb*0.001
    tile_dim = sqrt(limit_memory_mb/(element_in_mb*nb_time_coordinates_rd))
    geom_file = compute(ops, tile_dimension=int(tile_dim), verbose=True)        
  
  logger.info(' Succeeded with ocgis module call function ')
  return geom_file