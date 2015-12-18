from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from os.path import join, abspath, dirname, getsize

DIR_SHP = join(abspath(dirname(__file__)), 'processes', 'shapefiles')

def call(resource=[], variable=None, dimension_map=None, calc=None,  
  calc_grouping= None, conform_units_to=None, memory_limit=None,  prefix=None, 
  geom=None, output_format_options=False, select_ugid=None, time_region=None,time_range=None,
  dir_output=None, output_format='nc'):
  '''
  ocgis operation call

  :param resource:
  :param variable: variable in the input file to be picked
  :param dimension_map: dimension map in case of unconventional starage of data 
  :param calc: ocgis calc syntax for calcultion opartion 
  :param calc_grouping: time aggregate grouping 
  :param conform_units_to: 
  :param memory_limit: limit the amout of data to be loaded into the memory at once if None(default) free memory is detected by birdhouse
  :param prefix:
  :param geom: name of shapefile stored in birdhouse shape cabinet
  :param output_format_options: output options for netCDF e.g compression level()
  :param select_ugid: ugid for appropriate poligons 
  :param time_region:
  :param time_range: sequence of two datetime.datetime objects to mark start and end point 
  :param dir_output:
  :param output_format:
  :return: output file path
  '''

  logger.info('Start ocgis module call function')
  from ocgis import OcgOperations, RequestDataset , env
  from ocgis.util.large_array import compute
  
  # prepare the environment 
  env.DIR_SHPCABINET = DIR_SHP
  env.OVERWRITE = True
  env.DIR_OUTPUT = dir_output
  env.PREFIX = prefix

  if output_format_options == False: 
    output_format_options = None
  elif output_format_options == True:
    output_format_options={'data_model': 'NETCDF4_CLASSIC', 
                         'variable_kwargs': {'zlib': True, 'complevel': 5}}
  else:
    logger.info('output_format_options are set to %s ' % ( output_format_options ))
  
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
        #options=options,
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
  
  if memory_limit == None: 
    f = FreeMemory()
    mem_kb = f.user_free 
    mem_mb = mem_kb / 1024.
    mem_limit = mem_mb / 2. # set limit to half of the free memory
  else:
    mem_limit = memory_limit

  if mem_limit >= 1024. * 4: 
    mem_limit = 1024. * 4
    # 475.0 MB for openDAP 
  
  data_kb = ops.get_base_request_size()['total']
  data_mb = data_kb / 1024.

  if variable == None: 
    variable = rd.variable
    logger.info('%s as variable dedected' % (variable))

  #data_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
  logger.info('data_mb  = %s ; memory_limit = %s ' % (data_mb  , mem_limit ))
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
      logger.info('tile_dim = %s; calc = %s ' % (tile_dim, calc))
      if calc == None:
        calc = '%s=%s*1' % (variable, variable)
        logger.info('calc set to = %s ' %  calc)
        ops = OcgOperations(dataset=rd,
          output_format_options=output_format_options,
          calc=calc, 
          output_format=output_format, # 'nc' is necessary for chunked execution  
          select_ugid=select_ugid, 
          geom=geom,
          add_auxiliary_files=False)
      geom_file = compute(ops, tile_dimension=int(tile_dim) , verbose=True)
    except Exception as e: 
      logger.error('failed to compute ocgis operation: %s' % e)  
  
  logger.info('Succeeded with ocgis module call function')
  return geom_file