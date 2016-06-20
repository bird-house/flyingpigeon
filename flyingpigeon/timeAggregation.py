import logging
logger = logging.getLogger(__name__)

from flyingpigeon.utils import calc_grouping, sort_by_filename, get_frequency # aggregations,
from flyingpigeon.ocgis_module import call

def aggregatTime(resource=[], variable=None, frequency=None, prefix=None, grouping='mon', calculation='mean', historical_concatination=True):
  """
  Aggregates over the time axis. 

  :param resource: input netCDF files
  :param variable: variable to be used from resource 
  :param frequency: time frequency in resource
  :param grouping: time aggregation for output
  :param prefix: file name prefix
  :param calculation: calculation methode (default = mean )
  :param historical_concatination: if rcps and appropriate historical runs are present thy are concatinated 
  :return: path to netCDF file
  """ 
  try: 
    ncs = sort_by_filename(resource, historical_concatination=historical_concatination)
    group = calc_grouping(grouping=grouping)
  except Exception as e: 
    logger.exception('failed to determine ncs or calc_grouping')
    raise  
  
  if len(ncs.keys())!= 1: 
    logger.exception('None or more than one data experiments found in resource')
    raise Exception('None or more than one data experiments found in resource') 

  for key in ncs.keys()[0:1]:
    try:
      if frequency == None: 
        frequency = get_frequency(ncs[key][0])
      if variable == None: 
        variable = get_variable(ncs[key][0])

      meta_attrs = { 'field': {'frequency': grouping}}# 'variable': {'new_attribute': 5, 'hello': 'attribute'},
      calc = [{'func' : calculation , 'name' : variable, 'meta_attrs': meta_attrs}] 
      logger.info('calculation:  %s ' % (calc))
      if prefix == None:
        prefix = key.replace(frequency,grouping)
      
      logger.info('prefix:  %s ' % (prefix))
      output = call(resource=ncs[key], variable=None, 
      calc=calc, calc_grouping=group,
      prefix=prefix )
      logger.info('time aggregation done for %s '% (key))
    except Exception as e: 
      logger.exception('time aggregation failed for %s' % key)
      raise

  return output #  key # output
