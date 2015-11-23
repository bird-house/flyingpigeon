from .exceptions import CalculationException

from flyingpigeon.utils import calc_grouping, sort_by_filename # aggregations,
from flyingpigeon.ocgis_module import call

from malleefowl import wpslogging as logging

#import logging
logger = logging.getLogger(__name__)


def aggregatTime(resource=[], grouping='mon', calc='mean' historical_concatination=True): 

  ncs = sort_by_filename(resource, historical_concatination=historical_concatination)
  group = calc_grouping(grouping=grouping)

  for i , key in enumerate(ncs.keys()):
    try: 
      if i == 0: 
        prefix = 
        output = call(resource=[], variable=None, dimension_map=None, calc=calc, calc_grouping=group,
        prefix=None, dir_output=None, output_format='nc')
        logger.info('time aggregation done for %s '% (key))
      else: 
        logger.info('No or more than one data experiment found in resource %s '% (key))
    except Exception as e: 
      logger.error('time aggregation failed for %s: %s ' % (key, e))

  return output