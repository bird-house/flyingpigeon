# import ocgis

from .exceptions import CalculationException
from .utils import drs_filename, calc_grouping

from malleefowl import wpslogging as logging
#import logging

logger = logging.getLogger(__name__)

from os.path import dirname, join
DIR_MASKS = join(dirname(__file__), 'processes', 'masks')

def masking(resource , mask):
  from cdo import Cdo
  from tempfile import mkstemp
  
  cdo = Cdo()
  grid = cdo.griddes(input = resource)
  
  gt, gridtxt= mkstemp(suffix='.txt')
  with open(gridtxt, 'w') as fp:
    for row in grid:
      fp.write('%s \n'% (row))
      
  gt, nc_remap= mkstemp(suffix='.nc')    
  cdo.remapnn('%s'% (gridtxt), input = resource, output = nc_remap )     
  return nc_remap
  