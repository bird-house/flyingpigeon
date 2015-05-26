# import ocgis

from .exceptions import CalculationException
from .utils import drs_filename, calc_grouping

from malleefowl import wpslogging as logging
#import logging

logger = logging.getLogger(__name__)

from os.path import dirname, join
DIR_MASKS = join(dirname(__file__), 'processes', 'masks')

def masking(resource, mask, prefix=None, dir_output=None):
  """
  Returns a list of masked netCDF file(s) path(es).  
  :param resource: string path to netCDF resource
  :param mask: predifined mask ('EUR-11', 'EUR-44')
  :param prefix:  prefix for filename. If prefix is not set, a filename will be created
  :param dir_output: directory for output file. If dir_output is not set, a tempdir will be created
  """
  from cdo import Cdo
  cdo = Cdo()
  from tempfile import mkstemp
  from os import system, path 
  
  if dir_output == None: 
    dir_output = path.curdir
  nc_mask = path.join(DIR_MASKS, mask + '.nc')
  
  if prefix == None: 
    p1 , resource_masked = mkstemp(dir = dir_output, suffix='.nc')
  else: 
    resource_masked = path.join(dir_output, prefix + '.nc')
# try:
  call = "cdo div '%s' '%s' '%s'" % ( resource , nc_mask , resource_masked)
  system(call)
  
  return resource_masked


# === Functions for Clipping: 

# === Available Polygons
POLYGONS = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA',
                 'GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
                 'POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE',
                 'SRB','MDA','UKR','BIH','ALB','BLR','KOS']