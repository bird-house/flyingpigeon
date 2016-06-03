from cdo import Cdo
from tempfile import mkstemp
import os

from flyingpigeon.utils import drs_filename, get_variable, calc_grouping , sort_by_filename
from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

DIR_MASKS = config.masks_dir()
DIR_SHP = config.shapefiles_dir()

def countries():
    """
    :return: a list of all countries codes.
    """
    countries = _COUNTRIES_.keys()
    countries.sort()
    return countries  

def countries_longname():
    """
    :return: the longname of all countries.
    """
    longname = ''
    for country in countries(): 
      longname = longname + "%s : %s \n" % (country, _COUNTRIES_[country]['longname'])
    return longname

def masking(resource, mask, prefix=None, dir_output=None):
  """
  Returns a list of masked netCDF file(s) path(es).  
  :param resource: string path to netCDF resource
  :param mask: predifined mask ('EUR-11', 'EUR-44')
  :param prefix:  prefix for filename. If prefix is not set, a filename will be created
  :param dir_output: directory for output file. If dir_output is not set, a tempdir will be created
  """
  cdo = Cdo()
  
  if dir_output == None: 
    dir_output = os.curdir
  nc_mask = os.path.join(DIR_MASKS, mask + '.nc')
  
  if prefix == None: 
    p1 , resource_masked = mkstemp(dir = dir_output, suffix='.nc')
  else: 
    resource_masked = os.path.join(dir_output, prefix + '.nc')
# try:
  call = "cdo div '%s' '%s' '%s'" % ( resource , nc_mask , resource_masked)
  os.system(call)
  return resource_masked


def clipping(resource=[], variable=None, dimension_map=None, calc=None,  
  calc_grouping= None, historical_concatination=True, prefix=None, spatial_wrapping='wrap', polygons=None, mosaik=False, dir_output=None):
  """ returns list of clipped netCDF files
  possible entries: 
  :param resource: list of input netCDF files
  :param variable: variable (string) to be used in netCDF
  :param dimesion_map: specify a dimension map input netCDF has unconventional dimension
  :param calc: ocgis calculation argument
  :param calc_grouping: ocgis calculation grouping 
  :param historical_concatination: concat files of RCPs with appropriate historical runs to one timeseries 
  :param prefix: perfix for output file name
  :param polygons: list of polygons to be used. if more than 1 in the list, a appropriate mosaik will be clipped
  :param output_format: output_format (default='nc')
  :param dir_output: specify a output location
  """
  
  from flyingpigeon.utils import get_variable, drs_filename
  from flyingpigeon.ocgis_module import call
  
  if type(resource) != list: 
    resource = list([resource])
  if type(polygons) != list:
    polygons = list([polygons])
  if prefix != None:
    if type(prefix) != list:
      prefix = list([prefix])
  
  geoms = set()
  ncs = sort_by_filename(resource, historical_concatination=historical_concatination) #  historical_concatination=True
  geom_files = []

  if mosaik == True :
    try:
      nameadd = '_'
      for polygon in polygons: 
        geoms.add(get_geom(polygon))
        nameadd = nameadd + polygon 
      if len(geoms) > 1: 
        logger.debug('polygons belong to differnt shapefiles! mosaik is not possible %s', geoms)
      else: 
        geom = geoms.pop()
      ugids = get_ugid(polygons=polygons, geom=geom)
    except Exception as e:
      logger.debug('geom identification failed %s ' % e)
  
    for key in  ncs.keys() :
      if variable == None:
        variable = get_variable(ncs[key])
        logger.info('variable %s detected in resource' % (variable))  
      try:
        
        if prefix == None:
          name = key + nameadd
        else:
          name = prefix[0]
          
        geom_file = call(resource=ncs[key], variable=variable, 
                          calc=calc, calc_grouping=calc_grouping ,
          prefix=name, geom=geom, select_ugid=ugids,spatial_wrapping=spatial_wrapping,
          dir_output=dir_output, dimension_map=dimension_map)    
        
        geom_files.append( geom_file )
      except Exception as e:
        msg = 'ocgis calculations failed for %s ' % (key)
        logger.debug(msg)
        
  else: 
    try:
      for i, polygon in enumerate(polygons): 
        geom = get_geom(polygon)
        ugid = get_ugid(polygons=polygon, geom=geom)
        for key in  ncs.keys() :
          if variable == None:
            variable = get_variable(ncs[key])
            logger.info('variable %s detected in resource' % (variable))  
          try:
            if prefix == None: 
              name = key + '_' + polygon
            else:
              name = prefix[i]
            geom_file = call(resource=ncs[key], variable=variable,  calc=calc, calc_grouping=calc_grouping,
              prefix=name, geom=geom, select_ugid=ugid, dir_output=dir_output, dimension_map=dimension_map, spatial_wrapping=spatial_wrapping)
            geom_files.append( geom_file )
          except Exception as e:
            msg = 'ocgis calculations failed for %s ' % (key)
            logger.debug(msg)
            raise
    except Exception as e:
        logger.debug('geom identification failed')
        raise 

  return  geom_files


def get_dimension_map(resource): 
  """ returns the dimension map for a file, required for ocgis processing. 
  file must have a DRS conform filename (see: utils.drs_filename())
  
  :param resource: str input file path
  """

  file_name = os.path.basename(resource)
  
  dim_map1 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0}}
  
  #dim_map2 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              #'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              #'T': {'variable': 'time', 'dimension': 'time', 'pos': 0, 'bounds': 'time_bnds'}}
  
  dim_map3 = {'X': {'variable': 'rlon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'rlat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  dim_map4 = {'X': {'variable': 'Actual_longitude', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'Actual_latitude', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  dim_map5 = {'X': {'variable': 'x', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'y', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}
  
  if 'CM5A-MR_WRF331F' in file_name: 
    dimension_map = dim_map1
  elif 'CNRM-CM5_CNRM-ALADIN53' in file_name: 
    dimension_map = dim_map1
  elif 'MPI-ESM-LR_REMO019' in file_name: 
    dimension_map = dim_map1
  elif 'CLMcom-CCLM4-8-17' in file_name:
    dimension_map = dim_map1
  elif '_v11.0' in file_name: # EOBS Data
    dimension_map = dim_map4
  #elif 'EOBS' in file_name:   
    #dimension_map = dim_map5
  else:     
    dimension_map = None
    
  return dimension_map
  

def get_shp_column_values(geom, columnname): 
  """ returns a list of all entries the shapefile columnname
  :param geom: name of the shapefile
  :param columnname: Column name to be intereted
  """
  from ocgis import env, ShpCabinetIterator
  #import ocgis
  
  env.DIR_SHPCABINET = DIR_SHP
  sci = ShpCabinetIterator(geom)
  
  vals = []
  for row in sci: 
    vals.append(row['properties'][columnname])
  return vals


# === Functions for Clipping:
def get_ugid(polygons=None, geom=None):
    """
    returns geometry id of given polygon in a given shapefile.
    :param polygons: string or list of the region polygons 
    :param geom: available shapefile possible entries: '50m_country', 'NUTS2'
    """
    from ocgis import env, ShpCabinetIterator
    #from ocgis import env
    
    if polygons == None: 
      result = None
    else: 
      if type(polygons) != list:
        polygons = list([polygons])
      
      env.DIR_SHPCABINET = DIR_SHP
      sc_iter = ShpCabinetIterator(geom)
      result = []
      
      if geom == 'countries':
        for row in sc_iter:
          for polygon in polygons:
            if row['properties']['ADM0_A3'] == polygon:
              result.append(row['properties']['UGID'])
                
      elif geom == 'extremoscope':
        for row in sc_iter:
          for polygon in polygons:
            if row['properties']['HASC_1'] == polygon:
              result.append(row['properties']['UGID'])
                
      elif geom == 'continents':
        for row in sc_iter:
          for polygon in polygons:
            if row['properties']['CONTINENT'] == polygon:
              result.append(row['properties']['UGID'])    
      else:
        from ocgis import ShpCabinet
        sc = ShpCabinet(DIR_SHP)
        logger.debug('geom: %s not found in ShapeCabinet. Available geoms are: %s ', geom, sc)
    return result

def get_geom(polygon=None):
  """ retuns the approriate shapefile (geom) for one given polygon abbreviation
  
  :param polygon: polygon shortname"""
  
  if polygon == None:
    geom = None
  else:
    if polygon in _COUNTRIES_: # (polygon) == 3:
      geom = 'countries'
    elif polygon in _POLYGONS_EXTREMOSCOPE_: #len(polygon) == 5 and polygon[2] == '.': 
      geom = 'extremoscope'
    elif polygon in _EUREGIONS_:
      geom = 'extremoscope'
    elif polygon in _CONTINENTS_: 
      geom = 'continents'
    else:
      logger.debug('polygon: %s not found in geoms' % polygon) 

  return geom  

# === Available Polygons
_CONTINENTS_ = get_shp_column_values(geom='continents', columnname='CONTINENT') 

_COUNTRIES_ = {}
_COUNTRIES_Europe_ = {}

# === popultate polygons dictionaties 
ADM0_A3 = get_shp_column_values(geom='countries', columnname='ADM0_A3')
NAMELONG = get_shp_column_values(geom='countries', columnname='NAME_LONG')
CONTINENT = get_shp_column_values(geom='countries', columnname='CONTINENT')

for c , key in enumerate(ADM0_A3): 
  _COUNTRIES_[key] = dict(longname=NAMELONG[c])

for c , key in enumerate(ADM0_A3):
  if CONTINENT[c] == 'Europe':
    _COUNTRIES_Europe_[key] = dict(longname=NAMELONG[c])

_EUREGIONS_ = {}

HASC_1 = get_shp_column_values(geom='extremoscope', columnname='HASC_1')
NAME_1 = get_shp_column_values(geom='extremoscope', columnname='NAME_1')

_POLYGONS_EXTREMOSCOPE_ = HASC_1

for c , key in enumerate(HASC_1): 
  _EUREGIONS_[key] = dict(longname=NAME_1[c])
