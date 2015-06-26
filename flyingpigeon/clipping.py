import ocgis

from .exceptions import CalculationException
from .utils import drs_filename, calc_grouping

from malleefowl import wpslogging as logging
#import logging

logger = logging.getLogger(__name__)
COUNTRY_SHP = '50m_country' # 'world_countries_boundary_file_world_2002'

REGION_EUROPE = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA',
                 'GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
                 'POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE',
                 'SRB','MDA','UKR','BIH','ALB','BLR','KOS']

from os.path import dirname, join

ocgis.env.DIR_SHPCABINET = join(dirname(__file__), 'processes', 'shapefiles')

def select_ugid(region):
    """
    returns geometry id of given region in country shapefile.
    """
    from ocgis.util.shp_cabinet import ShpCabinetIterator
    sc_iter = ShpCabinetIterator(COUNTRY_SHP)
    result = []
    for row in sc_iter:
        if row['properties']['adm0_a3'] == region:
            result.append(row['properties']['UGID'])
    return result

def ugid_EUR(region):
    """
    returns geometry country id of given region in Europe shapefile.
    """
    from ocgis.util.shp_cabinet import ShpCabinetIterator
    sc_iter = ShpCabinetIterator('50m_country')
    result = []
    for row in sc_iter:
        if row['properties']['adm0_a3'] == region:
            result.append(row['properties']['UGID'])
    return result

def calc_region_clipping(resource, region='AUT', out_dir=None , variable=None):
    """
    calculates geometry clipping of netcdf files and given region.

    :param resource: netcdf filename
    :param out_dir: output directory for result file (netcdf)
    :param output_format: format of result file (nc or csv)

    :return: netcdf file for region
    """
  
    # preparing the working directory 
    #ocgis.env.OVERWRITE = True
    #ocgis.env.DIR_DATA = out_dir

    #from ocgis.interface.base.crs import CFWGS84
    #rd = ocgis.RequestDataset(resources, variable)
    
    
    rd = ocgis.RequestDataset(resource, variable=variable)

    filename = drs_filename(resource, variable=variable)
    filename = filename.replace("EUR", region)
    prefix = filename.replace(".nc", "")
    
    try:
        logger.debug('calculation of region %s with variable %s' % (region, rd.variable))
        output = ocgis.OcgOperations(
            dataset=rd,
            geom=COUNTRY_SHP,
            #output_crs=None,
            output_format="nc",
            select_ugid=select_ugid(region),
            prefix=prefix,
            dir_output=out_dir,
            add_auxiliary_files=False ).execute()
    except Exception as e:
        msg = 'processing failed for file prefix=%s : %s' % (prefix, e)
        logger.exception(msg)
        raise CalculationException(msg)

    return output

def normalize(resource, grouping='year', region='AUT', start_date="1971-01-01", end_date="2010-12-31", out_dir=None):
    """
    noramlize netcdf file for region and timeperiod

    :param resource: netcdf filename
    :param start_date: string with start date
    :param end_date: string with end date
    :param out_dir: output directory for result file (netcdf)

    :return: normalized netcdf file
    """
    rd = ocgis.RequestDataset(resource)
    variable = rd.variable
    from dateutil import parser as date_parser
    time_range=[ date_parser.parse(start_date) , date_parser.parse(end_date) ]
    calc = [{'func':'mean','name':'ref_' + variable }] 

    filename = drs_filename(resource)
    filename = filename.replace("EUR", region)
    from os.path import join
    output = join(out_dir, filename)
    prefix = "ref_%s" % filename
    prefix = prefix.replace('.nc', '')
    
    try: 
        reference = ocgis.OcgOperations(
            dataset=rd,
            geom=COUNTRY_SHP,
            dir_output=out_dir,
            output_format="nc",
            select_ugid=select_ugid(region),
            prefix=prefix,
            add_auxiliary_files=False,
            calc=calc,
            calc_grouping=calc_grouping(grouping),
            time_range=time_range  ).execute()

        from tempfile import mkstemp
        from cdo import Cdo   
        cdo = Cdo()
        _,out_resource = mkstemp(prefix="out_resource_", dir=out_dir)
        cdo.fldmean(input = resource , output = out_resource)
        _,out_ref = mkstemp(prefix="out_ref_", dir=out_dir)
        cdo.fldmean(input = reference , output = out_ref )
        cdo.sub(input = "%s %s" % (out_resource, out_ref) , output = output)
    except:
        msg = 'normalize failed for file : %s ' % filename
        logger.exception(msg)
        raise CalculationException(msg)
    return output

  

def clip_continent(urls, variable, dimension_map,  calc=None,  calc_grouping= None, prefix=None, 
                   continent='Europe', output_format='nc', dir_output='.'):
  """ returns clipped netCDF file 
  
  possible continent entries: 
  'Africa', 'Asia', 'Australia',
  'North America', 'Oceania', 
  'South America', 'Antarctica', 'Europe'"""
  
  from ocgis.util.shp_cabinet import ShpCabinetIterator
  
  sci = ShpCabinetIterator('continent')
  ugids = []
  geom = 'continent'
  
  #try : 
    #for row in sci:
      ##for continent in continents: 
      #if row['properties']['CONTINENT'] == 'Europe':
          #ugids.append(row['properties']['UGID'])
          #print 'UGID %s' (ugids)
  #except Exception as e:
      #msg = 'selection of continent failed'
      #logger.exception(msg)
      #raise CalculationException(msg, e)
  ugids = [8]     
  try:    
    rd = ocgis.RequestDataset(urls)
    ocgis.env.DIR_OUTPUT = dir_output
    ocgis.env.PREFIX = prefix    
    geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping,
                                    output_format=output_format, select_ugid=ugids, geom=geom, add_auxiliary_files=False).execute()
  except Exception as e:
      msg = 'ocgis calculations failed '
      logger.exception(msg)
      raise CalculationException(msg, e)
  return  geom_file

def clip_counties_EUR(urls,
                      variable=None,
                      dimension_map=None,
                      calc=None, 
                      calc_grouping= None,
                      prefix=None,
                      country='FRA',
                      output_format='nc',
                      dir_output=None):
  """ returns clipped netCDF file. 
  
  :param urls: str of input url netCDF file
  :param varible: variable to be clipped in netCDF file (see: utils.get_variable)
  :param dimension_map: required dimension_map (see: utils.get_dimensionmap)
  :param calc: parameter for calculation options (see: ocgis Documentation)
  :param calc_grouping: ocgis parameter for aggregation 
  
  :param country: Abreviation for EUR countries (default='FRA') possible values
      'AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA',
      'GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD',
      'POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE',
      'SRB','MDA','UKR','BIH','ALB','BLR','KOS'"""
    
  from ocgis.util.shp_cabinet import ShpCabinetIterator
  from flyingpigeon import utils
  from os.path import basename
  
  ocgis.env.OVERWRITE = True
  sci = ShpCabinetIterator('50m_country')
  geoms = '50m_country'
  
  #if urls == str: 
    #urls = list([urls])
  if variable == None: 
    variable = basename(urls).split('_')[0]
    
  try : 
    ugid = ugid_EUR(country)
  except Exception as e:
    msg = 'selection of continent failed'
    logger.exception(msg)
    raise CalculationException(msg, e)
  try:
    rd = ocgis.RequestDataset(urls, variable=variable, dimension_map=dimension_map)
    ocgis.env.DIR_OUTPUT = dir_output
    ocgis.env.PREFIX = prefix
    geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping, 
                                    output_format=output_format, select_ugid=ugid, geom=geoms,
                                    add_auxiliary_files=False).execute()
  except Exception as e:
      msg = 'ocgis calculations failed dimension_map = %s' % (dimension_map)
      logger.exception(msg)
      raise CalculationException(msg, e)
  return  geom_file
        
        
  
  
  
  
  
