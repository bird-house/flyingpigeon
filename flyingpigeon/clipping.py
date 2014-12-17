import ocgis

from .exceptions import CalculationException

from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

COUNTRY_SHP = '50m_country' # 'world_countries_boundary_file_world_2002'
REGION_EUROPE = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS']

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

def calc_region_clipping(resource, region='AUT', out_dir=None):
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
    rd = ocgis.RequestDataset(resource)
    from flyingpigeon.utils import drs_filename

    filename = drs_filename(resource)
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
    except:
        msg = 'processing failed for file prefix=%s' % prefix
        logger.exception(msg)
        raise CalculationException(msg)

    return output

def normalize(resource, region='AUT', start_date="1971-01-01", end_date="2010-12-31", out_dir=None):
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
    calc_grouping = ['month']

    prefix = "ref_"
    from flyingpigeon.utils import drs_filename
    filename = drs_filename(resource)
    filename = filename.replace("EUR", region)
    from os.path import join
    output = join(out_dir, filename)
    
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
            calc_grouping=calc_grouping ,
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
        msg = 'normalized fieldmean failed for file : %s ' % filename
        logger.exception(msg)
        raise CalculationException(msg)
    return output

  
  
  
  
