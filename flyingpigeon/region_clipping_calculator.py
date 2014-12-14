import ocgis

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

def calc_region_clipping(resources=[], variable='tas', region='AUT', output_format='nc', out_dir=None):
    """
    calculates geometry clipping of netcdf files and given region.

    :param resources: list of netcdf filenames
    :param out_dir: output directory for result file (netcdf)
    :param output_format: format of result file (nc or csv)

    :return: netcdf file for region
    """
  
    # preparing the working directory 
    #ocgis.env.OVERWRITE = True
    #ocgis.env.DIR_DATA = out_dir

    #from ocgis.interface.base.crs import CFWGS84
    rd = ocgis.RequestDataset(resources, variable)
    from flyingpigeon.utils import drs_filename

    prefix = region
    output = None
    filename = drs_filename(resources[0])
    try:
        logger.debug('calculation of polygon %s with variable %s in %s' % (prefix, variable, region))
        output = ocgis.OcgOperations(
            dataset=rd,
            geom=COUNTRY_SHP,
            #output_crs=CFWGS84,
            output_format=output_format,
            select_ugid=select_ugid(region),
            prefix=prefix,
            dir_output=out_dir,
            add_auxiliary_files=False ).execute()
    except:
        logger.exception('processing failed for file prefix=%s', prefix)

    return dict(output=output, drs_filename=filename)
  
  
  
  
