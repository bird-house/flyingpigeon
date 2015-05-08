import ocgis
from os.path import dirname, join

#from exceptions import CalculationException

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

#COUNTRY_SHP = '50m_country' # 'world_countries_boundary_file_world_2002'
#ocgis.env.DIR_SHPCABINET = join(dirname(__file__), 'processes', 'shapefiles')


def clip_continent(urls, calc=None,  calc_grouping= None, prefix=None, 
                   continent='Europe', output_format='nc', dir_output='.'):
  
  """ possible continent entries: 
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
    geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping, output_format=output_format, select_ugid=ugids, geom=geom, add_auxiliary_files=False).execute()
  except Exception as e:
      msg = 'ocgis calculations failed '
      logger.exception(msg)
      raise CalculationException(msg, e)
  return  geom_file

def clip_counties_EUR(urls, calc=None, calc_grouping= None, prefix=None,
                   country='FRA', output_format='nc', dir_output=None):
  """ possible continent entries:
  'AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS'"""
  
  from ocgis.util.shp_cabinet import ShpCabinetIterator
  ocgis.env.OVERWRITE = True
  sci = ShpCabinetIterator('50m_country')
  select_ugid = []
  geoms = '50m_country'
  
  try : 
    ugid = ugid_EUR(country)
  except Exception as e:
      msg = 'selection of continent failed'
      logger.exception(msg)
      raise CalculationException(msg, e)
  try:    
    rd = ocgis.RequestDataset(urls)
    ocgis.env.DIR_OUTPUT = dir_output
    ocgis.env.PREFIX = prefix
    geom_file = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping,  output_format=output_format, select_ugid=ugid, geom=geoms, add_auxiliary_files=False).execute()
  except Exception as e:
      msg = 'ocgis calculations failed '
      logger.exception(msg)
      raise CalculationException(msg, e)
  return  geom_file # countrycountry # # geom_file
