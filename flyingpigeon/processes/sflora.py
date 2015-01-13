from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class sflora(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "sflora",
      title="Segetal Flora",
      version = "0.1",
      metadata=[],
      abstract="Species biodeversity of segetal Flora",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF File",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1000,
      maxmegabites=500000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.climate_type = self.addLiteralInput(
      identifier="climate_type",
      title="Climate type",
      abstract="Select climate type",
      default='3',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["1", "2", "3", "4", "5", "6", "7", "all"] # sem
      )

    self.culture_type = self.addLiteralInput(
      identifier="culture_type",
      title="Culture type",
      abstract="Select culture type",
      default='fallow',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["fallow", "intensiv", "extensiv", "all"] # sem
      )


# calculation of number of segetal flora species

import tempfile
import shutil

from cdo import *
cdo = Cdo()

def get_equation(culture_type, climate_type):
  if culture_type == 'fallow': 
    if climate_type == '1': 
      equation = "\'sf_fal_1=exp(46.4619-15.4355*(%s-273.15)+0.7070*((%s-273.15)^2))\'" %('tas','tas')
    elif climate_type == '2': 
      equation = "\'sf_fal_2=66.463*exp(-0.1315*(%s-273.15))\'"  %('tas')
    elif climate_type == '3': 
      equation ="\'sf_fal_3=exp(1.837+0.2856*(%s-273.15)-0.0156*(%s-273.15)^2)\'" %('tas','tas')
    elif climate_type == '4': 
      equation ="\'sf_fal_4=0.0685*(%s-273.15)^3-2.9184*(%s-273.15)^2+38.864*(%s-273.15)-80.046\'" %('tas','tas','tas')
    elif climate_type == '5': 
      equation ="\'sf_fal_5=1.68*exp(0.735*exp(0.1134*(%s-273.15)))\'" %('tas')
    elif climate_type == '6': 
      equation ="\'sf_fal_6=0.0088*(%s-273.15)^3-0.3457*(%s-273.15)^2+3.6656*(%s-273.15)+5.3486\'" %('tas','tas','tas')
    elif climate_type == '7': 
      equation ="\'sf_fal_7=3.4655*exp(0.1303*(%s-273.15))\'" %('tas')
    elif climate_type == 'all': 
      equation ="\'sf_fal_a=0.2787*(%s-273.15)^3-7.5658*(%s-273.15)^2+72.4143*(%s-273.15)-76.29\'" %('tas','tas','tas')
    else: 
      equation = None
      
  elif culture_type == 'extensiv': 
    if climate_type == '1': 
      equation ="\'sf_ext_1=exp(46.0518-15.4597*%s+0.7143*%s^2)\'" %('tas', 'tas')
    elif climate_type == '2': 
      equation ="\'sf_ext_2=45.6589*exp(-0,0987*%s)\'" %('tas')
    elif climate_type == '3': 
      equation ="\'sf_ext_3=exp(1.4383+0.33*%s-0.0176*%s^2)\'" %('tas','tas')
    elif climate_type == '4': 
      equation ="\'sf_ext_4=-0.7587*%s^2+17.8515*%s-32.8794\'" %('tas','tas')
    elif climate_type == '5': 
      equation ="\'sf_ext_5=0.0817*exp(0.4597*%s)\'" %('tas')
    elif climate_type == '6': 
      equation ="\'sf_ext_6=-0.0263*%s^3+0.7119%s^2-4.878*%s+11.154\'" %('tas','tas','tas')
    elif climate_type == '7': 
      equation ="\'sf_ext_7=1.1653*exp(0.1711*%s)\'" %('tas')
    elif climate_type == 'all': 
      equation ="\'sf_ext_a=0.2386*%s^3-6.5351*%s^2+62.475*%s-73.9023\'" %('tas','tas','tas')
    else: 
      equation = None 

  elif culture_type == 'intensiv': 
    if climate_type == '1': 
      equation ="\'sf_int_1=exp(46.0518-15.4597*%s+0.7143*%s^2)\'" %('tas','tas')
    elif climate_type == '2': 
      equation ="\'sf_int_2=31.3493*exp(-0.1108*%s)\'" %('tas')
    elif climate_type == '3': 
      equation ="\'sf_int_3 = exp(1.0791+0.3449*%s-0.0189*%s^2)\'" %('tas','tas')
    elif climate_type == '4': 
      equation ="\'sf_int_4=-0.0919%s^3+2.3824*%s^2-14.29*%s+38.93\'" %('tas','tas','tas')
    elif climate_type == '5': 
      equation ="\'sf_int_5=exp(0.1663+0.0457*%s+0.0128%s^2)\'" %('tas','tas')
    elif climate_type == '6': 
      equation ="\'sf_int_6=14.1641*exp(-0.0363*%s)\'" %('tas')
    elif climate_type == '7': 
      equation ="\'sf_int_7=1.704*exp(0.0982*%s)\'" %('tas')
    elif climate_type == 'all': 
      equation ="\'sf_int_a=43.2846*exp(0.071*%s)\'" %('tas')
    else: 
      equation = None
  return equation


def clipping (ncs , tmp_dir):
  from ocgis.util.shp_process import ShpProcess
  from ocgis.util.shp_cabinet import ShpCabinetIterator
  from ocgis.util.helpers import get_sorted_uris_by_time_dimension
  import ocgis

  ocgis.env.DIR_SHPCABINET = '/homel/nhempel/birdhouse/flyingpigeon/flyingpigeon/processes/shapefiles' #path.join(path.dirname(__file__),'shapefiles')
  ocgis.env.DIR_OUTPUT = tmp_dir
  ocgis.env.OVERWRITE = True  
  sc = ocgis.ShpCabinet()
  geoms = 'continent'
  select_ugid = [8] # UGID for Europe

  p2, tmp2 = tempfile.mkstemp(dir=tmp_dir, suffix='.nc')
  p2, f2 = os.path.split(tmp1)
  
  if type(ncs) == list: 
    ncs = get_sorted_uris_by_time_dimension(ncs)

  rd = ocgis.RequestDataset(ncs)

  geom_nc  = ocgis.OcgOperations(dataset=rd, geom=geoms, select_ugid=select_ugid, output_format='nc',\
    prefix=f2.strip('.nc'), add_auxiliary_files=False).execute()
  
  return '%s' % (geom_nc)
     
climate_type = ['1','2','3','4','5','6','7','all']
culture_type = ['fallow', 'extensiv', 'intensiv', 'all']

equation = get_equation(culture_type='fallow', climate_type='2')

nc = '/homel/nhempel/anaconda/var/cache/pywps/tas_EUR-44_CCCma-CanESM2_historical_r1i1p1_SMHI-RCA4_v1_sem_200012-200511.nc'
out = '/homel/nhempel/data/cdotest.nc'

# create temp dir
tmp_dir = tempfile.mkdtemp()

p1, tmp1 = tempfile.mkstemp(dir=tmp_dir, suffix='.nc')
cdo.yearmean (input = nc , output = tmp1)

nc_eur = clipping( '%s' % (tmp1) , tmp_dir)
cdo.expr(equation , input=tmp1, output=out)

os.close( p1 )
shutil.rmtree(tmp_dir)
