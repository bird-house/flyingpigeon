from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.subset import POLYGONS
from flyingpigeon.indices import indices, indices_description 
from flyingpigeon.utils import GROUPING


class extremoscop(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "extremoscop",
      title="Extremoscop Viewer",
      version = "0.1",
      metadata=[],
      abstract="Preparing icclim indices for Cathy's Extremoscop-Viewer (fieldmeans over french regions polygons)",
      )

    self.resource = self.addComplexInput(
      identifier="resource",
      title="Resouces",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1024,
      maxmegabites=50000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )
    
    self.indice = self.addLiteralInput(
      identifier="indice",
      title="Indice",
      abstract=indices_description(),
      default='TG',
      type=type(''),
      minOccurs=1,
      maxOccurs=len(indices()),
      allowedValues=indices()
      )

    self.grouping = self.addLiteralInput(
      identifier="grouping",
      title="Time Aggregation",
      abstract="Select time aggegation",
      default='year',
      type=type(''),
      minOccurs=1,
      maxOccurs=10,
      allowedValues=GROUPING 
      )

    self.polygons = self.addLiteralInput(
      identifier="polygons",
      title="Polygons",
      abstract="Regions polygons for subset",
      default='FRA',
      type=type(''),
      minOccurs=1,
      allowedValues=POLYGONS
      )
      
    #self.output = self.addComplexOutput(
      #identifier="output",
      #title="Indices",
      #abstract="List of calculated indices.",
      #metadata=[],
      #formats=[{"mimeType":"text/json"}],
      #asReference=True
      #)
      
    self.tarout = self.addComplexOutput(
      identifier="tarout",
      title="netCDF result files",
      abstract="Tar archive containing folder structure with the result fieldmean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      )    

  def execute(self):
    
