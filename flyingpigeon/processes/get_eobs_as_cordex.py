from malleefowl import wpslogging as logging
from malleefowl.process import WPSProcess
from datetime import datetime, date
import types

from flyingpigeon.eobs_to_cordex import EOBS_VARIABLES

from cdo import * 
cdo = Cdo()

# initialise
logger = logging.getLogger(__name__)

class eobs_to_cordex(WPSProcess):
  
  def __init__(self):
    # definition of this process
    WPSProcess.__init__(self, 
      identifier = "eobs_to_cordex",
      title="EOBS to CORDEX",
      version = "0.1",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="Converts EOBS data into CORDEX format",
      #extra_metadata={
          #'esgfilter': 'variable:tas,variable:evspsbl,variable:hurs,variable:pr',  #institute:MPI-M, ,time_frequency:day
          #'esgquery': 'variable:tas AND variable:evspsbl AND variable:hurs AND variable:pr' # institute:MPI-M AND time_frequency:day 
          #},
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF",
      abstract="URL to netCDF EOBS file (if not set, the original EOBS source will be downloaded)",
      minOccurs=0,
      maxOccurs=100,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )
    
    self.var_eobs = self.addLiteralInput(
      identifier="var_eobs",
      title="EOBS Variable",
      abstract="choose an EOBS Variable",
      default="tg",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=EOBS_VARIABLES
      )
    
    self.ncout = self.addComplexOutput(
      identifier="ncout",
      title="netCDF inputfile",
      abstract="netCDF file of the ps valuels",
      formats=[{"mimeType":"application/netcdf"}],
      asReference=True,
      )
    
  def execute(self):
    import tempfile 
    import datetime as dt
    import os
    from malleefowl.download import download
    from flyingpigeon import get_eobs_as_cordex
   
    self.show_status('execution started at : %s '  % dt.datetime.now() , 5)

    var_eobs_list = self.getInputValues(identifier='var_eobs')
    self.show_status('processing with ocgis : %s '  % var_eobs , 5)
    
    files = []
    for var_eobs in var_eobs_list: 
      f = get_eobs_as_cordex.get_data_worker(var_eobs, start=2014, polygons=['DEU','AUT'])
      files.append(f)
    
    self.ncout.setValue('%s' % (cordex_file))
    self.show_status('execution ended at : %s'  %  dt.datetime.now() , 100)
    