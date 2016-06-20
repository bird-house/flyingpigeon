import os
import datetime as dt

from flyingpigeon.get_eobs_as_cordex import get_data
from flyingpigeon.get_eobs_as_cordex import EOBS_VARIABLES
from flyingpigeon.subset import countries, countries_longname 

from pywps.Process import WPSProcess

import logging

class EobsToCordexProcess(WPSProcess):
  
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "eobs_to_cordex",
      title="EOBS to CORDEX",
      version = "0.4",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="downloads EOBS data in adaped CORDEX format",
      statusSupported=True,
      storeSupported=True
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF",
      abstract="URL to netCDF EOBS file (if not set, the original EOBS source will be downloaded)",
      minOccurs=0,
      maxOccurs=1,
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
    
    #self.countries = self.addLiteralInput(
      #identifier="countries",
      #title="Countries",
      #abstract=countries_longname(), # "Administrative european countries",
      ##default='FRA',
      #type=type(''),
      #minOccurs=0,
      #maxOccurs=40,
      #allowedValues=countries()
      #)
    
    self.start = self.addLiteralInput(
      identifier="start",
      title="Starting Year",
      abstract="EOBS provides data since 1950",
      type=type(1),
      default=1950,
      minOccurs=0,
      maxOccurs=1,
      allowedValues=range(1950,2015)
      )
    
    self.end = self.addLiteralInput(
      identifier="end",
      title="End Year",
      abstract="Currently up to 2014 processable",
      type=type(1),
      default=2014,
      minOccurs=0,
      maxOccurs=1,
      allowedValues=range(1950,2015)
      )
    
    #self.tarout = self.addComplexOutput(
      #title="Result files",
      #abstract="Tar archive containing the netCDF result files",
      #formats=[{"mimeType":"application/x-tar"}],
      #asReference=True,
      #identifier="tarout",
      #)
    
    self.ncout = self.addComplexOutput(
      identifier="ncout",
      title="netCDF inputfile",
      abstract="EOBS netCDF file in CORDEX format",
      formats=[{"mimeType":"application/netcdf"}],
      asReference=True,
      )
    
  def execute(self):
   
   
    self.status.set('execution started at : %s '  % dt.datetime.now() , 0)

    variable = self.var_eobs.getValue()
    resource = self.netcdf_file.getValue()
    start = self.start.getValue()
    end = self.end.getValue()
    
    #polygons = self.getInputValues(identifier='polygons')
    
    #if len(polygons) == 0: 
    polygons=None
    
    cordex_file = get_data(resource=resource, variable=variable, polygons=polygons, dir_output=os.curdir, start=start, end=end)
    
    #(id_tarout, f_tarout) = tempfile.mkstemp(dir=".", suffix='.tar')
    #tarout_file = tarfile.open(f_tarout, "w")
    #self.status.set('environment set :' , 5)
    
    #files = []
    #for var_eobs in var_eobs_list: 
      #f = get_eobs_as_cordex.get_data_worker(var_eobs, start=2014, polygons=['DEU','AUT'])
      #files.append(f)
    
    #for f in files:
      #try:
        #tarout_file.add(f) #, arcname = anomalies_dir.replace(os.curdir, ""))
      #except Exception as e:
        #msg = 'add Tar faild  : %s ' % (e)
        #logging.error(msg)
        
    self.ncout.setValue('%s' % (cordex_file))
    self.status.set('execution ended at : %s'  %  dt.datetime.now() , 100)
    
