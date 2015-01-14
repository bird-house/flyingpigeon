from malleefowl import wpslogging as logging
from malleefowl.process import WPSProcess
from datetime import datetime, date
import types

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
      abstract="URL to netCDF file",
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
      allowedValues=['tn', 'tx' , 'tn' , 'rr']
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
    
    import ocgis
    from cdo import * 
    cdo = Cdo()
    
    self.show_status('execution started at : %s '  % dt.datetime.now() , 5)

    var_eobs = self.getInputValues(identifier='var_eobs')
    if var_eobs == 'tg':
      var = 'tas'
      unit = 'K'
    elif var_eobs == 'tn':
      var = 'tasmin'
      unit = 'K'
    elif var_eobs == 'tx':
      var = 'tasmax'
      unit = 'K'
    elif var_eobs == 'rr':
      var = 'pr'
      unit = 'kg m-2 s-1'
      
    url_2014 = 'http://www.ecad.eu/download/ensembles/data/months/%s_0.22deg_rot_2014.nc.gz' % (var_eobs)  
    url = 'http://www.ecad.eu/download/ensembles/data/Grid_0.22deg_rot/%s_0.22deg_rot_v10.0.nc.gz' % (var_eobs)
    nc_2014 = os.path.join(path,'%s_0.22deg_rot_2014.nc' % var_eobs )
    nc = os.path.join(path,'%s_0.22deg_rot_v10.0.nc' % var_eobs )
    
    # todo: check if decompressed file exist. 
    
    eobs_2014 = download(url_2014)
    eobs = download(url)
    
    path, gz_2014 = os.path.split(eobs)
    cmd = 'gunzip %s; gunzip %s ' % (eobs_2014, eobs)
    os.system(cmd)
    
    rd = ocgis.RequestDataset([nc,nc_2014] , var_eobs, conform_units_to=unit)

    ocgis.env.OVERWRITE=True

    geom_file = ocgis.OcgOperations(dataset= rd, output_format='nc', dir_output= '.', add_auxiliary_files=False).execute()
    ### print(geom_file)

    ##cdo.setreftime('1949-12-01,00:00:00,days', input=geom_file, output='/home/nils/data/EOBS/tx_0.22deg_rot_2014_Cordex.nc')
    ##cdo.setname('tasmax', input='/home/nils/data/EOBS/tx_0.22deg_rot_2014_Cordex.nc' , output='/home/nils/data/EOBS/tasmax_EOBS-22_2014.nc')
    
    self.ncout.setValue('%s' % (geom_file))
    self.show_status('execution ended at : %s'  %  dt.datetime.now() , 100)
    
    
    