from malleefowl import wpslogging as logging
from malleefowl.process import WPSProcess
from datetime import datetime, date
import types

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

    
    self.show_status('execution started at : %s '  % dt.datetime.now() , 5)

    list_eobs = self.getInputValues(identifier='var_eobs')

    if type(list_eobs) == list:
      var_eobs = list_eobs[0]
    elif type(list_eobs) == str:
      var_eobs = list_eobs
    else: 
      logger.exception('eobs variable setting failed!')
      
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
    
    # todo: check if decompressed file exist. 
    
    self.show_status('downlaoding  EOBS file.gz: %s '  % var_eobs , 5)
    eobs_2014 = download(url_2014)
    eobs = download(url)
   
    self.show_status('extraction of : %s '  % var_eobs , 5)
    p, gz_2014 = os.path.split(eobs)
    
    cmd = 'gunzip %s; gunzip %s ' % (eobs_2014, eobs)
    logger.debug('system call : %s '  % (cmd))
    os.system(cmd)
    
    nc_2014 = os.path.join(p,'%s_0.22deg_rot_2014.nc' % var_eobs )
    nc = os.path.join(p,'%s_0.22deg_rot_v10.0.nc' % var_eobs )
    
    self.show_status('processing with ocgis : %s '  % var_eobs , 5)
    rd = ocgis.RequestDataset([nc, nc_2014] , var_eobs, conform_units_to=unit) # 
    ocgis.env.OVERWRITE=True

    geom_file = ocgis.OcgOperations(dataset= rd, output_format='nc', dir_output= '.', add_auxiliary_files=False).execute()
    
    logger.debug('geom_file : %s '  % (geom_file))
    
    p1, tmp1 = tempfile.mkstemp(dir='.', suffix='.nc')
    p2, tmp2 = tempfile.mkstemp(dir='.', suffix='.nc')
    
    ### print(geom_file)
    cdo.setreftime('1949-12-01,00:00:00,days', input=geom_file, output=tmp1)
    cdo.setname('tasmax', input=tmp1, output=tmp2)
    
    self.ncout.setValue('%s' % (tmp2))
    self.show_status('execution ended at : %s'  %  dt.datetime.now() , 100)
    
    
    