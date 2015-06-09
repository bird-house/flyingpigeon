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
    
#    url_2014 = 'http://www.ecad.eu/download/ensembles/data/months/%s_0.25deg_reg_2014.nc.gz' % (var_eobs)  
#    url = 'http://www.ecad.eu/download/ensembles/data/Grid_0.25deg_reg/%s_0.25deg_reg_v10.0.nc.gz' % (var_eobs)

    url = 'http://opendap.knmi.nl/knmi/thredds/dodsC/e-obs_0.22rotated/%s_0.22deg_rot_v11.0.nc' % (var_eobs)
    rd = ocgis.RequestDataset( url, var_eobs, time_region = {'year':[2010]}) # start 1950
    
    #cordex_format = ocgis.OcgOperations(dataset=rd, calc=calc, prefix=str('cordex_format'), output_format='nc').execute()
      
    # todo: check if decompressed file exist. 
    
    #self.show_status('downlaoding  EOBS file.gz: %s '  % var_eobs , 5)
    #eobs_2014 = download(url_2014)
    #eobs = download(url)
   
    #self.show_status('extraction of : %s '  % var_eobs , 5)
    #p, gz_2014 = os.path.split(eobs)
    
    #cmd = 'gunzip -k %s; gunzip -k %s ' % (eobs_2014, eobs)
    #logger.debug('system call : %s '  % (cmd))
    #os.system(cmd)
    
    #nc_2014 = os.path.join(p,'%s_0.25deg_reg_2014.nc' % var_eobs )
    #nc = os.path.join(p,'%s_0.25deg_reg_v10.0.nc' % var_eobs )
    
    #logger.debug('starting mergetime ')
    
    #p1, nc_merge = tempfile.mkstemp(dir='.', suffix='.nc')
    #cdo.mergetime(input = [nc, nc_2014], output=nc_merge)
    
    #logger.debug('mergetime done')
    
    self.show_status('processing with ocgis : %s '  % var_eobs , 5)
    if var_eobs != 'rr': 
      rd = ocgis.RequestDataset( url , var_eobs, conform_units_to=unit) # nc, nc,   , nc_2014
      ocgis.env.OVERWRITE=True

      cordex_file = ocgis.OcgOperations(dataset= rd, name=var, output_format='nc', dir_output= '.', add_auxiliary_files=False).execute()
    
    logger.debug('geom_file : %s '  % (geom_file))
    
    #p1, tmp1 = tempfile.mkstemp(dir='.', suffix='.nc')
    #p2, tmp2 = tempfile.mkstemp(dir='.', suffix='.nc')
    
    ### print(geom_file)
    #cdo.setreftime('1949-12-01,00:00:00,days', input=geom_file, output=tmp1)
    #cdo.setname('tasmax', input=tmp1, output=tmp2)
    
    # set globlal attributes 

    self.ncout.setValue('%s' % (cordex_file))
    self.show_status('execution ended at : %s'  %  dt.datetime.now() , 100)
    
 
#:CDI = "Climate Data Interface version 1.6.7 (https://code.zmaw.de/projects/cdi)" ;
#:Conventions = "CF-1.4" ;
#:history = "Wed Jan 21 12:08:17 2015: cdo setname,tasmax /home/nils/anaconda/var/tmp/pywps-instanceP7sNaQ/tmplQqdsp.nc /home/nils/anaconda/var/tmp/pywps-instanceP7sNaQ/tmpchdCfC.nc\n",
        #"Wed Jan 21 12:08:14 2015: cdo setreftime,1949-12-01,00:00:00,days ./ocgis_output.nc /home/nils/anaconda/var/tmp/pywps-instanceP7sNaQ/tmplQqdsp.nc\n",
        #"\n",
        #"2015-01-21 11:08:13.977041 UTC ocgis-1.0.1-next: OcgOperations(calc_sample_size=False, optimizations=None, output_format=\"nc\", select_ugid=None, format_time=True, select_nearest=False, output_crs=None, time_range=None, calc_grouping=None, prefix=\"ocgis_output\", abstraction=\"None\", regrid_destination=None, allow_empty=False, vector_wrap=False, aggregate=False, interpolate_spatial_bounds=False, dataset=RequestDatasetCollection(request_datasets=[RequestDataset(uri=\"/home/nils/anaconda/var/cache/pywps/tn_0.25deg_reg_2014.nc\", variable=\"tn\", alias=\"tn\", units=None, time_range=None, time_region=None, level_range=None, conform_units_to=\"K\", crs={\'no_defs\': True, \'ellps\': \'WGS84\', \'proj\': \'longlat\', \'towgs84\': \'0,0,0,0,0,0,0\'}, t_units=None, t_calendar=None, t_conform_units_to=None, did=1, meta={}, s_abstraction=None, dimension_map=None, name=\"tn\", driver=\"netCDF\", regrid_source=True, regrid_destination=False)]), dir_output=\".\", backend=\"ocg\", search_radius_mult=2.0, add_auxiliary_files=False, slice=None, callback=None, calc_raw=False, agg_selection=False, level_range=None, snippet=False, time_region=None, geom=None, regrid_options={\'value_mask\': None, \'with_corners\': \'choose\'}, conform_units_to=None, spatial_operation=\"intersects\", headers=None, calc=None, file_only=False, )" ;
#:Ensembles_ECAD = "9.0" ;
#:References = "http://www.ecad.eu\\nhttp://www.ecad.eu/download/ensembles/ensembles.php\\nhttp://www.ecad.eu/download/ensembles/Haylock_et_al_2008.pdf" ;
#:grid_north_pole_latitude = 39.25f ;
#:grid_north_pole_longitude = -162.f ;
#:CDO = "Climate Data Operators version 1.6.7 (https://code.zmaw.de/projects/cdo)" ;
 
 
 
