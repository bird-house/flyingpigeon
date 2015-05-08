from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


class segetalflora(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "segetalflora",
      title="Segetal Flora",
      version = "0.1",
      metadata=[],
      abstract="Species biodiversity of segetal flora",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF Files",
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

    # complex output
    # -------------
    #self.logout = self.addComplexOutput(
      #identifier="logout",
      #title="Indice log-file",
      #abstract="logfile for segetalflora process",
      #metadata=[],
      #formats=[{"mimeType":"text/plain"}],
      #asReference=True,
      #)
    
    self.out_tas = self.addComplexOutput(
      title="tas_EUR",
      abstract="Tar archive containing the netCDF EUR tas mean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_tas",
      )

    self.out_polygons = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_polygons",
      )

    self.out_fieldmeans = self.addComplexOutput(
      title="fieldmeans",
      abstract="Tar archive containing the netCDF EU-countries fieldmeans segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_fieldmeans",
      )

# calculation of number of segetal flora species
  def execute(self):
    
    from os import mkdir
    from os.path import curdir
    import tarfile
    import tempfile
    
    import tools #, clipping, timeseries, utils
    import segetalflora as sg

    logger.debug('starting segetalflora process execution')
    self.show_status('starting calcualtion segetalflora', 5)
    
    ## prepare environment

    # create the tar files 
    (fp_tarf_tas, tarf_tas) = tempfile.mkstemp(dir=".", suffix='.tar')
    (fp_tarf_polygons, tarf_polygons) = tempfile.mkstemp(dir=".", suffix='.tar')
    (fp_tarf_fieldmeans, tarf_fieldmeans) = tempfile.mkstemp(dir=".", suffix='.tar')
    tar_tas = tarfile.open(tarf_tas, "w")
    tar_polygons = tarfile.open(tarf_polygons, "w")
    tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
    
    # create output folders
    dir_tas = (curdir+'/dir_tas/')
    dir_polygons = (curdir+'/dir_polygons/')
    dir_fieldmeans = (curdir+'/dir_fieldmeans/')
    mkdir(dir_tas)
    mkdir(dir_polygons)
    mkdir(dir_fieldmeans)

    # read argments to variables
    ncs = self.getInputValues(identifier='netcdf_file')
    
    # recreate the filenames for CORDEX convention
    nc_renamed = tools.fn_creator(ncs)

    logger.debug('working dir environment prepared ')


    self.show_status('files to tar archives', 75)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(curdir, ""))
    tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(curdir, ""))
    tar_fieldmeans.add(dir_fieldmeans, arcname = dir_fieldmeans.replace(curdir, ""))
    tar_tas.close()
    tar_fieldmeans.close()
    tar_polygons.close()
    logger.debug('tar ncfiles closed')
    
   # self.logout.setValue( logfile )
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_polygons.setValue( tarf_polygons )
    self.out_tas.setValue( tarf_tas )
    self.show_status("processing done", 100)
