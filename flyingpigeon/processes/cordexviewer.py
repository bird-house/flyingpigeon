from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.subsetting import POLYGONS
from flyingpigeon.indices import indices, indices_description 
from flyingpigeon.utils import GROUPING
from flyingpigeon.workflow import calc_cordexviewer


class cordexviewer(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "cordexviewer",
      title="Cordex Viewer",
      version = "0.1",
      metadata=[],
      abstract="Preparing icclim indices for Cordex-Viewer (fieldmeans over country polygons)",
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
      maxOccurs=9,
      allowedValues=GROUPING 
      )

    self.polygons = self.addLiteralInput(
      identifier="polygons",
      title="Polygons",
      abstract="Regions polygons for subsetting",
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
      
    self.out_polygons = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons indices  ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_polygons",
      )

    self.out_fieldmeans = self.addComplexOutput(
      title="fieldmeans",
      abstract="Tar archive containing the netCDF EU-countries fieldmeans indices ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_fieldmeans",
      )

  def execute(self):
      from os import mkdir
      from tempfile import  mkstemp
      import tarfile
    
      ncs = self.getInputValues(identifier='resource')
      indice_list = self.getInputValues(identifier='indice')
      region_list = self.getInputValues(identifier='polygons')

      self.show_status('starting: indice=%s, num_files=%s' % (indice_list, len(ncs)), 0)
      
      # === Calculation of indices
      try:
        dir_simple_indices = path.abspath(path.curdir+'/dir_simple_indices/')
        mkdir(dir_simple_indices)
      except  Exception as e:
        msg = 'calculation of indice failed!: %s ' % (e)
        logger.error(msg)

        
      # === Calculation of fieldmeans
      try:  
        dir_fieldmeans = path.abspath(path.curdir+'/dir_fieldmean/')
        mkdir(dir_fieldmeans)
      except  Exception as e:
        msg = 'calculation of indice failed!: %s ' % (e)
        logger.error(msg)
      
      # === create file structure and sort files into
      
      # === archivating
      try: 
        (fp_tarf_fieldmeans, tarf_fieldmeans) = mkstemp(dir=".", suffix='.tar')
        (fp_tarf_polygons, tarf_polygons) = mkstemp(dir=".", suffix='.tar')
        
        tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
        tar_polygons = tarfile.open(tarf_polygons, "w")
        logger.debug('tar files initialized')
        
        
        tar_fieldmeans.add(dir_fieldmean, arcname = dir_fieldmean.replace(path.abspath(path.curdir), ""))
        tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(path.abspath(path.curdir), ""))
        logger.debug('data to tar files')
        
        tar_fieldmeans.close()
        tar_polygons.close()
        logger.debug('tar files closed')
        
        # create output folders
       
        logger.debug('out directories created')
      except  Exception as e:
        msg = 'tar file or mkdir failed!: %s ' % (e)
        logger.error(msg)
      
      
      self.out_fieldmeans.setValue( tarf_fieldmeans )
      self.out_polygons.setValue( tarf_polygons )
      
      self.show_status('done:', 100) #  indice=%s, num_outfiles=%s' % (indice_list, len(files))

