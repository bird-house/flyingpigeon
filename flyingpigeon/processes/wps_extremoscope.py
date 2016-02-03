import os

from flyingpigeon.subset import _COUNTRIES_ 
from flyingpigeon.indices import indices, indices_description 
from flyingpigeon.utils import GROUPING

from pywps.Process import WPSProcess

import logging

class ExtremoScopeProcess(WPSProcess):
  """This process calculates data for Extremoscope viewer"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "extremoscope",
      title="Extremoscope Viewer",
      version = "0.2",
      metadata=[],
      abstract="Preparing icclim indices for the Extremoscope -Viewer (fieldmeans over regions polygons)",
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
      #allowedValues=_COUNTRIES_()
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
    
      from os import mkdir
      from tempfile import  mkstemp
      import tarfile
    
      ncs = self.getInputValues(identifier='resource')
      indice_list = self.getInputValues(identifier='indice')
      region_list = self.getInputValues(identifier='polygons')

      self.status.set('starting: indice=%s, num_files=%s' % (indice_list, len(ncs)), 0)
      
      # === Calculation of indices
      try:
        dir_simple_indices = os.path.abspath(os.curdir+'/dir_simple_indices/')
        mkdir(dir_simple_indices)
      except  Exception as e:
        logging.exception('calculation of indice failed!')
        raise

        
      # === Calculation of fieldmeans
      try:  
        dir_fieldmeans = os.path.abspath(os.curdir+'/dir_fieldmean/')
        mkdir(dir_fieldmeans)
        dir_polygons = os.path.abspath(os.curdir+'/dir_polygon/')
        mkdir(dir_polygons)
      except  Exception as e:
        logging.exception('creation of output dir failed')
        raise
      
      # === create file structure and sort files into
      
      # === archivating
      try: 
        (fp_tarf_fieldmeans, tarf_fieldmeans) = mkstemp(dir=".", suffix='.tar')
        (fp_tarf_polygons, tarf_polygons) = mkstemp(dir=".", suffix='.tar')
        
        tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
        tar_polygons = tarfile.open(tarf_polygons, "w")
        logging.debug('tar files initialized')
        
        
        tar_fieldmeans.add(dir_fieldmeans, arcname = dir_fieldmeans.replace(os.path.abspath(os.curdir), ""))
        tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(os.path.abspath(os.curdir), ""))
        logging.debug('data to tar files')
        
        tar_fieldmeans.close()
        tar_polygons.close()
        logging.debug('tar files closed')
        
        # create output folders
       
        logging.debug('out directories created')
      except  Exception as e:
        logging.exception('tar file or mkdir failed!')
        raise
      
      #self.out_fieldmeans.setValue( tarf_fieldmeans )
      self.tarout.setValue( tarf_polygons )
      self.status.set('done:', 100) #  indice=%s, num_outfiles=%s' % (indice_list, len(files))


