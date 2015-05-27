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
      
    self.output = self.addComplexOutput(
      identifier="output",
      title="Indices",
      abstract="List of calculated indices.",
      metadata=[],
      formats=[{"mimeType":"text/json"}],
      asReference=True
      )
      
    #self.tarout = self.addComplexOutput(
      #identifier="tarout",
      #title="netCDF result files",
      #abstract="Tar archive containing the result fieldmean files",
      #formats=[{"mimeType":"application/x-tar"}],
      #asReference=True,
      #)    

  def execute(self):
    
      ncs = self.getInputValues(identifier='resource')
      indice_list = self.getInputValues(identifier='indice')
      region_list = self.getInputValues(identifier='polygons')

      self.show_status('starting: indice=%s, num_files=%s' % (indice_list, len(ncs)), 0)

      results = calc_cordexviewer(
          resource = ncs,
          indices = indice_list,
          regions = region_list,
          grouping = self.grouping.getValue(),
         # start_date = self.start_date.getValue(),
         # end_date = self.end_date.getValue(),
          out_dir = self.working_dir,
          monitor=self.show_status,
          )

      self.show_status("publishing results ...", 99)
      
      files = [result.strip() for result in results]

      from malleefowl.publish import publish
      urls = publish(files)

      import json
      outfile = self.mktempfile(suffix='.txt')
      with open(outfile, 'w') as fp:
          json.dump(obj=urls, fp=fp, indent=4, sort_keys=True)
          self.output.setValue(outfile)

      outfile = self.mktempfile(suffix='.txt')
      with open(outfile, 'w') as fp:
          for status in status_log:
              fp.write("%s\n" % status)
          self.status_log.setValue(outfile)
      
      self.show_status('done: indice=%s, num_outfiles=%s' % (indice_list, len(files)), 100)

