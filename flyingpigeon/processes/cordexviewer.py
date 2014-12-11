from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class cordexviewer(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "cordexviewer",
      title="Cordex Viewer",
      version = "0.1",
      metadata=[],
      abstract="Post processing of indice output for EUR-Cordex-Viewer",
      )

    self.tarin = self.addComplexInput(
      identifier="tarin",
      title="Tar archiv",
      abstract="Tar archiv with icclim output",
      minOccurs=1,
      maxOccurs=1,
      maxmegabites=500000,
      formats=[{"mimeType":"application/x-tar"}],
     # asReference=True,
      )
    
# output

    self.tarout = self.addComplexOutput(
      identifier="tarout",
      title="netCDF result files",
      abstract="Tar archive containing the result files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      )    

  def execute(self):
    import os
    import tools # from flyingpigeon 
    import tarfile
    import tempfile

    indices_uri = self.getInputValues(identifier='netcdf_file')
    
    indices_tar = tarfile.open(indices_uri, "r")
    logger.debug('tar file with indices opened')
    
    
    indices_tar.close()
    logger.debug('tar file closed')
    
    
    (fp_tar, tarf) = tempfile.mkstemp(dir="", suffix='.tar')
    tar = tarfile.open(tarf, "w")
    os.mkdir(os.path.curdir+'/cordexviewer/')
    outdir = (os.path.curdir+'/cordexviewer/')


    self.tarout.setValue( tarf )
    self.show_status("processing done", 100)
