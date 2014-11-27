from malleefowl.process import WPSProcess
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


class icclimWorker(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "indice",
      title="Climate indices (icclim)",
      version = "0.1",
      metadata=[],
      abstract="Calculation of climate indices based on icclim",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
      title="NetCDF File",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=100,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.TG = self.addLiteralInput(
      identifier="TG",
      title="TG",
      abstract="Mean of mean temperatur (tas files as input files)",
      type=type(False),
      default=False,
      minOccurs=0,
      maxOccurs=0,
      )
        
    self.TX = self.addLiteralInput(
      identifier="TX",
      title="TX",
      abstract="mean of max temperatur (tasmax files as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=0,
      )
        
    self.TN = self.addLiteralInput(
      identifier="TN",
      title="TN",
      abstract="Mean over min temperatur (tasmin files as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=0,
      )
    
    self.RR = self.addLiteralInput(
      identifier="RR",
      title="RR",
      abstract="precipitation sum (pr files as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=0,
      )
        
    self.SU = self.addLiteralInput(
      identifier="SU",
      title="SU",
      abstract="Nr of summer days (tasmax files as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=0,
      )

    # complex output
    # -------------
    self.logout = self.addComplexOutput(
      identifier="logout",
      title="indice log",
      abstract="indice log",
      metadata=[],
      formats=[{"mimeType":"text/plain"}],
      asReference=True,
      )
    
    self.tarout = self.addComplexOutput(
      title="netCDF result files",
      abstract="Tar archive containing the netCDF result files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="ncout",
      )

  def execute(self):
    import os
    import tools # from flyingpigeon 
    import tarfile
    import tempfile
    
    logger.debug('starting icclim indices execution')
    
    self.show_status('starting calcualtion of icclim indices', 0)
    ncfiles = self.getInputValues(identifier='netcdf_file')
    (fp_tar, tarf) = tempfile.mkstemp(dir=".", suffix='.tar')
    tar = tarfile.open(tarf, "w")
    
    logger.debug('working dir prepared ')
    
    nc_renamed = tools.fn_creator(ncfiles)
    
    idic = { 'outdir':os.curdir, 
             'ncs': nc_renamed,
             'TG': self.TG.getValue(),
             'TX': self.TX.getValue(),
            #'TXx':
            #'TXn':
             'TN':self.TN.getValue(),
            #'TNx':
            #'TNn':
             'SU':self.SU.getValue(),
            #'CSU':
            #'FD':
            #'CFD':
            #'TR':
            #'ID':
            #'HD17':
            #'GD4':
             'RR':self.RR.getValue(),
            #'RR1':
            #'CWD':
            #'SDII':
            #'R10mm':
            #'R20mm':
            #'RX1day':
            #'RX5day':
            #'SD':
            #'SD1':
            #'SD5cm':
            #'SD50cm':
            #'CDD':
            #'DTR':
            #'ETR':
            #'vDTR':
            }   
    
    logtxt = tools.indices(idic)
    
    logger.debug('flyingpigeon indices tool processed') 
    
    ncsout = [f for f in os.listdir(os.curdir) if '.nc' in f]
    for n in ncsout: 
      tar.add(n, arcname = n.replace(os.curdir, ""))
    
    logger.debug('tar ncfiles')
    
    logfile = self.mktempfile(suffix='.txt')
    with open(logfile, 'w') as fp:
        fp.write(logtxt)
    
    tar.add(logfile )
    tar.close()    
    
    logger.debug('tar file closed')
    
    self.logout.setValue( logfile )
    self.tarout.setValue( tarf )
    self.show_status("processing done", 100)