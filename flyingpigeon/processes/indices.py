#import sys
#sys.path.append('/home/nils/birdhouse/malleefowl/malleefowl/process.py')

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
      maxOccurs=1000,
      maxmegabites=500000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )
    
    self.concat = self.addLiteralInput(
      identifier="concat",
      title="Concatination",
      abstract="Concatination of rcps to the approprate historical runs",
      type=type(False),
      default=False,
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.group = self.addLiteralInput(
      identifier="group",
      title="Group",
      abstract="Select an aggregation",
      default='year',
      type=type(''),
      minOccurs=0,
      maxOccurs=1,
      allowedValues=["year", "month", "sem"]
      )

    self.TG = self.addLiteralInput(
      identifier="TG",
      title="TG",
      abstract="Mean of mean temperatur (tas as input files)",
      type=type(False),
      default=False,
      minOccurs=0,
      maxOccurs=1,
      )
      
    
    self.TX = self.addLiteralInput(
      identifier="TX",
      title="TX",
      abstract="mean of max temperatur (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
        
    self.TN = self.addLiteralInput(
      identifier="TN",
      title="TN",
      abstract="Mean of daily min temperatur (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.TXx = self.addLiteralInput(
      identifier="TXx",
      title="TXx",
      abstract="Max of daily max temperatur (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.TXn = self.addLiteralInput(
      identifier="TXn",
      title="TXn",
      abstract="Min of daily min temperatur (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.TNx = self.addLiteralInput(
      identifier="TNx",
      title="TNx",
      abstract="Max of daily min temperatur (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.TNn = self.addLiteralInput(
      identifier="TNn",
      title="TNn",
      abstract="Min of daily min temperatur (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
      
    self.SU = self.addLiteralInput(
      identifier="SU",
      title="SU",
      abstract="Nr of summer days (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )

    self.CSU = self.addLiteralInput(
      identifier="CSU",
      title="CSU",
      abstract="Nr of consecutive summer days (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )

    self.FD = self.addLiteralInput(
      identifier="FD",
      title="FD",
      abstract="Nr of frost days (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.CFD = self.addLiteralInput(
      identifier="CFD",
      title="CFD",
      abstract="Nr of consecutive frost days (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
       
    
    self.TR = self.addLiteralInput(
      identifier="TR",
      title="TR",
      abstract=" ... (tasmin as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
       
    self.ID = self.addLiteralInput(
      identifier="ID",
      title="ID",
      abstract=" ... (tasmax as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      ) 
       
    self.HD17 = self.addLiteralInput(
      identifier="HD17",
      title="HD17",
      abstract=" ... (tas as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.GD4 = self.addLiteralInput(
      identifier="GD4",
      title="GD4",
      abstract=" ... (tasmean as input files)",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.RR = self.addLiteralInput(
      identifier="RR",
      title="RR",
      abstract="precipitation flux (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.RR1 = self.addLiteralInput(
      identifier="RR1",
      title="RR1",
      abstract="precipitation ... (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.CWD = self.addLiteralInput(
      identifier="CWD",
      title="CWD",
      abstract="Consecutive wet days (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.SDII = self.addLiteralInput(
      identifier="SDII",
      title="SDII",
      abstract="... (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
             
    self.R10mm = self.addLiteralInput(
      identifier="R10mm",
      title="R10mm",
      abstract="Nr of days >10mm (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
         
    self.R20mm = self.addLiteralInput(
      identifier="R20mm",
      title="R20mm",
      abstract="Nr of days >20mm (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
              
    self.RX1day = self.addLiteralInput(
      identifier="RX1day",
      title="RX1day",
      abstract=" ...  (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )   
                     
    self.RX5day = self.addLiteralInput(
      identifier="RX5day",
      title="RX5day",
      abstract=" ...  (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.SD = self.addLiteralInput(
      identifier="SD",
      title="SD",
      abstract="Nr of snow days (prsn as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.SD1 = self.addLiteralInput(
      identifier="SD1",
      title="SD1",
      abstract=" ... (prsn as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )   
          
    self.SD5cm = self.addLiteralInput(
      identifier="SD5cm",
      title="SD5cm",
      abstract=" ... (prsn as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
     
    self.SD50cm = self.addLiteralInput(
      identifier="SD50cm",
      title="SD50cm",
      abstract=" ... (prsn as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
      )
    
    self.CDD = self.addLiteralInput(
      identifier="CDD",
      title="CDD",
      abstract="Consecutive dry days (pr as input files) ",
      default=False,
      type=type(False),
      minOccurs=0,
      maxOccurs=1,
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
    os.mkdir(os.path.curdir+'/icclim_files/')
    outdir = (os.path.curdir+'/icclim_files/')
    
    logger.debug('working dir prepared ')
    logger.debug('group parameter: %s ' % self.group.getValue() )
    
    nc_renamed = tools.fn_creator(ncfiles)
    
    idic = { 'outdir':outdir, 
            'ncs': nc_renamed,
            'concat':self.concat.getValue(),
            'group':self.group.getValue(),
            'TG':self.TG.getValue(),
            'TX':self.TX.getValue(),
            'TXx':self.TXx.getValue(),
            'TXn':self.TXn.getValue(),
            'TN':self.TN.getValue(),
            'TNx':self.TNx.getValue(),
            'TNn':self.TNn.getValue(),
            'SU':self.SU.getValue(),
            'CSU':self.CSU.getValue(),
            'FD':self.FD.getValue(),
            'CFD':self.CFD.getValue(),
            'TR':self.TR.getValue(),
            'ID':self.ID.getValue(),
            'HD17':self.HD17.getValue(),
            'GD4':self.GD4.getValue(),
            'RR':self.RR.getValue(),
            'RR1':self.RR1.getValue(),
            'CWD':self.CWD.getValue(),
            'SDII':self.SDII.getValue(),
            'R10mm':self.R10mm.getValue(),
            'R20mm':self.R20mm.getValue(),
            'RX1day':self.RX1day.getValue(),
            'RX5day':self.RX5day.getValue(),
            'SD':self.SD.getValue(),
            'SD1':self.SD1.getValue(),
            'SD5cm':self.SD5cm.getValue(),
            'SD50cm':self.SD50cm.getValue(),
            'CDD':self.CDD.getValue(),
            }

#multivariate indice  DTR, ETR, vDTR  indice_multivar(...)
#percentile-based indice TG10p, TX10p, TN10p, TG90p, TX90p, TN90p, WSDI, CSDI, R75p, R75TOT, R95p, R95TOT, R99p, R99TOT  indice_perc(...)
#compound percentile-based indice CD, CW, WD, WW  indice_compound(...)
    
    logtxt = tools.indices(idic)
    logger.debug('flyingpigeon indices tool processed') 
    #ncs = os.listdir(outdir)
    
    #ncs_new = tools.fn_creator(ncs)
    
    tar.add(outdir, arcname = outdir.replace(os.curdir, ""))
    logger.debug('tar ncfiles')
    
    logfile = self.mktempfile(suffix='.txt')
    with open(logfile, 'w') as fp:
        fp.write(logtxt)
    
    tar.add(logfile, arcname = outdir.replace(os.curdir, ""))
    tar.close()
    logger.debug('tar file closed')
    
    self.logout.setValue( logfile )
    self.tarout.setValue( tarf )
    self.show_status("processing done", 100)