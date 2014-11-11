from malleefowl import wpslogging as logging
from malleefowl.process import WPSProcess
from datetime import datetime, date
import types

# initialise
logger = logging.getLogger(__name__)

class analogs(WPSProcess):
  
  def __init__(self):
    # definition of this process
    WPSProcess.__init__(self, 
      identifier = "analogs",
      title="Days with analog pressure pattern",
      version = "0.1",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="Search for days with analog pressure pattern",
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
      maxOccurs=1000,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.experiment = self.addLiteralInput(
      identifier="experiment",
      title="Data experiment",
      abstract="Choose the experiment",
      default="NCEP",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['NCEP', 'CMPI5', 'CORDEX']
      )
       
    self.region = self.addLiteralInput(
      identifier="region",
      title="Select Region",
      abstract="Select a predifined region",
      default="NOA",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['NOA', 'Dummy', 'Dummy']
      )       

    #self.bbox = self.addLiteralOutput(
      #identifier="bbox",
      #title="Bounding Box",
      #abstract="This is a BBox: (minx,miny,maxx,maxy)",
      #default="0,-90,180,90",
      #type=type(''),
      #)

         
    self.dateSt = self.addLiteralInput(
      identifier="dateSt",
      title="Start date of analyse period",
      abstract="This is a Date: 2013-07-15",
      default="2013-07-15",
      type=type(date(2013,7,15)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.dateEn = self.addLiteralInput(
      identifier="dateEn",
      title="End date of analyse period",
      abstract="This is a Date: 2013-12-31",
      default="2013-12-31",
      type=type(date(2013,12,31)),
      minOccurs=1,
      maxOccurs=1,
      )

    self.refSt = self.addLiteralInput(
      identifier="refSt",
      title="Start date of reference period",
      abstract="This is a Date: 1955-01-01",
      default="1955-01-01",
      type=type(date(1955,01,01)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.refEn = self.addLiteralInput(
      identifier="refEn",
      title="End date of reference period",
      abstract="This is a Date: 1985-12-31",
      default="1985-12-31",
      type=type(date(1985,12,31)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    
    self.tarout = self.addComplexOutput(
      identifier="tarout",
      title="Tarfile",
      abstract="tar archive containing the value tables",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      )

  def execute(self):

    import tempfile
    import tarfile
    # import wget
    from subprocess import call

    refSt = self.getInputValues(identifier='refSt')
    refEn = self.getInputValues(identifier='refEn')
    dateSt = self.getInputValues(identifier='dateSt')
    dateEn = self.getInputValues(identifier='dateEn')
    tar = tarfile.open(tarout_file, "w")
    
    start = min(refSt, refEn, dateSt, dateEn )
    end = max(refSt, refEn, dateSt, dateEn )
    
    for y in range(start.year , end.year +1 , 1): 
      url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.%s.nc' % (y)
      call(['wget', url]) # ["ls", "-l"])nc = wget.download(url)
      tar.add(('slp.%s.nc' % (y)))

    (fp_tar, tarout_file) = tempfile.mkstemp(dir=".", suffix='.tar')
    
    tar.close()
    self.tarout.setValue( tarout_file )