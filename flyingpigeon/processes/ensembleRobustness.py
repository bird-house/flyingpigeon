from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class modelUncertainty(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "ensembleRobustness",
            title="Calculation of the robustness of an ensemle",
            version = "0.1",
            metadata= [ {"title": "LSCE" , "href": "http://www.lsce.ipsl.fr/"} ],
            abstract="Calculates the robustness as the ratio of noise to signal in an ensemle of timeseries",
            )
        
        # input arguments    
        self.resource = self.addComplexInput(
            identifier="resource",
            title="NetCDF Files",
            abstract="NetCDF Files",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.start = self.addLiteralInput(
          identifier="start",
          title="Start Year",
          abstract="Beginn of the analysed period (e.g 1971; if not set, the first consistend year of the ensemble will be taken)",
          type=type("1"),
          #default='1950',
          minOccurs=0,
          maxOccurs=1,
          #allowedValues=range(1900,2200)
          )
  
        self.end = self.addLiteralInput(
          identifier="end",
          title="End Year",
          abstract="End of the analysed period (e.g. 2050 if not set, the last consistend year of the ensemble will be taken)",
          type=type("1"),
          #default='1950',
          minOccurs=0,
          maxOccurs=1,
          #allowedValues=range(1900,2200)
          )
        
        self.timeslice = self.addLiteralInput(
          identifier="timeslice",
          title="Time slice",
          abstract="Time slice (in years) for robustness reference (default=10))",
          type=type("1"),
          default='10',
          minOccurs=0,
          maxOccurs=1,
          #allowedValues=range(1,50)
          )
        
        # output 
        
        self.output_high = self.addComplexOutput(
            identifier="output_high",
            title="Mask for areas with high agreement",
            abstract="netCDF file containing calculated robustness mask",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         
        
        self.output_low = self.addComplexOutput(
            identifier="output_low",
            title="Mask for areas with low agreement",
            abstract="netCDF file containing calculated robustness mask",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         


        self.output_signal = self.addComplexOutput(
            identifier="output_signal",
            title="Signal",
            abstract="netCDF file containing calculated change of mean over the timeperiod and ensemble members",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         

    def execute(self):
        self.show_status('starting uncertainty process', 0)
    
        from flyingpigeon import ensembleRobustness as erob
        
        ncfiles = self.getInputValues(identifier='resource')
        start = self.getInputValues(identifier='start')
        end = self.getInputValues(identifier='end')
        timeslice = self.getInputValues(identifier='timeslice')
        
        signal , low_agreement_mask , high_agreement_mask = erob.worker(resource=ncfiles, start=None, end=None, timeslice=10)
        
        self.output_signal.setValue( signal )
        self.output_high.setValue( high_agreement_mask )
        self.output_low.setValue( low_agreement_mask )
        
        self.show_status('uncertainty process done', 99)       
