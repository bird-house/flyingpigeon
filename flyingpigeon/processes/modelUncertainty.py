from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class modelUncertainty(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "modelUncertainty",
            title="Calculation of model uncertainty",
            version = "0.1",
            metadata= [ {"title": "LSCE" , "href": "http://www.lsce.ipsl.fr/"} ],
            abstract="Calculates the ensemble mean and uncertainty mask",
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

        # output 
        
        self.output = self.addComplexOutput(
            identifier="output",
            title="ensemble mean and uncertainty",
            abstract="netCDF file containing calculated mean and uncertinty mask",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         

    def execute(self):
        self.show_status('starting uncertainty process', 0)
    
        from flyingpigeon.modelUncertainty import modelUncertaintyWorker as muw
        
        ncfiles = self.getInputValues(identifier='resource')
        
        result = muw(ncfiles)
        
        self.output.setValue( result )
            
        self.show_status('uncertainty process done', 99)       
