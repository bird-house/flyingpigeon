from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class modelUncertainty(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "modelUncertainty",
            title="Robustness of modelled signal change",
            version = "0.1",
            metadata= [ {"title": "LSCE" , "href": "http://www.lsce.ipsl.fr/"} ],
            abstract="Calculates whether the magnitude of the ensemble mean is larger than the ensemble standard deviation.",
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

        self.ensmean = self.addComplexOutput(
            identifier="ensmean",
            title="ensemble mean",
            abstract="netCDF file containing ensemble mean",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )

        self.ensstd = self.addComplexOutput(
            identifier="ensstd",
            title="ensemble std",
            abstract="netCDF file containing ensemble std",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )
        
        self.absdelta = self.addComplexOutput(
            identifier="abs delta",
            title="magnitude of mean model change",
            abstract="netCDF file containing magnitude of mean model change",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         

        self.binmask = self.addComplexOutput(
            identifier="binmask",
            title="binary mask",
            abstract="netCDF mask file where mean model change magnitude greater than ensemble std",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )

    def execute(self):
        self.show_status('starting uncertainty process', 0)
    
        from flyingpigeon.modelUncertainty import modelUncertaintyWorker as muw
        
        ncfiles = self.getInputValues(identifier='resource')
        
        result, result2, result3, result4  = muw(ncfiles)        
        
        self.ensmean.setValue( result ) #ensemble mean

        self.ensstd.setValue( result2 ) #ensemble std

        self.absdelta.setValue( result3 ) #magnitude of model change    

        self.binmask.setValue( result4 ) #absdelta > std
            
        self.show_status('uncertainty process done', 99)