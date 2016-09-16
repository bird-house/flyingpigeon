from flyingpigeon import visualisation as vs
from pywps.Process import WPSProcess

class plottimeseriesProcess(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "plot_timeseries",
            title="Plots -- timeseries",
            version = "0.9",
            metadata= [
                    {"title": 'Plots timeseries'}
                    ],
            abstract="Outputs some timeseries of the file field means. Spaghetti and uncertainty plot",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="NetCDF Files",
            abstract="NetCDF Files",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.variableIn = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable to be expected in the input files (variable will be detected if not set)",
            default=None,
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )
    
        self.plotout_spagetti = self.addComplexOutput(
            identifier="plotout_spagetti",
            title="Visualisation, Spaghetti plot",
            abstract="Visualisation of single variables as a spaghetti plot",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )         

        self.plotout_uncertainty = self.addComplexOutput(
            identifier="plotout_uncertainty",
            title="Visualisation, Uncertainty plot",
            abstract="Visualisation of single variables ensemble mean with uncertainty",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )         
            
    def execute(self):
        ncfiles = self.getInputValues(identifier='resource')
        var = self.variableIn.getValue()

        self.status.set('plotting variable %s' % var, 10)

        plotout_spagetti_file = vs.spaghetti(ncfiles , variable=var, title='Fieldmean of %s ' % var, dir_out=None)
        
        self.status.set('Spagetti plot for %s %s files done' % (len(ncfiles), var), 50)
        
        plotout_uncertainty_file = vs.uncertainty(ncfiles , variable=var, title='Ensemble uncertainty for %s ' % dir_out=None)

        self.status.set('Uncertainty plot for %s %s files done' % (len(ncfiles), var), 90)
        
        self.plotout_spagetti.setValue( plotout_spagetti_file )
        self.plotout_uncertainty.setValue( plotout_uncertainty_file )
        
        self.status.set('visualisation done', 100)

