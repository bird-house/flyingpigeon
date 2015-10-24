from datetime import datetime, date
import tempfile
import subprocess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from netCDF4 import Dataset
import numpy as np

#from bokeh.plotting import *
from malleefowl.process import WPSProcess

class VisualisationProcess(WPSProcess):
    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "visualisation",
            title="Visualisation of netcdf files",
            version = "0.1",
            metadata= [
                    {"title": "Climate Service Center", "href": "http://www.climate-service-center.de/"}
                    ],
            abstract="Just testing a nice script to visualise some variables",
            #extra_metadata={
                #'esgfilter': 'variable:tas,variable:evspsbl,variable:hurs,variable:pr',  #institute:MPI-M, ,time_frequency:day
                #'esgquery': 'variable:tas AND variable:evspsbl AND variable:hurs AND variable:pr' # institute:MPI-M AND time_frequency:day 
                #},
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
            abstract="Variable to be expected in the input files",
            default=None,
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )
    
        self.plotout_spagetti = self.addComplexOutput(
            identifier="plotout_spagetti",
            title="Visualisation, Spagetti plot",
            abstract="Visualisation of single variables as a spagetti plot",
            formats=[{"mimeType":"application/html"}],
            asReference=True,
            )         

        self.plotout_uncertainty = self.addComplexOutput(
            identifier="plotout_uncertainty",
            title="Visualisation, Uncertainty plot",
            abstract="Visualisation of single variables ensemble mean with uncertainty",
            formats=[{"mimeType":"application/html"}],
            asReference=True,
            )         
            
    def execute(self):
        
        from flyingpigeon import visualisation as vs
        
        ncfiles = self.getInputValues(identifier='resource')
        var = self.variableIn.getValue()

        self.show_status('Spagetti plot for %s %s files' % (len(ncfiles), var), 7)
        
        plotout_spagetti_file    = vs.spaghetti(ncfiles , variable=var, title=var, dir_out=None)
        
        self.show_status('Uncertainty plot for %s %s files' % (len(ncfiles), var), 7)
        
        plotout_uncertainty_file = vs.uncertainty(ncfiles , variable=var, title=var, dir_out=None)
        
        
        self.plotout_spagetti.setValue( plotout_spagetti_file )
        self.plotout_uncertainty.setValue( plotout_uncertainty_file )
        
        self.show_status('visualisation done', 99)

