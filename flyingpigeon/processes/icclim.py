from malleefowl.process import WPSProcess
import subprocess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


class IndicesProcess(WPSProcess):
    """This process calculates the relative humidity"""

    def __init__(self):
        # definition of this process
        WPSProcess.__init__(self, 
            identifier = "indice",
            title="Climate indices",
            version = "0.1",
            metadata=[],
            abstract="Calculation of climate indices",
            # TODO: filter are configured in phoenix
            #extra_metadata={
                #'esgfilter': 'variable:tas, variable:evspsblpot, variable:huss, variable:ps, variable:pr, variable:sftlf, time_frequency:day', 
                #'esgquery': 'data_node:esg-dn1.nsc.liu.se' 
                #},
            ## extra_metadata={
            ##       'esgfilter': 'variable:tasmax, variable:tasmin, variable:tas, variable:pr, project:CMIP5, project:CORDEX',  
            ##       'esgquery': ' time_frequency:day' 
            ##       },
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

        # TXx, TXn, TNx, TNn, TR, CSU, GD4, FD, CFD, ID, HD17, CDD, CWD, RR, RR1, SDII, R10mm, R20mm, RX1day, RX5day, SD, SD1, SD5cm, SD50cm

        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="indice log",
            abstract="indice log",
            metadata=[],
            formats=[{"mimeType":"text/plain"}],
            asReference=True,
            )
    def execute(self):
        import os
        
        self.show_status('starting calcualtion of icclim indices', 0)

        ncfiles = self.getInputValues(identifier='netcdf_file')

        from csc import tools
        result = tools.indices(
            os.curdir, ncfiles,
            self.TG.getValue(),
            self.TX.getValue(),
            self.TN.getValue(),
            self.RR.getValue(),
            self.SU.getValue())

        outfile = self.mktempfile(suffix='.txt')
        
        with open(outfile, 'w') as fp:
            fp.write(result)
            
        self.output.setValue( outfile )
        
        self.show_status("processing done", 100)
        
        
            
