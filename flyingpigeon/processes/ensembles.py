from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class ensembles(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "ensembles",
            title = "Ensembles Operations",
            version = "0.1",
            metadata=[
                {"title":"CDO ens","href":"https://code.zmaw.de/projects/cdo"},
                ],
            abstract="calling cdo operation to calculate ensembles operations",
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

        # operators
        self.operator_in = self.addLiteralInput(
            identifier="operator",
            title="Ensemble command",
            abstract="Choose a CDO Operator",
            default="ensmean",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['ensmin', 'ensmax', 'enssum', 'ensmean', 'ensavg', 'ensvar', 'ensstd', 'enspctl']
            )

        # complex output
        # -------------

        self.output = self.addComplexOutput(
            identifier="output",
            title="NetCDF Output",
            abstract="NetCDF Output",
            metadata=[],
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True,
            )

    def execute(self):
        self.show_status("starting cdo operator", 0)

        nc_files = self.getInputValues(identifier='netcdf_file')
        operator = self.operator_in.getValue()

        out_filename = self.mktempfile(suffix='.nc')
        try:
            cmd = ["cdo", operator]
            cmd.extend(nc_files)
            cmd.append(out_filename)
            self.cmd(cmd=cmd, stdout=True)
        except:
            logger.exception('cdo failed')
            raise
        self.show_status("ensembles calculation done", 100)
        self.output.setValue( out_filename )
