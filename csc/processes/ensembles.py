"""
Processes with cdo commands

Author: Nils Hempelmann (nils.hempelmann@hzg.de)
"""

#from malleefowl.process import WorkerProcess
import malleefowl.process

class ensembles(malleefowl.process.WorkerProcess):
    """This process calls cdo with operation on netcdf file"""
    def __init__(self):
        malleefowl.process.WorkerProcess.__init__(
            self,
            identifier = "de.csc.esgf.ensembles",
            title = "Ensembles Operations",
            version = "0.1",
            metadata=[
                {"title":"CDO ens","href":"https://code.zmaw.de/projects/cdo"},
                ],
            abstract="calling cdo operation to calculate ensembles operations",
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

        # netcdf input
        # -------------

        # defined in WorkflowProcess ...

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
        self.status.set(msg="starting cdo operator", percentDone=10, propagate=True)

        nc_files = self.get_nc_files()
        operator = self.operator_in.getValue()

        out_filename = self.mktempfile(suffix='.nc')
        try:
            cmd = ["cdo", operator]
            cmd.extend(nc_files)
            cmd.append(out_filename)
            self.cmd(cmd=cmd, stdout=True)
        except:
            self.message(msg='cdo failed', force=True)
        self.status.set(msg="ensembles calculation done", percentDone=90, propagate=True)
        self.output.setValue( out_filename )
