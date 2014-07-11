"""
Processes with cdo commands

Author: Carsten Ehbrecht (ehbrecht@dkrz.de)
"""

#from malleefowl.process import WorkerProcess
import malleefowl.process

class HeavyPrecip(malleefowl.process.WorkerProcess):
    """write a comment   """
    def __init__(self):
        malleefowl.process.WorkerProcess.__init__(
            self,
            identifier = "de.csc.heavy_precip",
            title = "heavy precipitation",
            version = "0.1",
            metadata=[
                {"title":"CSC","href":"https://code.zmaw.de/projects/cdo"},
                ],
            abstract="computes precipitation above threshold ...",
            )


        # operators
        self.operator_in = self.addLiteralInput(
            identifier="operator",
            title="CDO Operator",
            abstract="Choose a CDO Operator",
            default="monmax",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['merge', 'dayavg', 'daymax', 'daymean', 'daymin', 'monmax', 'monmin', 'monmean', 'monavg']
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
        self.status.set(msg="starting precipitation calculation", percentDone=10, propagate=True)

        nc_files = self.get_nc_files()
        operator = self.operator_in.getValue()

        out_filename = self.mktempfile(suffix='.nc')
        try:
            cmd = ["cdo", operator]
            if operator == 'merge':
                cmd.extend(nc_files)
            else:
                cmd.append(nc_files[0])
            cmd.append(out_filename)
            self.cmd(cmd=cmd, stdout=True)
        except:
            self.message(msg='cdo failed', force=True)

        self.status.set(msg="precipitation calculation done", percentDone=90, propagate=True)
        self.output.setValue( out_filename )


