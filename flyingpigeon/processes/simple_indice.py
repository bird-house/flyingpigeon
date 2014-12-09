from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon import indices_calculator

class CalcIndice(WPSProcess):
    """This process calculates a climate indice for the given input netcdf files."""

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "simple_indice",
            title="Calculation of climate indice (simple)",
            version = "1.0",
            metadata=[],
            abstract="This process calculates a climate indice for the given input netcdf files."
            )

        indice_values = indices_calculator.indices()
        indice_abstract = indices_calculator.indices_description()

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.variable = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable used for calculation",
            default='tasmax',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=["tasmax", "tasmin", "tas", "pr", "prsn"]
            )
      
        self.grouping = self.addLiteralInput(
            identifier="grouping",
            title="Grouping",
            abstract="Select an aggregation grouping",
            default='year',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=["year", "month", "sem"]
            )

        self.indice = self.addLiteralInput(
            identifier="indice",
            title="Indice",
            abstract=indice_abstract,
            default='SU',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=indice_values
            )
      
        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Indice",
            abstract="Calculated indice as NetCDF file",
            metadata=[],
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True
            )
        
    def execute(self):
        self.show_status('starting ...', 0)

        resources = self.getInputValues(identifier='resource')

        result = indices_calculator.calc_indice(
            resources = resources,
            indice = self.indice.getValue(),
            variable = self.variable.getValue(),
            grouping = self.grouping.getValue(),
            out_dir = self.working_dir,
            )
        self.output.setValue( result )

        self.show_status('done', 100)
