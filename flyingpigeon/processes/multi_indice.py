from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon import indices_calculator
from flyingpigeon import dispel

class CalcMultipleIndices(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "multi_indice",
            title="Calculation of climate multiple indices",
            version = "1.0",
            metadata=[],
            abstract="This process calculates a multiple climate indices for the given input netcdf files."
            )

        indice_values = indices_calculator.indices()
        num_indices = len(indice_values)
        indice_abstract = indices_calculator.indices_description()

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=1024,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
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
            maxOccurs=num_indices,
            allowedValues=indice_values
            )
      
        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Indice",
            abstract="Calculated indice as NetCDF file",
            metadata=[],
            formats=[{"mimeType":"text/text"}],
            asReference=True
            )
        
    def execute(self):
        resources = self.getInputValues(identifier='resource')
        indices = self.getInputValues(identifier='indice')

        self.show_status('starting: indice=%s, num_files=%s' % (indices, len(resources)), 0)

        import tempfile
        
        result = dispel.climate_indice_workflow(
            url = 'http://localhost:8093/wps',
            resources = resources,
            indices = indices,
            grouping = self.grouping.getValue(),
            #out_dir = self.working_dir,
            out_dir = tempfile.mkdtemp(),
            monitor=self.show_status,
            )
        self.output.setValue( result )

        self.show_status('done: indice=%s, num_files=%s' % (indices, len(resources)), 100)

