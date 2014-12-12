from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon import indices_calculator
from flyingpigeon import dispel

class CalcMultipleIndices(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "multiple_indices",
            title="Calculation of multiple climate indices",
            version = "1.0",
            metadata=[],
            abstract="This process calculates multiple climate indices for the given input netcdf files."
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
            title="Indices",
            abstract="List of calculated indices.",
            metadata=[],
            formats=[{"mimeType":"text/json"}],
            asReference=True
            )
        
    def execute(self):
        resources = self.getInputValues(identifier='resource')
        indices = self.getInputValues(identifier='indice')

        self.show_status('starting: indice=%s, num_files=%s' % (indices, len(resources)), 0)

        results = dispel.climate_indice_workflow(
            resources = resources,
            indices = indices,
            grouping = self.grouping.getValue(),
            out_dir = self.working_dir,
            monitor=self.show_status,
            )

        self.show_status("publishing results ...", 99)
        
        files = [result.strip() for result in results]

        from malleefowl.publish import publish
        urls = publish(files)

        import json
        outfile = self.mktempfile(suffix='.txt')
        with open(outfile, 'w') as fp:
            json.dump(obj=urls, fp=fp, indent=4, sort_keys=True)
            self.output.setValue(outfile)
        
        self.show_status('done: indice=%s, num_files=%s' % (indices, len(resources)), 100)

