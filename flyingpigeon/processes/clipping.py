from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.clipping import REGION_EUROPE, calc_region_clipping

class CalcClipping(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "clipping",
            title="Calculation of region clipping",
            version = "1.0",
            metadata=[],
            abstract="This process returns only the given region from netcdf file."
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.region = self.addLiteralInput(
            identifier="region",
            title="Region",
            abstract="European Regions ...",
            default='FRA',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=REGION_EUROPE
            )
      
        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Output",
            abstract="Calculated region as NetCDF file",
            metadata=[],
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True
            )

    def execute(self):
        resources = self.getInputValues(identifier='resource')

        self.show_status('starting: region=%s, num_files=%s' % (self.region.getValue(), len(resources)), 0)

        result = calc_region_clipping(
            resources = resources,
            region = self.region.getValue(),
            out_dir = self.working_dir,
            )
        
        self.output.setValue( result.get('output') )

        self.show_status('done: region=%s, num_files=%s' % (self.region.getValue(), len(resources)), 100)

