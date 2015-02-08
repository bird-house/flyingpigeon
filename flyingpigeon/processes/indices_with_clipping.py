from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.clipping import REGION_EUROPE
from flyingpigeon.indices import indices, indices_description
from flyingpigeon.workflow import calc_indice_with_clipping

class CalcIndicesWithClipping(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "indices_clipping",
            title="Calculation of multiple climate indices with clipping",
            version = "1.0",
            metadata=[],
            abstract="This process calculates multiple climate indices for the given input netcdf files and clips regions."
            )

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
            title="Time Aggregation",
            abstract="Select time aggegation",
            default='year',
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=["yr", "mon", "sem", "ONDJFM", "AMJJAS", "DJF", "MAM", "JJA", "SON" ]
            )

        self.indice = self.addLiteralInput(
            identifier="indice",
            title="Indice",
            abstract=indices_description(),
            default='SU',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(indices()),
            allowedValues=indices()
            )

        self.region = self.addLiteralInput(
            identifier="region",
            title="Region",
            abstract="European Regions ...",
            default='FRA',
            type=type(''),
            minOccurs=1,
            allowedValues=REGION_EUROPE
            )

        self.start_date = self.addLiteralInput(
            identifier="start",
            title="Start",
            abstract="Start date: 2001-01-01",
            type=type("2001-01-01"),
            minOccurs=0,
            maxOccurs=1,
            )

        self.end_date = self.addLiteralInput(
            identifier="end",
            title="End",
            abstract="End date: 2005-12-31",
            type=type("2005-12-31"),
            minOccurs=0,
            maxOccurs=1,
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
        self.status_log = self.addComplexOutput(
            identifier="status_log",
            title="Status Logfile",
            abstract="Status of processed files.",
            metadata=[],
            formats=[{"mimeType":"text/text"}],
            asReference=True
            )
        
    def execute(self):
        resources = self.getInputValues(identifier='resource')
        indice_list = self.getInputValues(identifier='indice')
        region_list = self.getInputValues(identifier='region')

        self.show_status('starting: indice=%s, num_files=%s' % (indice_list, len(resources)), 0)

        results,status_log = calc_indice_with_clipping(
            resources = resources,
            indices = indice_list,
            regions = region_list,
            grouping = self.grouping.getValue(),
            start_date = self.start_date.getValue(),
            end_date = self.end_date.getValue(),
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

        outfile = self.mktempfile(suffix='.txt')
        with open(outfile, 'w') as fp:
            for status in status_log:
                fp.write("%s\n" % status)
            self.status_log.setValue(outfile)
        
        self.show_status('done: indice=%s, num_files=%s' % (indice_list, len(resources)), 100)

