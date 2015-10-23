from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.indices import indices, indices_description, calc_indice_single
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.utils import GROUPING

class IndiceSingle(WPSProcess):
    """This process calculates a climate indice for the given input netcdf files."""
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "indices_single",
            title="Calculation of climate indice (single variable)",
            version = "0.1",
            metadata=[],
            abstract="This process calculates climate indices based on one single variable."
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )
    
        self.groupings = self.addLiteralInput(
            identifier="groupings",
            title="Grouping",
            abstract="Select an time grouping (time aggregation)",
            default='yr',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(GROUPING),
            allowedValues=GROUPING
            )

        self.indices = self.addLiteralInput(
            identifier="indices",
            title="Indice",
            abstract=indices_description(),
            default='SU',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(indices()),
            allowedValues=indices()
            )
        
        self.polygons = self.addLiteralInput(
            identifier="polygons",
            title="Country subset",
            abstract= countries_longname(), 
            default='FRA',
            type=type(''),
            minOccurs=0,
            maxOccurs=len(countries()),
            allowedValues=countries() 
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
        import os
        import tarfile
        from tempfile import mkstemp
        from os import path
        
        logger.debug('PYHONPATH = %s', os.environ['PYTHONPATH'])
        logger.debug('PATH = %s', os.environ['PATH'])

        ncs       = self.getInputValues(identifier='resource')
        indices   = self.getInputValues(identifier='indices')
        polygons  = self.polygons.getValue() 
        groupings = self.getInputValues(identifier='groupings')

        polygons = self.polygons.getValue()
        if len(polygons)>1: 
            polygons = None

        self.show_status('starting: indices=%s, groupings=%s, num_files=%s' % (indices, 
            groupings, len(ncs)), 0)

        results = calc_indice_single(
            resource = ncs,
            indices = indices,
            polygons= polygons,
            groupings = groupings,
            dir_output = self.working_dir,
            )
        
        self.show_status('result %s' % results, 90)
        try: 
            (fp_tarf, tarf) = mkstemp(dir=".", suffix='.tar')
            tar = tarfile.open(tarf, "w")

            for result in results: 
                tar.add( result , arcname = result.replace(path.abspath(path.curdir), ""))
            tar.close()

            logger.info('Tar file prepared')
        except Exception as e:
            logger.error('Tar file preparation failed %s' % e)

        self.output.setValue( tarf )
        self.show_status('done: indice=%s, num_files=%s' % (indices, len(ncs)), 100)