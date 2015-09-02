from malleefowl.process import WPSProcess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

from flyingpigeon.indices import indices, indices_description, calc_indice
from flyingpigeon.subset import countries, countries_longname # COUNTRIES
from flyingpigeon.utils import GROUPING

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

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
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
            maxOccurs=len(GROUPING),
            allowedValues=GROUPING
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
            abstract="Select a country for polygon subset", #countries_longname
            default='FRA',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(countries()),
            allowedValues=countries() #REGION_EUROPE #COUNTRIES # 
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

        self.drs_filename = self.addLiteralOutput(
            identifier = "drs_filename",
            title = "DRS Filename",
            type = type(''))
        
    def execute(self):
        import os
        logger.debug('PYHONPATH = %s', os.environ['PYTHONPATH'])
        logger.debug('PATH = %s', os.environ['PATH'])

        ncs = self.getInputValues(identifier='resource')
        self.show_status('starting: indice=%s, grouping=%s, num_files=%s' % (self.indice.getValue(), self.grouping.getValue(), len(ncs)), 0)

        result = calc_indice(
            resource = ncs,
            indice = self.indice.getValue(),
            grouping = self.grouping.getValue(),
            out_dir = self.working_dir,
            )
        
        self.show_status('result %s' % result, 90)

        self.output.setValue( result )
        from os.path import basename
        self.drs_filename.setValue( basename(result) )

        self.show_status('done: indice=%s, num_files=%s' % (self.indice.getValue(), len(ncs)), 100)

