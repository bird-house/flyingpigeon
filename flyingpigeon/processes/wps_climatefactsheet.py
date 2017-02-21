import os
import tarfile

from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.log import init_process_logger

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class climatefactsheetProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="climatefactsheet",
            title="Climate Fact Sheet Generator",
            version="0.1",
            abstract="Returns a pdf with a short overview of the climatological situation for the selected countries",
            metadata=[
                # {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Doc", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF Files or archive (tar/zip) containing netCDF files",
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=5000,
            formats=[{"mimeType": "application/x-netcdf"},
                     {"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            )

        self.region = self.addLiteralInput(
            identifier="region",
            title="Region",
            # abstract= countries_longname(), # need to handle special non-ascii char in countries.
            default='DEU',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(countries()),
            allowedValues=countries()  # REGION_EUROPE #COUNTRIES #
            )
        #
        # self.mosaic = self.addLiteralInput(
        #     identifier="mosaic",
        #     title="Mosaic",
        #     abstract="If Mosaic is checked, selected polygons will be merged to one Mosaic for each input file",
        #     default=False,
        #     type=type(False),
        #     minOccurs=0,
        #     maxOccurs=1,
        #     )

        # self.dimension_map = self.addLiteralInput(
        #     identifier="dimension_map",
        #     title="Dimension Map",
        #     abstract= 'if not ordered in lon/lat a dimension map has to be provided',
        #     type=type(''),
        #     minOccurs=0,
        #     maxOccurs=1
        #     )
        #
        # self.variable = self.addLiteralInput(
        #     identifier="variable",
        #     title="Variable",
        #     abstract="Variable to be expected in the input files (Variable will be detected if not set)",
        #     default=None,
        #     type=type(''),
        #     minOccurs=0,
        #     maxOccurs=1,
        #     )

        ###########
        # OUTPUTS
        ###########

        # self.output = self.addComplexOutput(
        #     title="Subsets",
        #     abstract="Tar archive containing the netCDF files",
        #     formats=[{"mimeType": "application/x-tar"}],
        #     asReference=True,
        #     identifier="output",
        #     )

        # self.output_netcdf = self.addComplexOutput(
        #     title="Subsets for one dataset",
        #     abstract="NetCDF file with subsets of one dataset.",
        #     formats=[{"mimeType": "application/x-netcdf"}],
        #     asReference=True,
        #     identifier="ncout",
        #     )

        self.output_factsheet = self.addComplexOutput(
            title="Climate Fact Sheet",
            abstract="PDF with a short overview of the climatological situation of the selected countries",
            formats=[{"mimeType": "application/pdf"}],
            asReference=True,
            identifier="output_factsheet",
            )

        self.output_log = self.addComplexOutput(
            identifier="output_log",
            title="Logging information",
            abstract="Collected logs during process run.",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
        )

    def execute(self):
        from flyingpigeon.utils import archive, archiveextract
        from tempfile import mkstemp

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        ncs = archiveextract(self.getInputValues(identifier='resource'))
        # mosaic = self.mosaic.getValue()
        regions = self.region.getValue()

        self.status.set('Arguments set for subset process', 0)
        logger.debug('starting: regions=%s, num_files=%s' % (len(regions), len(ncs)))

        try:
            from flyingpigeon.visualisation import plot_polygons
            png_country = plot_polygons(regions)
        except:
            logger.exception('failed to plot the polygon to world map')
            o1, png_country = mkstemp(dir='.', suffix='.png')

        # clip the demanded polygons
        from flyingpigeon.subset import clipping
        subsets = clipping(resource=ncs, variable=None,
                           dimension_map=None,
                           calc=None,
                           output_format='nc',
                           calc_grouping=None,
                           time_range=None,
                           time_region=None,
                           historical_concatination=True,
                           prefix=None,
                           spatial_wrapping='wrap',
                           polygons=regions,
                           mosaic=True
                           )
        try:
            from flyingpigeon.visualisation import uncertainty
            png_uncertainty = uncertainty(subsets)
        except:
            logger.exception('failed to generate the uncertainty plot')
            _, png_uncertainty = mkstemp(dir='.', suffix='.png')

        try:
            from flyingpigeon.visualisation import spaghetti
            png_spaghetti = spaghetti(subsets)
        except:
            logger.exception('failed to generate the spaghetti plot')
            _, png_spaghetti = mkstemp(dir='.', suffix='.png')

        try:
            from flyingpigeon import robustness as erob
            signal, low_agreement_mask, high_agreement_mask,  png_robustness, text_src = erob.method_A(
                # resource=subsets,
                # start=None, end=None,
                # timeslice=None,
                # variable=None
                )
        except:
            logger.exception('failed to generate the robustness plot')
            _, png_robustness = mkstemp(dir='.', suffix='.png')

        from flyingpigeon.visualisation import factsheetbrewer
        factsheet = factsheetbrewer(png_country=png_country,
                                    png_uncertainty=png_uncertainty,
                                    png_spaghetti=png_spaghetti,
                                    png_robustness=png_robustness)

        self.output_factsheet.setValue(factsheet)
        self.status.set('done', 100)
