import os
import tarfile

from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.log import init_process_logger

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class factsheetgeneratorProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="factsheetgenerator",
            title="Fact Sheet Generator",
            version="0.1",
            abstract="Returns a pdf with a short overview of ",
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

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        ncs = archiveextract(self.getInputValues(identifier='resource'))
        # mosaic = self.mosaic.getValue()
        regions = self.region.getValue()

        self.status.set('Arguments set for subset process', 0)
        logger.debug('starting: regions=%s, num_files=%s' % (len(regions), len(ncs)))

        try:
            import shapefile as shp
            import matplotlib.pyplot as plt
            from os.path import join
            from tempfile import mkstemp

            from flyingpigeon import config
            from flyingpigeon.subset import get_ugid
            DIR_SHP = config.shapefiles_dir()

            sf = shp.Reader(join(DIR_SHP, "countries.shp"))

            ugid = get_ugid(polygons=regions, geom='countries')
            logger.debug("selcted ugids: %s " % ugid)

            fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
            o1, factsheet_plot = mkstemp(dir='.', suffix='.pdf')

            for c, shape in enumerate(sf.shapeRecords()):
                if c in ugid:
                    x = [i[0] for i in shape.shape.points[:]]
                    y = [i[1] for i in shape.shape.points[:]]
                    plt.plot(x, y)

            fig.savefig(factsheet_plot)
            plt.close()

        except:
            logger.exception('failed to generate the fact sheet')
            o1, factsheet_plot = mkstemp(dir='.', suffix='.pdf')
        # try:
        #     results = clipping(
        #         resource=ncs,
        #         polygons=regions,  # self.region.getValue(),
        #         mosaic=mosaic,
        #         spatial_wrapping='wrap',
        #         variable=variable,
        #         dir_output=os.path.abspath(os.curdir),
        #         # dimension_map=dimension_map,
        #         )
        #     logger.info('results %s' % results)
        # except Exception as e:
        #     msg = 'clipping failed'
        #     logger.exception(msg)
        #     raise Exception(msg)

        # if not results:
        #     raise Exception('no results produced.')

        # prepare tar file
        # try:
        #     tarf = archive(results)
        #     logger.info('Tar file prepared')
        # except Exception as e:
        #     msg = 'Tar file preparation failed'
        #     logger.exception(msg)
        #     raise Exception(msg)
        #
        # self.output.setValue(tarf)
        #
        # i = next((i for i, x in enumerate(results) if x), None)
        # self.output_netcdf.setValue(results[i])

        self.output_factsheet.setValue(factsheet_plot)
        self.status.set('done', 100)
