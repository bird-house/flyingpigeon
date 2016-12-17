import os
import tarfile

from flyingpigeon.subset import clipping
from flyingpigeon.subset import countries, countries_longname
from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class ClippingProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="subset_countries",
            title="Subset countries",
            version="0.9",
            abstract="Returns only the selected polygon for each input dataset",
            metadata=[
                {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
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

        self.mosaic = self.addLiteralInput(
            identifier="mosaic",
            title="Mosaic",
            abstract="If Mosaic is checked, selected polygons will be merged to one Mosaic for each input file",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )

        # self.dimension_map = self.addLiteralInput(
        #     identifier="dimension_map",
        #     title="Dimension Map",
        #     abstract= 'if not ordered in lon/lat a dimension map has to be provided',
        #     type=type(''),
        #     minOccurs=0,
        #     maxOccurs=1
        #     )

        self.variable = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable to be expected in the input files (Variable will be detected if not set)",
            default=None,
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        self.output = self.addComplexOutput(
            title="Subsets",
            abstract="Tar archive containing the netCDF files",
            formats=[{"mimeType": "application/x-tar"}],
            asReference=True,
            identifier="output",
            )

        self.output_netcdf = self.addComplexOutput(
            title="Subsets for one dataset",
            abstract="NetCDF file with subsets of one dataset.",
            formats=[{"mimeType": "application/x-netcdf"}],
            asReference=True,
            identifier="ncout",
            )

    def execute(self):
        from ast import literal_eval
        from flyingpigeon.utils import archive, archiveextract

        ncs = archiveextract(self.getInputValues(identifier='resource'))
        mosaic = self.mosaic.getValue()
        regions = self.region.getValue()
        variable = self.variable.getValue()

        # logger.info('regions: %s' % regions)
        # dimension_map = self.dimension_map.getValue()
        # if dimension_map != None:
        #     dimension_map = literal_eval(dimension_map)

        logger.info('ncs = %s', ncs)
        logger.info('regions = %s', regions)
        logger.info('mosaic = %s', mosaic)
        # logger.info('dimension_map = %s', dimension_map)

        self.status.set('Arguments set for subset process', 0)
        logger.debug('starting: regions=%s, num_files=%s' % (len(regions), len(ncs)))
        try:
            results = clipping(
                resource=ncs,
                polygons=regions,  # self.region.getValue(),
                mosaic=mosaic,
                spatial_wrapping='wrap',
                variable=variable,
                dir_output=os.path.abspath(os.curdir),
                # dimension_map=dimension_map,
                )
            logger.info('results %s' % results)
        except Exception as e:
            msg = 'clipping failed'
            logger.exception(msg)
            raise Exception(msg)

        if not results:
            raise Exception('no results produced.')

        # prepare tar file
        try:
            tarf = archive(results)
            logger.info('Tar file prepared')
        except Exception as e:
            msg = 'Tar file preparation failed'
            logger.exception(msg)
            raise Exception(msg)

        self.output.setValue(tarf)

        i = next((i for i, x in enumerate(results) if x), None)
        self.output_netcdf.setValue(results[i])

        self.status.set('done', 100)
