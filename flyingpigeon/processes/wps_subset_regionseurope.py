import os
import tarfile

from flyingpigeon.subset import clipping
from flyingpigeon.subset import _EUREGIONS_ #countries, countries_longname
europeanregions = _EUREGIONS_.keys()
europeanregions.sort()

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class subset_regionseuropeProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "subset_regionseurope",
            title="Subset European Regions",
            version = "0.3",
            abstract="Returns the selected European administrative region defined in the GADM database (v2.5) for each input dataset.",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )
       
        self.region = self.addLiteralInput(
            identifier="region",
            title="Region",
            #abstract= countries_longname(), # need to handle special non-ascii char in countries.
            default='DE.MV',
            type=type(''),
            minOccurs=1,
            maxOccurs=len(europeanregions),
            allowedValues=europeanregions
            )


        # self.dimension_map = self.addLiteralInput(
        #     identifier="dimension_map",
        #     title="Dimension Map",
        #     abstract= 'If not ordered in lon/lat, a dimension map has to be provided.',
        #     type=type(''),
        #     minOccurs=0,
        #     maxOccurs=1
        #     )

        self.variable = self.addLiteralInput(
            identifier="variable",
            title="Variable",
            abstract="Variable to be expected in the input files (variable will be detected if not set).",
            default=None,
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        self.mosaic = self.addLiteralInput(
            identifier="mosaic",
            title="Mosaic",
            abstract="If Mosaic is checked, selected polygons will be clipped as a mosaic for each input file.",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )

        self.output = self.addComplexOutput(
            title="Subsets",
            abstract="Tar archive containing the netCDF files",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            identifier="output",
            )

    def execute(self):
        from ast import literal_eval

        urls = self.getInputValues(identifier='resource')
        mosaic = self.mosaic.getValue()
        regions = self.region.getValue()
        variable = self.variable.getValue()
        
        #logger.info('regions: %s' % regions)

        # dimension_map = self.dimension_map.getValue()
        # if dimension_map != None: 
        #     dimension_map = literal_eval(dimension_map)

        logger.info('urls = %s', urls)
        logger.info('regions = %s', regions)
        logger.info('mosaic = %s', mosaic)
        # logger.info('dimension_map = %s', dimension_map)
    
        self.status.set('Arguments set for subset process', 0)

        logger.debug('starting: regions=%s, num_files=%s' % (len(regions), len(urls)))

        try:
            results = clipping(
                resource = urls,
                polygons = regions, # self.region.getValue(),
                mosaic = mosaic,
                variable = variable, 
                dir_output = os.path.abspath(os.curdir),
              #  dimension_map=dimension_map,
                )

        except Exception as e:
            logger.exception('clipping failed')
            self.status.set('clipping failed')
        # prepare tar file 
        try:
            from flyingpigeon.utils import archive
            tarf = archive(results)
            logger.info('Tar file prepared')
        except Exception as e:
            logger.exception('Tar file preparation failed')
            raise

        self.output.setValue( tarf )
        self.status.set('done', 100)
