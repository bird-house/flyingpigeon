from pywps.Process import WPSProcess

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.indices import calc_indice_simple
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.utils import GROUPING

import logging
logger = logging.getLogger(__name__)


class SingleIndicesProcess(WPSProcess):
    """
    This process calculates climate indices for the given input datasets.
    """
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="indices_simple",
            title="Climate indices -- Simple",
            version="0.9",
            abstract="Climate indices based on one single input variable.",
            metadata=[
                {'title': 'Documentation',
                 'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#climate-indices'},
                {"title": "ICCLIM",
                 "href": "http://icclim.readthedocs.io/en/latest/"},
                {"title": "Simple Indices",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/indices.html"}
                ],
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resouce",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType": "application/x-netcdf"}],
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
            title="Index",
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
            abstract=str(countries_longname()),
            type=type(''),
            minOccurs=0,
            maxOccurs=len(countries()),
            allowedValues=countries()
            )

        self.mosaic = self.addLiteralInput(
            identifier="mosaic",
            title="Mosaic",
            abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )

        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Index",
            abstract="Calculated index as NetCDF file",
            metadata=[],
            formats=[{"mimeType": "application/x-tar"}],
            asReference=True
            )

        self.output_netcdf = self.addComplexOutput(
            title="one dataset as example",
            abstract="NetCDF file to be dispayed on WMS",
            formats=[{"mimeType": "application/x-netcdf"}],
            asReference=True,
            identifier="ncout",
            )

    def execute(self):
        import os
        from flyingpigeon.utils import archive
        # import tarfile
        from tempfile import mkstemp
        from os import path
        from numpy import squeeze

        ncs = self.getInputValues(identifier='resource')
        indices = self.indices.getValue()
        polygons = self.polygons.getValue()
        mosaic = self.mosaic.getValue()
        groupings = self.groupings.getValue()

        if polygons is None:
            self.status.set('No countries selected, entire domain will be calculated', 10)
        logger.debug('indices=%s', indices)
        logger.debug('groupings=%s', groupings)
        logger.debug('num files=%s', len(ncs))
        self.status.set('processing indices : %s' % indices, 12)

        results = squeeze(calc_indice_simple(
            resource=ncs,
            mosaic=mosaic,
            indices=indices,
            polygons=polygons,
            groupings=groupings,
            dir_output=path.curdir,
            ))
        results_list = results.tolist()
        self.status.set('indices calculated', 90)
        logger.debug('results type: %s', type(results_list))
        logger.debug('indices files: %s ' % results_list)

        try:
            archive_indices = archive(results_list)
            logger.info('archive prepared')
        except Exception as e:
            msg = "archive preparation failed"
            logger.exception(msg)
            raise Exception(msg)
        try:
            self.output.setValue(archive_indices)
            if type(results_list) == list:
                i = next((i for i, x in enumerate(results.tolist()) if x), None)
                self.output_netcdf.setValue(str(results[i]))
            elif type(results_list) == str:
                self.output_netcdf.setValue(results_list)
            else:
                logger.debug('results_list type: %s  not extractable ' % type(results_list))
                self.output_netcdf.setValue(None)
        except Exception as e:
            msg = "extraction of example file failed"
            logger.exception(msg)

        self.status.set('done', 100)
