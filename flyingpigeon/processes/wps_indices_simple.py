from pywps.Process import WPSProcess

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.indices import calc_indice_simple
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.utils import GROUPING

import logging
logger = logging.getLogger(__name__)


class SingleIndicesProcess(WPSProcess):
    """
    This process calculates a climate indice for the given input netcdf files.
    """
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="indices_simple",
            title="Climate indices -- Simple",
            version="0.3",
            abstract="Climate indices based on one single input variable.",
            metadata=[
                {'title': 'Documentation', 'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#climate-indices'},
                {"title": "ICCLIM" , "href": "http://icclim.readthedocs.io/en/latest/"},
                {"title": "Simple Indices", "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/indices.html"}
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
            abstract=str(countries_longname()),
            default='DEU',
            type=type(''),
            minOccurs=0,
            maxOccurs=len(countries()),
            allowedValues=countries()
            )

        self.mosaik = self.addLiteralInput(
            identifier="mosaik",
            title="Mosaik",
            abstract="If Mosaik is checked, selected polygons be clipped as a mosaik for each input file",
            default=False,
            type=type(False),
            minOccurs=0,
            maxOccurs=1,
            )

        # complex output
        # -------------
        self.output = self.addComplexOutput(
            identifier="output",
            title="Indice",
            abstract="Calculated indice as NetCDF file",
            metadata=[],
            formats=[{"mimeType": "application/x-tar"}],
            asReference=True
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
        mosaik = self.mosaik.getValue()
        groupings = self.groupings.getValue()  # getInputValues(identifier='groupings')

        polygons = self.polygons.getValue()
        # if len(polygons)==0:
        #     polygons = None

        logger.debug('indices=%s', indices)
        logger.debug('groupings=%s', groupings)
        logger.debug('num files=%s', len(ncs))
        self.status.set('starting: indices=%s' % indices, 0)

        results = squeeze(calc_indice_simple(
            resource=ncs,
            mosaik=mosaik,
            indices=indices,
            polygons=polygons,
            groupings=groupings,
            dir_output=path.curdir,
            ))

        if not results:
            raise Exception("failed to produce results")

        self.status.set('indices calculated', 90)
        logger.debug('results type: %s', type(results))
        logger.debug('indices files: %s ' % results.tolist())

        try:

            archive_indices = archive(results.tolist())

            #(fp_tarf, tarf) = mkstemp(dir=".", suffix='.tar')
            #tar = tarfile.open(tarf, "w")

            #for result in results:
                #tar.add( result , arcname = os.path.basename(result))
            #tar.close()

            logger.info('archive prepared')
        except Exception as e:
            msg = "archive preparation failed"
            logger.exception(msg)
            raise Exception(msg)

        self.output.setValue(archive_indices)
        self.status.set('done', 100)
