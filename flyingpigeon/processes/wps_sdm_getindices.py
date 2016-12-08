"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps.Process import WPSProcess
from flyingpigeon.sdm import _SDMINDICES_
import logging
logger = logging.getLogger(__name__)


class SDMgetindicesProcess(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="sdm_getindices",
            title="SDM -- calculation only indices",
            version="0.9",
            metadata=[
                {"title": "LWF", "href": "http://www.lwf.bayern.de/"},
                {"title": "Doc",
                    "href": "http://flyingpigeon.readthedocs.io/en/latest/\
                    descriptions/index.html#species-distribution-model"},
                {"title": "Paper",
                    "href": "http://www.hindawi.com/\
                    journals/jcli/2013/787250/"},
                {"title": "Tutorial",
                    "href": "http://flyingpigeon.readthedocs.io/en/latest/\
                    tutorials/sdm.html"},
                ],
            abstract="Indices preparation for SDM process",
            statusSupported=True,
            storeSupported=True
            )

        # Literal Input Data
        # ------------------
        self.resources = self.addComplexInput(
            identifier="resources",
            title="NetCDF File",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=500,
            maxmegabites=50000,
            formats=[{"mimeType": "application/x-netcdf"}],
            )

        self.input_indices = self.addLiteralInput(
            identifier="input_indices",
            title="Indices",
            abstract="Climate indices related to growth conditions \
                of tree species",
            default=['TG_JJA', 'TNn_Jan'],
            type=type(''),
            minOccurs=1,
            maxOccurs=10,
            allowedValues=_SDMINDICES_
            )

        self.archive_format = self.addLiteralInput(
            identifier="archive_format",
            title="Archive format",
            abstract="Result files will be compressed into archives. \
                Choose an appropriate format",
            default="tar",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['zip', 'tar']
            )

        ###########
        # OUTPUTS
        ###########

        self.output_indices = self.addComplexOutput(
            identifier="output_indices",
            title="Climate indices for growth conditions over all timesteps",
            abstract="Archive (tar/zip) containing calculated climate indices",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

    def execute(self):
        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive

        self.status.set('Start process', 0)

        try:
            logger.info('reading the arguments')
            resources = self.getInputValues(identifier='resources')
            indices = self.getInputValues(identifier='input_indices')
            logger.debug("indices = %s", indices)
            archive_format = self.archive_format.getValue()
        except Exception as e:
            logger.error('failed to read in the arguments %s ' % e)
        logger.info('indices %s ' % indices)

        #################################
        # calculate the climate indices
        #################################

        # indices calculation
        ncs_indices = None
        try:
            self.status.set('start calculation of climate indices for %s'
                            % indices, 30)
            ncs_indices = sdm.get_indices(resources=resources, indices=indices)
            logger.info('indice calculation done')
        except:
            msg = 'failed to calculate indices'
            logger.exception(msg)
            raise Exception(msg)

        # archive multiple output files to one archive file
        try:
            archive_indices = archive(ncs_indices, format=archive_format)
            logger.info('indices 3D added to tarfile')
        except:
            msg = 'failed adding indices to tar'
            logger.exception(msg)
            raise Exception(msg)

        self.output_indices.setValue(archive_indices)
        self.status.set('done', 100)
