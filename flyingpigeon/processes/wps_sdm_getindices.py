"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from flyingpigeon.sdm import _SDMINDICES_

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class SDMgetindicesProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('dataset', 'Dataset',
                         abstract="Enter either URL pointing to a NetCDF File"
                                  " or an archive (tar/zip) containing NetCDF files.",
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            # ComplexInput('resource', 'Resource',
            #              abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
            #              metadata=[Metadata('Info')],
            #              min_occurs=1,
            #              max_occurs=1000,
            #              supported_formats=[
            #                  Format('application/x-netcdf'),
            #                  Format('application/x-tar'),
            #                  Format('application/zip'),
            #              ]),

            LiteralInput("input_indices", "Indices",
                         abstract="Climate indices related to growth conditions \
                                    of tree species",
                         default=['TG_JJA', 'TNn_Jan'],
                         data_type='string',
                         min_occurs=1,
                         max_occurs=10,
                         allowed_values=_SDMINDICES_
                         ),

            LiteralInput("archive_format", "Archive format",
                         abstract="Result files will be compressed into archives. \
                                   Choose an appropriate format",
                         default="tar",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['zip', 'tar']
                         )
        ]

        outputs = [
            ComplexOutput("output_indices", "Climate indices for growth conditions over all timesteps",
                          abstract="Archive (tar/zip) containing calculated climate indices",
                          supported_formats=[Format('application/x-tar'),
                                             Format('application/zip')
                                             ],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          )
        ]

        super(SDMgetindicesProcess, self).__init__(
            self._handler,
            identifier="sdm_getindices",
            title="Species distribution Model (only indices calculation )",
            version="0.10",
            metadata=[
                Metadata("LWF", "http://www.lwf.bayern.de/"),
                Metadata(
                    "Doc",
                    "http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#species-distribution-model"),
                Metadata("paper",
                         "http://www.hindawi.com/journals/jcli/2013/787250/"),
                Metadata("Tutorial",
                         "http://flyingpigeon.readthedocs.io/en/latest/tutorials/sdm.html"),
            ],
            abstract="Indices preparation for SDM process",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        response.update_status('Start process', 0)

        try:
            LOGGER.info('reading the arguments')
            resources = archiveextract(
                resource=rename_complexinputs(request.inputs['dataset']))
            indices = request.inputs['input_indices']
            LOGGER.debug("indices = %s", indices)
            archive_format = request.inputs['archive_format']
        except:
            LOGGER.exception('failed to read in the arguments')
        LOGGER.info('indices %s ' % indices)

        #################################
        # calculate the climate indices
        #################################

        # indices calculation
        ncs_indices = None
        try:
            response.update_status('start calculation of climate indices for %s'
                                   % indices, 30)
            ncs_indices = sdm.get_indices(resources=resources, indices=indices)
            LOGGER.info('indice calculation done')
        except:
            msg = 'failed to calculate indices'
            LOGGER.exception(msg)
            raise Exception(msg)

        # archive multiple output files to one archive file
        try:
            archive_indices = archive(ncs_indices, format=archive_format)
            LOGGER.info('indices 3D added to tarfile')
        except:
            msg = 'failed adding indices to tar'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output_indices'].file = archive_indices
        response.update_status('done', 100)
        return response
