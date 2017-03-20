import os

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from flyingpigeon import config
from flyingpigeon.subset import masking
from flyingpigeon.utils import searchfile
from flyingpigeon.utils import search_landsea_mask
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class LandseamaskProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract="NetCDF Files or archive (tar/zip) containing netCDF files",
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput("threshold", "Threshold",
                         abstract="Percentage of Land Area",
                         default="50",
                         data_type='integer',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("mask", "Land Area Fraction File",
                         abstract="Optionally provide a OpenDAP URL to an appropriate Land Area Fraction File."
                                  " If no file is provided, the process will search an"
                                  " appropriate mask in the local cache.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("land_area", "Land/Sea",
                         abstract="If land_area (default) is checked, sea areas will be set to missing value",
                         default='1',
                         data_type='boolean',
                         )
        ]

        outputs = [
            ComplexOutput("output_archive", "Masked Files Archive",
                          abstract="Tar file of the masked netCDF files",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            ComplexOutput("output_example", "Example",
                          abstract="one example file to display in the WMS",
                          supported_formats=[Format("application/x-netcdf")],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")])
        ]

        super(LandseamaskProcess, self).__init__(
            self._handler,
            identifier="landseamask",
            title="Masking Land-Sea",
            version="0.3",
            abstract="Find the appropriate land_area fraction file and perform a"
                     " CDO division to mask either land or sea areas",
            metadata=[
                {"title": "Doc", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        resources = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        # masks = archiveextract(
        #     resource=rename_complexinputs(request.inputs['mask']))
        land_area = request.inputs['land_area'][0].data

        ncs = []
        for nc in resources:
            try:
                if 'mask' in request.inputs:
                    landsea_mask = request.inputs['mask'][0].data
                else:
                    landsea_mask = search_landsea_mask(nc)
                LOGGER.debug("using landsea_mask: %s", landsea_mask)
                prefix = 'masked{}'.format(os.path.basename(nc).replace('.nc', ''))
                nc_masked = masking(nc, landsea_mask, land_area=land_area, prefix=prefix)
                ncs.extend([nc_masked])
                LOGGER.info('masking processed for %s', nc)
            except:
                LOGGER.exception('failed to mask file: %s', nc)
        if not ncs:
            raise Exception("Could not mask input files. Maybe appropriate mask files are not available?")
        nc_archive = archive(ncs)

        response.outputs['output_archive'].file = nc_archive
        i = next((i for i, x in enumerate(ncs) if x), None)
        response.outputs['output_example'].file = ncs[i]

        response.update_status("done", 100)
        return response
