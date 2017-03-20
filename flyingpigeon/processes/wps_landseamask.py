import os

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.inout.literaltypes import AllowedValue
from pywps.app.Common import Metadata

from flyingpigeon import config
from flyingpigeon.subset import masking
from flyingpigeon.utils import searchfile
from flyingpigeon.utils import search_landsea_mask_by_esgf
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class LandseamaskProcess(Process):
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

            LiteralInput("threshold", "Threshold",
                         abstract="Percentage of Land Area Fraction",
                         default="50",
                         data_type='integer',
                         allowed_values=AllowedValue(minval=1, maxval=100),
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("mask", "Land Area Fraction File",
                         abstract="Optionally provide an OpenDAP URL to an appropriate Land Area Fraction File."
                                  " If no file is provided, the process will search an"
                                  " appropriate mask in the local cache.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("land_or_sea", "Land or Sea",
                         abstract="Either the land or the sea area of the mask will be subsetted.",
                         default='land',
                         data_type='string',
                         allowed_values=['land', 'sea'],
                         min_occurs=1,
                         max_occurs=1,
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

        datasets = archiveextract(
            resource=rename_complexinputs(request.inputs['dataset']))
        land_area_flag = request.inputs['land_or_sea'][0].data == 'land'

        masked_datasets = []
        count = 0
        max_count = len(datasets)
        for ds in datasets:
            LOGGER.debug('masking dataset: %s', os.path.basename(ds))
            if 'mask' in request.inputs:
                landsea_mask = request.inputs['mask'][0].data
            else:
                landsea_mask = search_landsea_mask_by_esgf(ds)
            LOGGER.debug("using landsea_mask: %s", landsea_mask)
            prefix = 'masked_{}'.format(os.path.basename(ds).replace('.nc', ''))
            masked_datasets.append(masking(ds, landsea_mask, land_area=land_area_flag, prefix=prefix))
            count = count + 1
            response.update_status("masked: %d/%d".format(count, max_count), int(100.0 * count / max_count))

        response.outputs['output_archive'].file = archive(masked_datasets)
        response.outputs['output_example'].file = masked_datasets[0]

        response.update_status("done", 100)
        return response
