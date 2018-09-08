import logging
import os

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger
from flyingpigeon.subset import masking
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
from eggshell.log import init_process_logger
from flyingpigeon.utils import search_landsea_mask_by_esgf

LOGGER = logging.getLogger("PYWPS")


class LandseamaskProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('dataset', 'Dataset',
                         abstract="URL pointing to a NetCDF File"
                                  " or an archive (tar/zip) containing NetCDF files.",
                         min_occurs=0,
                         max_occurs=100,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput('dataset_opendap', 'Remote OpenDAP Data URL',
                         data_type='string',
                         abstract="Remote OpenDAP data URL, for example:"
                                  " http://www.esrl.noaa.gov/psd/thredds/dodsC/Datasets/ncep.reanalysis2.dailyavgs/surface/mslp.2016.nc",
                         # noqa
                         metadata=[
                             Metadata(
                                 'application/x-ogc-dods',
                                 'https://www.iana.org/assignments/media-types/media-types.xhtml')],
                         min_occurs=0,
                         max_occurs=100),

            LiteralInput("threshold", "Threshold",
                         abstract="Land Area Fraction in percent.",
                         default="50",
                         data_type='integer',
                         allowed_values=[10, 25, 50, 75, 90],
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("mask", "Land Area Fraction File",
                         abstract="Optionally provide an OpenDAP URL to an appropriate land area fraction file."
                                  " If no file is provided, the process will run a search on the ESGF archive.",
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
            ComplexOutput("output_archive", "Tar archive",
                          abstract="Tar archive of the masked netCDF files.",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            ComplexOutput("output_example", "Example netCDF file",
                          abstract="An example file to display in the WMS.",
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
            title="Land-Sea Mask",
            version="0.3",
            abstract="Mask grid cells according to their land area fraction."
                     " This process uses the ESGF datastore to access an appropriate land/sea mask.",
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

        datasets = []
        # append file urls
        if 'dataset' in request.inputs:
            datasets.extend(archiveextract(
                resource=rename_complexinputs(request.inputs['dataset'])))
        # append opendap urls
        if 'dataset_opendap' in request.inputs:
            for dataset in request.inputs['dataset_opendap']:
                datasets.append(dataset.data)
        # land or sea flag
        land_area_flag = request.inputs['land_or_sea'][0].data == 'land'

        masked_datasets = []
        count = 0
        max_count = len(datasets)
        for ds in datasets:
            ds_name = os.path.basename(ds)
            LOGGER.info('masking dataset: {}'.format(ds_name))
            if 'mask' in request.inputs:
                landsea_mask = request.inputs['mask'][0].data
            else:
                landsea_mask = search_landsea_mask_by_esgf(ds)

            LOGGER.info("using landsea_mask: {}".format(landsea_mask))
            prefix = 'masked_{}'.format(ds_name.replace('.nc', ''))
            try:
                new_ds = masking(ds, landsea_mask, land_area=land_area_flag, prefix=prefix)
                masked_datasets.append(new_ds)

            except Exception as ex:
                msg = 'Could not subset dataset {}: {}'.format(ds_name, str(ex))
                LOGGER.exception(msg)
                raise Exception(msg)
            count = count + 1
            response.update_status("masked: {:d}/{:d}".format(count, max_count), int(100.0 * count / max_count))

        response.outputs['output_archive'].file = archive(masked_datasets)
        response.outputs['output_example'].file = masked_datasets[0]

        response.update_status("done", 100)
        return response
