from flyingpigeon.subset import clipping
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class ClippingProcess(Process):
    def __init__(self):
        inputs = [
            LiteralInput('region', 'Region',
                         data_type='string',
                         # abstract= countries_longname(), # need to handle special non-ascii char in countries.
                         abstract="Country code, see ISO-3166-3:\
                          https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements",
                         min_occurs=1,
                         max_occurs=len(countries()),
                         default='DEU',
                         allowed_values=countries()),  # REGION_EUROPE #COUNTRIES

            LiteralInput('mosaic', 'Union of multiple regions',
                         data_type='boolean',
                         abstract="If True, selected regions will be merged"
                                  " into a single geometry.",
                         min_occurs=0,
                         max_occurs=1,
                         default=False),

            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files or archive (tar/zip) containing NetCDF files.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),
        ]

        outputs = [
            ComplexOutput('output', 'Tar archive',
                          abstract="Tar archive of the subsetted netCDF files.",
                          as_reference=True,
                          supported_formats=[Format('application/x-tar')]
                          ),

            ComplexOutput('ncout', 'Example netCDF file',
                          abstract="NetCDF file with subset for one dataset.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          )
        ]

        super(ClippingProcess, self).__init__(
            self._handler,
            identifier="subset_countries",
            title="Subset (World Countries)",
            version="0.10",
            abstract="Return the data whose grid cells intersect the selected countries for each input dataset.",
            metadata=[
                Metadata('LSCE', 'http://www.lsce.ipsl.fr/en/index.php'),
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        # input files
        LOGGER.debug("url=%s, mime_type=%s",
                     request.inputs['resource'][0].url,
                     request.inputs['resource'][0].data_format.mime_type)
        ncs = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        # mime_type=request.inputs['resource'][0].data_format.mime_type)
        # mosaic option
        # TODO: fix defaults in pywps 4.x
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False
        # regions used for subsetting
        regions = [inp.data for inp in request.inputs['region']]

        LOGGER.info('ncs = %s', ncs)
        LOGGER.info('regions = %s', regions)
        LOGGER.info('mosaic = %s', mosaic)

        response.update_status("Arguments set for subset process", 0)
        LOGGER.debug('starting: regions=%s, num_files=%s', len(regions), len(ncs))

        try:
            results = clipping(
                resource=ncs,
                polygons=regions,  # self.region.getValue(),
                mosaic=mosaic,
                spatial_wrapping='wrap',
                # variable=variable,
                # dir_output=os.path.abspath(os.curdir),
                # dimension_map=dimension_map,
            )
            LOGGER.info('results %s' % results)
        except:
            msg = 'clipping failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        if not results:
            raise Exception('no results produced.')

        # prepare tar file
        try:
            tarf = archive(results)
            LOGGER.info('Tar file prepared')
        except:
            msg = 'Tar file preparation failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output'].file = tarf

        i = next((i for i, x in enumerate(results) if x), None)
        response.outputs['ncout'].file = results[i]

        response.update_status("done", 100)
        return response
