import logging

from pywps import ComplexInput, ComplexOutput, Format, LiteralInput, Process
from pywps.app.Common import Metadata

from flyingpigeon.subset import clipping
from flyingpigeon.subset import countries
from eggshell.utils import archive, extract_archive
# from eggshell.utils import rename_complexinputs

LOGGER = logging.getLogger("PYWPS")


# TODO: Rename this to "SubsetcountryProcess"
class SubsetcountryProcess(Process):
    """
    TODO: opendap input support, additional metadata to display region names.
    """

    def __init__(self):
        inputs = [
            LiteralInput('region', 'Region',
                         data_type='string',
                         # abstract= countries_longname(),
                         # need to handle special non-ascii char in countries.
                         abstract="Country code, see ISO-3166-3:\
                          https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements",
                         min_occurs=1,
                         max_occurs=len(countries()),
                         default='DEU',
                         allowed_values=countries()),

            LiteralInput('mosaic', 'Union of multiple regions',
                         data_type='boolean',
                         abstract="If True, selected regions will be merged"
                                  " into a single geometry.",
                         min_occurs=0,
                         max_occurs=1,
                         default=False),

            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files or archive (tar/zip) containing NetCDF files.',
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

            # ComplexOutput('output_log', 'Logging information',
            #               abstract="Collected logs during process run.",
            #               as_reference=True,
            #               supported_formats=[Format('text/plain')]
            #               )
        ]

        super(SubsetcountryProcess, self).__init__(
            self._handler,
            identifier="subset_countries",
            title="Subset Countries",
            version="0.11",
            abstract="Return the data whose grid cells intersect the selected countries for each input dataset.",
            metadata=[
                Metadata('Doc', 'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#subset-processes'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        # init_process_logger('log.txt')
        # response.outputs['output_log'].file = 'log.txt'

        # input files
        LOGGER.debug('url={}, mime_type={}'.format(
            request.inputs['resource'][0].url,
            request.inputs['resource'][0].data_format.mime_type))
        ncs = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource']],
            dir_output=self.workdir)
        # mime_type=request.inputs['resource'][0].data_format.mime_type)
        # mosaic option
        # TODO: fix defaults in pywps 4.x
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False
        # regions used for subsetting
        regions = [inp.data for inp in request.inputs['region']]

        LOGGER.info('ncs={}'.format(ncs))
        LOGGER.info('regions={}'.format(regions))
        LOGGER.info('mosaic={}'.format(mosaic))

        response.update_status("Arguments set for subset process", 0)
        LOGGER.debug('starting: regions={}, num_files={}'.format(len(regions), len(ncs)))

        try:
            results = clipping(
                resource=ncs,
                polygons=regions,  # self.region.getValue(),
                mosaic=mosaic,
                spatial_wrapping='wrap',
                # variable=variable,
                dir_output=self.workdir,
                # dimension_map=dimension_map,
            )
            LOGGER.info('results %s' % results)
        except Exception as ex:
            msg = 'Clipping failed: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        if not results:
            raise Exception('No results produced.')

        # prepare tar file
        try:
            tarf = archive(results, dir_output=self.workdir)
            LOGGER.info('Tar file prepared.')
        except Exception as ex:
            msg = 'Tar file preparation failed: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output'].file = tarf

        i = next((i for i, x in enumerate(results) if x), None)
        response.outputs['ncout'].file = results[i]

        response.update_status("done", 100)
        return response
