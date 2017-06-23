from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.indices import calc_indice_simple
from flyingpigeon.utils import GROUPING
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon import config
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class IndicessingleProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract="NetCDF Files or archive (tar/zip) containing netCDF files.",
                         min_occurs=1,
                         max_occurs=1000,
                         #  maxmegabites=5000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput("indices", "Index",
                         abstract='Climate index code.',
                         default='TG',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=indices()
                         ),

            LiteralInput("grouping", "Grouping",
                         abstract="Temporal group over which the index is computed.",
                         default='yr',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=len(GROUPING),
                         allowed_values=GROUPING
                         ),

            LiteralInput('region', 'Region',
                         data_type='string',
                         # abstract= countries_longname(), # need to handle special non-ascii char in countries.
                         abstract="Country code, see ISO-3166-3:\
                          https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements",
                         min_occurs=0,
                         max_occurs=len(countries()),
                         # default='DEU',
                         allowed_values=countries()),  # REGION_EUROPE #COUNTRIES

            LiteralInput("mosaic", "Mosaic",
                         abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file",
                         default='0',
                         data_type='boolean',
                         min_occurs=0,
                         max_occurs=1,
                         ),

        ]

        outputs = [
            ComplexOutput("output_archive", "Tar archive",
                          abstract="Tar archive of the netCDF files storing the index values.",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            ComplexOutput('ncout', 'Example netCDF file',
                          abstract="NetCDF file storing the index computed over one dataset.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),


            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")])
        ]

        super(IndicessingleProcess, self).__init__(
            self._handler,
            identifier="indices_single",
            title="Climate indices (Single variable)",
            version="0.10",
            abstract="Climate index calculated from one daily input variable.",
            metadata=[
                {'title': 'Doc',
                 'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/\
                 index.html#climate-indices'},
                {"title": "ICCLIM",
                 "href": "http://icclim.readthedocs.io/en/latest/"},
                {"title": "Percentile-based indices", "href": "http://flyingpigeon.readthedocs.io/en/\
                latest/descriptions/indices.html#percentile-based-indices"},
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        try:
            resources = archiveextract(
                resource=rename_complexinputs(request.inputs['resource']))

            indices = [inpt.data for inpt in request.inputs['indices']]
            grouping = [inpt.data for inpt in request.inputs['grouping']]

            if 'mosaic' in request.inputs:
                mosaic = request.inputs['mosaic'][0].data
            else:
                mosaic = False

            if 'region' in request.inputs:
                region = [inpt.data for inpt in request.inputs['region']]
            else:
                region = None

            LOGGER.debug("grouping %s " % grouping)
            LOGGER.debug("mosaic %s " % mosaic)
            LOGGER.debug('indices= %s ' % indices)
            LOGGER.debug('region %s' % region)
            LOGGER.debug('Nr of input files %s ' % len(resources))
        except:
            LOGGER.exception('failed to read in the arguments')


        response.update_status('starting: indices=%s, grouping=%s, num_files=%s'
                               % (indices,  grouping, len(resources)), 2)

        results = []

        from flyingpigeon.utils import sort_by_filename
        datasets = sort_by_filename(resources, historical_concatination=True)
        results = []
        try:
            for group in grouping:
                for indice in indices:
                    for key in datasets.keys():
                        try:
                            response.update_status('Dataset %s: %s' % (len(results)+1,  key), 10)

                            LOGGER.debug("group %s " % group)
                            LOGGER.debug("mosaic %s " % mosaic)
                            LOGGER.debug('indice %s ' % indice)
                            LOGGER.debug('region %s' % region)
                            LOGGER.debug('Nr of input files %s ' % len(datasets[key]))

                            result = calc_indice_simple(
                                resource=datasets[key],
                                mosaic=mosaic,
                                indice=indice,
                                polygons=region,
                                grouping=group,
                                # dir_output=path.curdir,
                                )
                            LOGGER.debug('result: %s' % result)
                            results.extend(result)
                        except:
                            LOGGER.exception('failed for %s', key)
        except:
            LOGGER.exception('Failed to calculate indices')
#         # if not results:
#         #     raise Exception("failed to produce results")
#         # response.update_status('num results %s' % len(results), 90)


        tarf = archive(results)

        response.outputs['output_archive'].file = tarf

        i = next((i for i, x in enumerate(results) if x), None)
        if i is None:
            i = "dummy.nc"
        response.outputs['ncout'].file = results[i]

#       response.update_status("done", 100)
        return response
