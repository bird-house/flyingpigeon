import logging

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.indices import calc_indice_percentile
from flyingpigeon.subset import countries, countries_longname, clipping
from flyingpigeon.utils import GROUPING
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon import config
# from eggshell.log import init_process_logger
from flyingpigeon.log import init_process_logger
from flyingpigeon.subset import countries, clipping
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs

LOGGER = logging.getLogger("PYWPS")


class IndicespercentiledaysProcess(Process):
    """
    TODO: need a more detailed description and an example.
    TODO: data input might need a data selection filter? metadata attributes could be used for this.
    """

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

            # LiteralInput("indices", "Index",
            #              abstract='Select an index',
            #              default='TG',
            #              data_type='string',
            #              min_occurs=1,
            #              max_occurs=1,  # len(indices()),
            #              allowed_values=['TG', 'TN', 'TX'],  # indices()
            #              ),

            LiteralInput("percentile", "Percentile",
                         abstract='Percentile value [1-100].',
                         default='90',
                         data_type='integer',
                         min_occurs=1,
                         max_occurs=1,  # len(indices()),
                         allowed_values=range(1, 100),  # indices()
                         ),

            # LiteralInput("refperiod", "Reference Period",
            #              abstract="Time refperiod to retrieve the percentile level",
            #              default="19700101-20101231",
            #              data_type='string',
            #              min_occurs=0,
            #              max_occurs=1,
            #              ),
            #
            # self.refperiod = self.addLiteralInput(
            #     identifier="refperiod",
            #     title="Reference refperiod",
            #     abstract="Reference refperiod for climate condition (all = entire timeserie)",
            #     default=None,
            #     type=type(''),
            #     minOccurs=0,
            #     maxOccurs=1,
            #     allowedValues=['all','1951-1980', '1961-1990', '1971-2000','1981-2010']
            #     )

            # LiteralInput("grouping", "Grouping",
            #              abstract="Select an time grouping (time aggregation)",
            #              default='yr',
            #              data_type='string',
            #              min_occurs=1,
            #              max_occurs=1,
            #              allowed_values=GROUPING
            #              ),

            LiteralInput('region', 'Region',
                         data_type='string',
                         abstract="Country code, see ISO-3166-3:\
                          https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements",
                         min_occurs=0,
                         max_occurs=len(countries()),
                         allowed_values=countries()),  # REGION_EUROPE #COUNTRIES

            LiteralInput("mosaic", "Mosaic",
                         abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file.",
                         default='0',
                         data_type='boolean',
                         min_occurs=0,
                         max_occurs=1,
                         ),

        ]

        outputs = [
            ComplexOutput("output_archive", "Tar archive",
                          abstract="Tar archive of the netCDF files storing the percentile values.",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            ComplexOutput('ncout', 'Example netCDF file',
                          abstract="NetCDF file storing the percentiles computed over one dataset.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")])
        ]

        super(IndicespercentiledaysProcess, self).__init__(
            self._handler,
            identifier="indices_percentiledays",
            title="Climate indices (Daily percentiles)",
            version="0.10",
            abstract="Climatological percentile for each day of the year "
                     "computed over the entire dataset.",
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

            if 'region' in request.inputs:
                region = request.inputs['region'][0].data
            else:
                region = None

            if 'mosaic' in request.inputs:
                mosaic = request.inputs['mosaic'][0].data
            else:
                mosaic = False

            percentile = request.inputs['percentile'][0].data

            LOGGER.debug('mosaic: {}'.format(mosaic))
            LOGGER.debug('percentile: {}'.format(percentile))
            LOGGER.debug('region: {}'.format(region))
            LOGGER.debug('Nr of input files: {}'.format(len(resources)))

        except Exception as ex:
            msg = 'failed to read in the arguments: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        from flyingpigeon.utils import sort_by_filename
        from flyingpigeon.ocgis_module import call

        datasets = sort_by_filename(resources, historical_concatination=True)
        results = []

        kwds = {'percentile': 90, 'window_width': 5}
        calc = [{'func': 'daily_perc', 'name': 'dp', 'kwds': kwds}]

        try:
            for key in datasets.keys():
                try:
                    if region is None:
                        result = call(resource=datasets[key],
                                      output_format='nc',
                                      calc=calc,
                                      # prefix=key,
                                      # time_region={'year': [1995, 2000]}
                                      # calc_grouping='year'
                                      )
                        results.extend([result])
                        LOGGER.debug('percentile based indice done for {}'.format(result))
                    else:
                        result = clipping(resource=datasets[key],
                                          #  variable=None,
                                          calc=calc,
                                          #  calc_grouping=None,
                                          #  time_range=None,
                                          #  time_region=None,
                                          polygons=region,
                                          mosaic=mosaic
                                          )
                        results.extend(result)

                except Exception as ex:
                    msg = 'failed to calculate percentile-based indice for {}: {}'.format(key, str(ex))
                    LOGGER.exception(msg)
                    raise Exception(msg)

        except Exception as ex:
            msg = "failed to calculate percentile-based indices: {}".format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        tarf = archive(results)

        response.outputs['output_archive'].file = tarf

        i = next((i for i, x in enumerate(results) if x), None)
        if i is None:
            i = "dummy.nc"
        response.outputs['ncout'].file = results[i]

        #       response.update_status("done", 100)
        response.update_status("done", 100)
        return response
