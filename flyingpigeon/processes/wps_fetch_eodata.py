from pywps import Process
# from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class FetcheodataProcess(Process):
    """
    TODO: like FetchProcess
    """
    def __init__(self):
        inputs = [
            LiteralInput("product", "Earth Observation Product",
                         abstract="Choose a Earth Observation Product",
                         default="PSScene3Band",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=["PSScene3Band", "PSScene4Band"]
                         ),

            LiteralInput('BBox', 'Bounding Box',
                         data_type='string',
                         abstract="Enter a bbox: min_lon, max_lon, min_lat, max_lat."
                                  " min_lon=Western longitude,"
                                  " max_lon=Eastern longitude,"
                                  " min_lat=Southern or northern latitude,"
                                  " max_lat=Northern or southern latitude."
                                  " For example: -80,50,20,70",
                         min_occurs=1,
                         max_occurs=1,
                         default='-80,50,20,70',
                         ),

            LiteralInput('dateSt', 'Start date',
                         data_type='date',
                         abstract='First day of the period to be searched for EO data. (if not set, one month befor today will be selected',
                         # default='2013-07-15',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('dateEn', 'End date of analysis period',
                         data_type='date',
                         abstract='Last day of the period to be searched for EO data. (if not set, todays day is set.)',
                         # default='2013-12-31',
                         min_occurs=0,
                         max_occurs=1,
                         ),


            #
            # ComplexInput('resource', 'Resource',
            #              abstract="NetCDF Files or archive (tar/zip) containing netCDF files.",
            #              min_occurs=1,
            #              max_occurs=1000,
            #              #  maxmegabites=5000,
            #              supported_formats=[Format('application/x-netcdf'),
            #                                 Format('application/x-tar'),
            #                                 Format('application/zip'),
            #                                 ]
            #              )
        ]

        outputs = [
            ComplexOutput("output", "Fetched Files",
                          abstract="File containing the local pathes to downloades files.",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          ),

            ComplexOutput("output_log", "Logging information",
                          abstract="Collected logs during process run.",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          )
        ]

        super(FetchProcess, self).__init__(
            self._handler,
            identifier="fetch_resources",
            title="Fetch Resources",
            version="0.10",
            abstract="Fetch data resources (limited to 50GB) to the local file"
                     "system of the birdhouse compute provider.",
            metadata=[
                Metadata('Documentation', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        from flyingpigeon.log import init_process_logger
        from flyingpigeon.utils import rename_complexinputs
        from flyingpigeon.datafetch import write_fileinfo
        import os

        response.update_status("start fetching resource", 10)

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        resource = rename_complexinputs(request.inputs['resource'])

        response.outputs['output'].file = write_fileinfo(resource, filepath=True)

        # filepathes = 'out.txt'
        # with open(filepathes, 'w') as fp:
        #     fp.write('###############################################\n')
        #     fp.write('###############################################\n')
        #     fp.write('Following files are stored to your local discs: \n')
        #     fp.write('\n')
        #     for f in resources:
        #         fp.write('%s \n' % os.path.realpath(f))

        # response.outputs['output'].file = filepathes
        response.update_status("done", 100)

        return response
