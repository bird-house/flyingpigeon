from pywps import Process
# from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class FetchProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract="NetCDF Files or archive (tar/zip) containing netCDF files",
                         min_occurs=1,
                         max_occurs=1000,
                         #  maxmegabites=5000,
                         supported_formats=[Format('application/x-netcdf'),
                                            Format('application/x-tar'),
                                            Format('application/zip'),
                                            ]
                         )
        ]

        outputs = [
            ComplexOutput("output", "Fetched Files",
                          abstract="File containing the local pathes to downloades files",
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
            abstract="This process fetches data resources (limited to 50GB) \
                      to the local file system of the birdhouse compute provider",
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
