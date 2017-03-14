import os

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
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),
        ]

        outputs = [
            ComplexOutput("output", "Fetched Files",
                          abstract="File containing the local pathes to downloades files",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          ),

            ComplexOutput("output_log", "Logging information",
                          abstract="Collected logs during process run.",
                          formats=[Format("text/plain")],
                          as_reference=True,
                          )
        ]

        super(FetchProcess, self).__init__(
            self._handler,
            identifier="fetch_resources",
            title="Download Resources",
            version="0.10",
            abstract="This process downloads resources (limited to 50GB) \
                      to the local file system of the birdhouse compute provider",
            metadata=[
                # {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Doc", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        response.update_status("start fetching resources", 10)
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        resources = request.inputs['resource']
        filepathes = 'out.txt'
        with open(filepathes, 'w') as fp:
            fp.write('###############################################\n')
            fp.write('###############################################\n')
            fp.write('Following files are stored to your local discs: \n')
            fp.write('\n')
            for resource in resources:
                fp.write('%s \n' % os.path.realpath(resource))

        response.outputs['output'] = filepathes
        response.update_status("done", 100)
        return response
