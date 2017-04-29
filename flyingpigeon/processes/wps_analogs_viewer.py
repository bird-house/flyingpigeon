import os

from flyingpigeon import analogs as anlg
from flyingpigeon import config
from os.path import basename

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class AnalogsviewerProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput("resource", "Analogues result file",
                         abstract="Analogues text file",
                         min_occurs=1,
                         max_occurs=1,
                         # maxmegabites=5000,
                         supported_formats=[Format('text/plain')]
                         ),
             ]

        outputs = [

            ComplexOutput("output_html", "html viewer",
                          abstract="web browser compatible html file",
                          supported_formats=[Format("text/html")],
                          as_reference=True,
                          ),

            LiteralOutput("output_txt", "modified analogues txt file",
                          abstract="txt file for analogue viewer",
                          data_type='string',
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]

        super(AnalogsviewerProcess, self).__init__(
            self._handler,
            identifier="analogs_viewer",
            title="Analogues of circulation (visualization of analogs data)",
            abstract="Visualisation of text output of analogue process",
            version="0.10",
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

        ###########################################
        # reorganize analog txt file for javascript
        # and find associated config file
        ###########################################

        # Reformat data file output by the analogs detection process so that
        # it can be read by the analogues viewer template.
        try:
            # Get the output csv file of analogs process (input by user in
            # text box)
            analogs = request.inputs['resource'][0].data

            configfile = anlg.get_viewer_configfile(analogs)
            f = anlg.reformat_analogs(analogs)
            LOGGER.info('Analog file reformatted')
            response.update_status('Successfully reformatted analog file', 50)
            output_av = anlg.get_viewer(f, configfile)
            LOGGER.info('Viewer html page generated')
            response.update_status(
                'Successfully generated analogs viewer html page', 90)

            output_url = config.output_url()
            output_data = output_url + '/' + basename(f)
            LOGGER.info('Data url: %s ' % output_data)
            LOGGER.info('output_av: %s ' % output_av)

        except Exception as e:
            msg = 'Failed to reformat analogs file or generate viewer%s ' % e
            LOGGER.debug(msg)

        ################################
        # set the outputs
        ################################
        response.outputs['output_txt'] = output_data
        response.outputs['output_htm'] = output_av
        return response
