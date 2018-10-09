import logging
from os.path import basename

from flyingpigeon import analogs as anlg
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralOutput
from pywps import Process
from pywps.app.Common import Metadata

LOGGER = logging.getLogger("PYWPS")


class AnalogsviewerProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput("analog_result", "Analogues result file",
                         abstract="Analogues text file computed by Analogues of Corculation processes",
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
            analogs = rename_complexinputs(request.inputs['analog_result'])[0]

            # analogs = request.inputs['analog_result'][0]
            LOGGER.info("analogs file path %s ", analogs)

            configfile = "dummy.txt"  # anlg.get_viewer_configfile(analogs)
            analogs_mod = anlg.reformat_analogs(analogs)
            response.outputs['output_txt'].file = analogs_mod  # output_data
            LOGGER.info("analogs for visualisation prepared")
        except Exception:
            msg = 'Failed to reformat analogs file'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            output_av = anlg.get_viewer(
                configfile=basename(configfile),
                datafile=basename(analogs_mod))
            LOGGER.info('Viewer html page generated')
            response.update_status('Successfully generated analogs viewer html page', 90)
            response.outputs['output_html'].file = output_av
            LOGGER.info('output_av: %s ', output_av)
        except Exception:
            msg = 'Failed to generate viewer'
            LOGGER.exception(msg)
            raise Exception(msg)

        return response
