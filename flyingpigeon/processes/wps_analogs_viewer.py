import os

from pywps.Process import WPSProcess
import logging
from flyingpigeon.log import init_process_logger

logger = logging.getLogger(__name__)


class AnalogsviewerProcess(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(self,
                            identifier="analogs_viewer",
                            title="Analogues -- Viewer",
                            version="0.9",
                            abstract="Visualisation of text output of analogue process",
                            metadata=[
                                {"title": "LSCE",
                                    "href": "http://www.lsce.ipsl.fr/en/index.php"},
                                {"title": "Documentation",
                                 "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                            ],
                            statusSupported=True,
                            storeSupported=True)

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Analogues",
            abstract="Analogues text file",
            minOccurs=1,
            maxOccurs=1,
            # maxmegabites=5000,
            formats=[{"mimeType": "text/plain"}],
        )

        self.output_html = self.addComplexOutput(
            identifier="output_html",
            title="html viewer",
            abstract="web browser compatible html file",
            formats=[{"mimeType": "text/html"}],
            asReference=True,
        )

        self.output_txt = self.addLiteralOutput(
            identifier="output_txt",
            title="modified analogues txt file",
            abstract="txt file for analogue viewer",
            default=None,
            type=type(''),
        )

        self.output_log = self.addComplexOutput(
            identifier="output_log",
            title="Logging information",
            abstract="Collected logs during process run.",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
        )

    def execute(self):
        ######################
        # start execution
        ######################

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        from flyingpigeon import analogs as anlg
        from flyingpigeon import config
        from os.path import basename

        ###########################################
        # reorganize analog txt file for javascript
        # and find associated config file
        ###########################################

        # Reformat data file output by the analogs detection process so that
        # it can be read by the analogues viewer template.
        try:
            # Get the output csv file of analogs process (input by user in
            # text box)
            analogs = self.getInputValues(identifier='resource')[0]

            configfile = anlg.get_viewer_configfile(analogs)
            f = anlg.reformat_analogs(analogs)
            logger.info('Analog file reformatted')
            self.status.set('Successfully reformatted analog file', 50)
            output_av = anlg.get_viewer(f, configfile)
            logger.info('Viewer html page generated')
            self.status.set(
                'Successfully generated analogs viewer html page', 90)

            outputUrl_path = config.outputUrl_path()

            output_data = outputUrl_path + '/' + basename(f)
            logger.info('Data url: %s ' % output_data)
            logger.info('output_av: %s ' % output_av)

        except Exception as e:
            msg = 'Failed to reformat analogs file or generate viewer%s ' % e
            logger.debug(msg)

        ################################
        # set the outputs
        ################################

        self.output_txt.setValue(output_data)
        self.output_html.setValue(output_av)
