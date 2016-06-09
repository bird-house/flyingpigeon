import os

from pywps.Process import WPSProcess
import logging

logger = logging.getLogger(__name__)

class AnalogviewerProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self,
            identifier="analogviewer",
            title="Analogviewer",
            version = "0.1",
            abstract="Visualisation of text output of analogue process",
            statusSupported=True,
            storeSupported=True)

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Analogues",
            abstract="Analogues text file",
            minOccurs=1,
            maxOccurs=1,
            #maxmegabites=5000,
            formats=[{"mimeType":"text/plain"}],
            )

        self.output_html = self.addComplexOutput(
            identifier="output_html",
            title="html viewer",
            abstract="web browser compartible hmtl file",
            formats=[{"mimeType":"text/html"}],
            asReference=True,
            )

        self.output_txt = self.addComplexOutput(
            identifier="output_txt",
            title="modified analogs txt file",
            abstract="txt file for analog viewer",
            formats=[{"mimeType":"text/txt"}],
            asReference=True,
            )

    def execute(self):
        ######################
        # start execution 
        ######################

        from flyingpigeon.config import JSsrc_dir
        tmpl = JSsrc_dir() + '/template_analogviewer.html'
        analogs = self.getInputValues(identifier='resource')[0]     

        ################################
        # converting the analog txt file 
        ################################

        mod_analogs = analogs

        ################################
        # modify JS template
        ################################
        
        output_av = tmpl

        ################################
        # set the outputs
        ################################
        self.output_txt.setValue( mod_analogs )
        self.output_html.setValue( output_av )
