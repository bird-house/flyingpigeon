import os

from pywps.Process import WPSProcess
import logging

logger = logging.getLogger(__name__)

class AnalogsviewerProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self,
            identifier="analogs_viewer",
            title="Analogs -- Viewer",
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
            abstract="web browser compatible hmtl file",
            formats=[{"mimeType":"text/html"}],
            asReference=True,
            )

        self.output_txt = self.addLiteralOutput(
            identifier="output_txt",
            title="modified analogs txt file",
            abstract="txt file for analog viewer",
            default=None,
            type=type(''),
            )

        # self.output_txt = self.addComplexOutput(
        #     identifier="output_txt",
        #     title="modified analogs txt file",
        #     abstract="txt file for analog viewer",
        #     formats=[{"mimeType":"text/txt"}],
        #     asReference=True,
        #     )

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

        from flyingpigeon import config
        from tempfile import mkstemp


        output_path = config.output_path()
        ip , f = mkstemp(suffix='.json', prefix='modified-analogfile', dir=output_path, text=False)

        #Replace with pandas code
        #copyfile(myanalogs, f)
        from shutil import copyfile
        copyfile(analogs, f)

        ################################
        # modify JS template
        ################################

        from os.path import basename
        
        ip, output_av = mkstemp(suffix='.html', prefix='analogviewer', dir='.', text=False)
        #copyfile(tmpl, output_av) 

        tmpl_file = open(tmpl).read()
        
        out = open(output_av, 'w')

        #replacements = {'analogues_placeholder.json':basename(f)}
        #for i in replacements.keys():

        tmpl_file = tmpl_file.replace('analogues_placeholder.json', basename(f) )
        out.write(tmpl_file)
        out.close()

        # with open(output_av, 'r+') as f:
        #     content = f.read()
        #     f.seek(0)
        #     f.truncate()
        #     f.write(content.replace('analogues_placeholder.json', basename(f)))


        ################################
        # set the outputs
        ################################
        
        outputUrl_path = config.outputUrl_path()

        output_data = outputUrl_path  + '/' + basename(f)
        
        logger.info('Data url: %s ' % output_data)
        logger.info('output_av: %s ' % output_av)

        self.output_txt.setValue( output_data )     
        self.output_html.setValue( output_av )
