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

        ###########################################
        # reorganize analog txt file for javascript
        ###########################################

        from flyingpigeon import config
        from tempfile import mkstemp

        #begin CN
        import numpy as np
        import pandas as pd
        import collections
        import os

        num_analogues = 20 #number of analogues searched for

        #Create dataframe and read in output csv file of analogs process
        dfS = pd.DataFrame()
        dfS = pd.read_csv(analogs, delimiter=' ', index_col=0)

        #Save date index
        dfS['dateRef'] = dfS.index

        #Create date table
        dfA_raw = dfS.iloc[:, 0:20]

        #STACK ANALOGUE DATES ACROSS COLUMNS INTO ONE COLUMN

        #first remove index name of dfA_raw
        dfA_raw.index.name = ""

        dfA_stack = dfA_raw.stack()
        dfA_stack.index.name

        #CREATE EMPTY DATAFRAME WITH NUM ROWS = NUM COLS OF dfA_raw
        index = dfA_raw.shape[0] #num cols of dfA
        columns = 2
        dfA = pd.DataFrame(np.nan, index=range(0, dfA_raw.shape[0]), columns=['dateAnlg'])

        #REPLACE INDEX COL OF dfA WITH ORIG date_id
        dfA = dfA.set_index(dfS['dateRef'])

        #REPLICATE INDEX COL num_analogues = 20 TIMES PER ROW
        dfA = dfA.loc[np.repeat(dfA.index.values,num_analogues)]

        #REPLACE COL 0 WITH STACKED DATES
        dfA.iloc[:,0] = list(dfA_stack)

        #SAVE TO TSV FILE
        output_path = config.output_path()
        ip , f = mkstemp(suffix='.tsv', prefix='modified-analogfile', dir=output_path, text=False)
        dfA.to_csv(f, sep='\t')

       
        #copyfile(myanalogs, f)
        #from shutil import copyfile
        #copyfile(analogs, f)

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
