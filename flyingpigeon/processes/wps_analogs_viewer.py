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

        #Output csv file of analogs process
        analogs = self.getInputValues(identifier='resource')[0]     

        ###########################################
        # reorganize analog txt file for javascript
        ###########################################

        from flyingpigeon import config
        from tempfile import mkstemp
        from flyingpigeon.config import www_url
        #my_css_url = www_url() + "/static/css/style.css"

        #begin CN
        #use as test input file: http://birdhouse-lsce.extra.cea.fr:8090/wpsoutputs/flyingpigeon/output_txt-0797016c-378e-11e6-91dd-41d8cd554993.txt
        import numpy as np
        import pandas as pd
        import collections
        import os
        
        try: 
            num_analogues = 20 #number of analogues searched for
            num_cols = 3 #dateAnlg, Dis, Corr

            #Create dataframe and read in output csv file of analogs process
            dfS = pd.DataFrame()
            dfS = pd.read_csv(analogs, delimiter=r"\s+", index_col=0)
            
            #Define temporary df
            df_anlg = dfS.iloc[:, 0:20] #store only anlg dates
            df_dis = dfS.iloc[:, 20:40] #store only dis
            df_corr = dfS.iloc[:, 40:60] #store only corr

            #remove index name before stacking
            df_anlg.index.name = ""
            df_dis.index.name = ""
            df_corr.index.name = ""

            #Stack (transpose)into single col
            dateStack = df_anlg.stack()
            disStack = df_dis.stack().abs() #raw values < 0 so take abs
            corrStack = df_corr.stack()

            # #BUILD NEW DF IN CORRECT FORMAT

            #Create df of correct dimensions (n x num_cols) using dfS
            df_all = dfS.iloc[:, 0:num_cols] #NB data are placeholders
            #Rename cols
            df_all.columns = ['dateAnlg', 'Dis', 'Corr']
            #Replicate each row 20 times (for dcjs format)
            df_all = df_all.loc[np.repeat(df_all.index.values,num_analogues)]
            #Replace data placeholders with correct values
            df_all['dateAnlg'] = list(dateStack)
            df_all['Dis'] = list(disStack)
            df_all['Corr'] = list(corrStack)
            #Name index col
            df_all.index.name = 'dateRef'

            # #SAVE TO TSV FILE
            output_path = config.output_path()
            ip , f = mkstemp(suffix='.tsv', prefix='modified-analogfile', dir=output_path, text=False)
            df_all.to_csv(f, sep='\t')
            logger.info('successfully reformatted analog file')
            self.status.set('successfully reformatted analog file', 90)

        except Exception as e: 
            msg = 'failed to reformat analog file %s ' % e
            logger.debug(msg)  
            # raise Exception(msg)


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
