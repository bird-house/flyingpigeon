import os

from pywps.Process import WPSProcess
import logging

logger = logging.getLogger(__name__)

class AnalogsviewerProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self,
            identifier="analogs_viewer",
            title="Analogues -- Viewer",
            version = "0.9",
            abstract="Visualisation of text output of analogue process",
            metadata=[
                {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],
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
            abstract="web browser compatible html file",
            formats=[{"mimeType":"text/html"}],
            asReference=True,
            )

        self.output_txt = self.addLiteralOutput(
            identifier="output_txt",
            title="modified analogues txt file",
            abstract="txt file for analogue viewer",
            default=None,
            type=type(''),
            )

    def execute(self):
        ######################
        # start execution 
        ######################

        from flyingpigeon.config import JSsrc_dir
        tmpl = JSsrc_dir() + '/template_analogviewer.html'

        #Get the output csv file of analogs process (input by user in text box)
        analogs = self.getInputValues(identifier='resource')[0]


        
        #Get the output config file of analogs process using name of analogs file
        #(They share the same name tag)
        configfile = analogs.replace('analogs-', 'config-')
        

        ###########################################
        # reorganize analog txt file for javascript
        ###########################################

        from flyingpigeon import config
        from tempfile import mkstemp
        from flyingpigeon.config import www_url
        
        from flyingpigeon.analogs import get_configfile , config_edits

        #my_css_url = www_url() + "/static/css/style.css"

        #use as test input file: http://birdhouse-lsce.extra.cea.fr:8090/wpsoutputs/flyingpigeon/output_txt-0797016c-378e-11e6-91dd-41d8cd554993.txt
        import numpy as np
        import pandas as pd
        import collections
        import os
        import requests
        from shutil import copyfile

        try:
            outputUrl_path = config.outputUrl_path()
            output_path = config.output_path()
            
            #Config file with path (server URL address)
            configfile_with_path = os.path.join(outputUrl_path, configfile)
            logger.debug('configfile_with_path: %s' % configfile_with_path)

            #Check if config file exists
            r = requests.get(configfile_with_path)
            if r.status_code != 404:
                logger.debug('Config file exists on server URL address.')

            else:
                logger.debug('Config file does not exist on server address. Check local disk.')
               
                #Make config file name and get its path on local disk
                configfile = 'config_' + analogs
                logger.debug('local disk configfile: %s' % configfile)
                
                p , name = os.path.split(os.path.realpath(analogs))
                configfile_localAddress = os.path.join(p, configfile)
                logger.debug('local disk configfile_localAddress: %s' % configfile_localAddress)

                #Check if config file exists
                if os.path.isfile(configfile_localAddress):
                    logger.debug('Config file exists on local disk.')
                    #Copy config file to output_path (~/birdhouse/var/lib/pywps/outputs/flyingpigeon)
                    
                    configfile_outputlocation = os.path.join(output_path , configfile)

                    copyfile(configfile_localAddress, configfile_outputlocation)
                    logger.info(' time for coffee ')

                    configfile_outputlocation_edited = config_edits(configfile_outputlocation)
                    logger.info('outputlocation_edited: %s' % configfile_outputlocation_edited)

                    configfile = os.path.basename(configfile_outputlocation_edited)
                    logger.info('  configfile %s  ' % configfile)

                else:
                    logger.debug('There is no config file on local disk. Generating a default one.')

                    #Insert analogs filename into config file.
                    #The rest of the params are unknown.
                    configfile_wkdir = get_configfile(
                        files=['dummyconfig', 'dummyconfig',analogs],
                        nanalog='DUMMY!!!', 
                        varname='DUMMY!!!',
                        seacyc='DUMMY!!!',
                        cycsmooth='DUMMY!!!',
                        timewin='DUMMY!!!',
                        seasonwin='DUMMY!!!',
                        distfun='DUMMY!!!',
                        calccor='DUMMY!!!',
                        outformat='DUMMY!!!',
                        silent='DUMMY!!!',
                        period=['dummy','dummy'],
                        bbox='DUMMY!!!'
                    )

                    configfile = os.path.basename(configfile_wkdir) #just file name
                    #Add server path to file name
                    configfile_inplace = os.path.join(output_path, configfile)
                    
                    #Copy out of local working dir to output_path
                    copyfile(configfile_wkdir, configfile_inplace)

        except Exception as e:
            msg = 'failed to read number of analogues from config file %s ' % e
            logger.debug(msg)
        
        try:
            num_cols = 3 #dateAnlg, Dis, Corr

            #Create dataframe and read in output csv file of analogs process
            dfS = pd.DataFrame()
            dfS = pd.read_csv(analogs, delimiter=r"\s+", index_col=0)

            #Find number of analogues
            num_analogues = (dfS.shape[1])/3
            logger.debug('num_analogues: %s ' % num_analogues)
            
            #Define temporary df
            df_anlg = dfS.iloc[:, 0:num_analogues] #store only anlg dates
            df_dis = dfS.iloc[:, num_analogues:2*num_analogues] #store only dis
            df_corr = dfS.iloc[:, 2*num_analogues:3*num_analogues] #store only corr

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

        tmpl_file = open(tmpl).read()
        
        out = open(output_av, 'w')

        #Insert reformatted analogue file and config file into placeholders in the js script
        tmpl_file = tmpl_file.replace('analogues_placeholder.json', basename(f) )
        tmpl_file = tmpl_file.replace('analogues_config_placeholder.txt', configfile )
        out.write(tmpl_file)
        out.close()


        ################################
        # set the outputs
        ################################

        output_data = outputUrl_path  + '/' + basename(f)
        
        logger.info('Data url: %s ' % output_data)
        logger.info('output_av: %s ' % output_av)

        self.output_txt.setValue( output_data )     
        self.output_html.setValue( output_av )