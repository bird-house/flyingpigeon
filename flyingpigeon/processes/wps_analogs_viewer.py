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

        #Get the output csv file of analogs process (input by user in text box)
        analogs = self.getInputValues(identifier='resource')[0]
        
        #Get the output config file of analogs process using name of analogs file
        #(They share the same name tag)
        configfile = analogs.replace('analogs-', 'config-')
        

        ###########################################
        # reorganize analog txt file for javascript
        # and find associated config file
        ###########################################

        from flyingpigeon import config
        from tempfile import mkstemp
        from flyingpigeon.config import www_url
        from flyingpigeon.analogs import get_configfile, config_edits, refomat_analogs, get_viewer
     
        import numpy as np
        import os
        from os.path import basename
        import requests
        from shutil import copyfile

        #Get config file from either 1) working dir, 2) same local disk as analog file,
        #or 3) create a dummy config if none exists
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

        #Reformat data file output by the analogs detection process so that it can be
        #read by the analogues viewer template.
        try:
            f = refomat_analogs(analogs)
            logger.info('Analog file reformatted')
            self.status.set('Successfully reformatted analog file', 50)
            output_av = get_viewer(f, configfile)
            logger.info('Viewer html page generated')
            self.status.set('Successfully generated analogs viewer html page', 90)

            output_data = outputUrl_path  + '/' + basename(f)
            logger.info('Data url: %s ' % output_data)
            logger.info('output_av: %s ' % output_av)

        except Exception as e:
            msg = 'Failed to reformat analogs file or generate viewer%s ' % e
            logger.debug(msg)


        ################################
        # set the outputs
        ################################

        self.output_txt.setValue( output_data )     
        self.output_html.setValue( output_av )