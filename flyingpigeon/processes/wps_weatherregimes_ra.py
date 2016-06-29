"""
Processes for Weather Classification  
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""
from flyingpigeon.datafetch import _PRESSUREDATA_

from pywps.Process import WPSProcess
from datetime import  date #datetime,

import logging
logger = logging.getLogger(__name__)

class WeatherRegimesRProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "weatherregimes_reanalyse",
            title = "Weather Regimes -- Reanalyses data (R based)",
            version = "0.1",
            metadata=[
                {"title":"Weather Regimes -- Reanalyses data (R based)"},
                ],
            abstract="Weather Regimes based on pressure patterns, fetching selected Realayses Datasets",
            statusSupported=True,
            storeSupported=True
            )

        # Literal Input Data
        # ------------------
        # self.BBox = self.addBBoxInput(
        #     identifier="BBox",
        #     title="Bounding Box",
        #     abstract="coordinates to define the region for weather classification",
        #     minOccurs=1,
        #     maxOccurs=1,
        #     default=[-80,50,22.5,70],
        #     crss=['EPSG:4326']
        #     )

        self.BBox = self.addLiteralInput(
            identifier="BBox",
            title="Region",
            abstract="coordinates to define the region: (minlon,maxlon,minlat,maxlat)",
            default='-80,22.5,50,70', #  cdo syntax: 'minlon,maxlon,minlat,maxlat' ; ocgis syntax (minlon,minlat,maxlon,maxlat)
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )


        self.time_region = self.addLiteralInput(
            identifier="time_region",
            title="Time region",
            abstract="Select the months to define the time region (None == whole year will be analysed)",
            default="DJF",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues= ['JJA','SON','DJF','MAM','all',
            'JJAS','DJFM','MAMJ','FMA','SOND', 'SONDJF','MAMJJA']
            )


        self.period = self.addLiteralInput(
            identifier="period",
            title="Period for weatherregime calculation",
            abstract="Period for analysing the dataset",
            default="19700101-20101231",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        self.anualcycle = self.addLiteralInput(
            identifier="anualcycle",
            title="Period for anualcycle calculation",
            abstract="Period for anual cycle calculation",
            default="19700101-19991231",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        self.reanalyses = self.addLiteralInput(
            identifier="reanalyses",
            title="Reanalyses Data",
            abstract="Choose an reanalyses dataset for comparison",
            default="NCEP_slp",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues= _PRESSUREDATA_ 
            )
        
        ######################
        ### define the outputs
        ######################

        self.Routput_graphic = self.addComplexOutput(
            identifier="Routput_graphic",
            title="Weather Classification output",
            abstract="Weather Classification",
            formats=[{"mimeType":"image/pdf"}],
            asReference=True,
            )
        
        self.output_pca = self.addComplexOutput(
            identifier="output_pca",
            title="PCA",
            abstract="Principal components",
            formats=[{"mimeType":"text/plain"}],
            asReference=True,
            )

        self.output_classification = self.addComplexOutput(
            identifier="output_classification",
            title="classification",
            abstract="Weather regime classification",
            formats=[{"mimeType":"text/plain"}],
            asReference=True,
            )
        
        #self.output_info = self.addComplexOutput(
            #identifier="output_info",
            #title="Weather Regime per date",
            #abstract="Tar file containing tables of dates with appropriate weather regime association",
            #formats=[{"mimeType":"application/x-tar"}],
            #asReference=True,
            #)         

    def execute(self):
        logger.info('Start process')
        from datetime import datetime as dt
        from flyingpigeon import weatherregimes as wr
        from tempfile import mkstemp
        
      
        ################################
        # reading in the input arguments
        ################################
        try: 
            logger.info('read in the arguments')
            # resources = self.getInputValues(identifier='resources')
            time_region = self.getInputValues(identifier='time_region')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            model_var = self.getInputValues(identifier='reanalyses')[0]
            period = self.getInputValues(identifier='period')[0]            
            #datemodelen = self.getInputValues(identifier='datemodelen')[0]
            anualcycle = self.getInputValues(identifier='anualcycle')[0]
            
            model, var = model_var.split('_')
            
            bbox = [float(b) for b in bbox.split(',')]

            start = dt.strptime(period.split('-')[0] , '%Y%m%d')
            end = dt.strptime(period.split('-')[1] , '%Y%m%d')
            
            logger.info('bbox %s' % bbox)
            logger.info('period %s' % str(period))
            logger.info('time_region %s' % str(time_region))
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
        
        ###########################
        ### set the environment
        ###########################
        
        try:  
          # if datemodelst != None and datemodelen != None : 
          #   startyr = dt.strptime(datemodelst, '%Y-%m-%d')
          #   endyr = dt.strptime(datemodelen, '%Y-%m-%d')
          #   time_range = [startyr,endyr]
          # else: 
          #   time_range = None
            
          if model == 'NCEP': 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
            else:
              variable='slp'
              level=None
              conform_units_to='hPa'
          elif '20CRV2' in model: 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
            else:
              variable='prmsl'
              level=None
              conform_units_to='hPa'
          else:
            logger.error('Reanalyses dataset not known')          
          logger.info('environment set')
        except Exception as e: 
          msg = 'failed to set environment %s ' % e
          logger.error(msg)  
          raise Exception(msg)

        ##########################################
        ### fetch Data from original data archive
        ##########################################

        from flyingpigeon.datafetch import reanalyses as rl            
        try:
          model_nc = rl(start=start.year , 
                        end=end.year , 
                        dataset=model, variable=var)

          logger.info('reanalyses data fetched')
        except Exception as e:
          msg = 'failed to get reanalyses data  %s' % e
          logger.debug(msg)
          raise Exception(msg)
                
        ############################################################    
        ### get the required bbox and time region from resource data
        ############################################################
        
        # from flyingpigeon.weatherregimes import get_level
        from flyingpigeon.ocgis_module import call
        from cdo import Cdo
        cdo = Cdo()
        
        
        # fixing this as soon as possible :-)) 

        time_range = [start, end]  

        model_subset = call(resource=model_nc, variable=variable, 
          geom=bbox, spatial_wrapping='wrap', time_range=time_range, # conform_units_to=conform_units_to
          )

        logger.info('Dataset subset done: %s ' % model_subset)
        
        ########################
        ### computing anomalies 
        ########################
        
        cycst = anualcycle.split('-')[0]
        cycen = anualcycle.split('-')[0]
        
        reference = [dt.strptime(cycst,'%Y%m%d'), dt.strptime(cycen,'%Y%m%d')]         
        model_anomal = wr.get_anomalies(model_subset, reference=reference)

        ########################
        ### extracting seasons
        ########################

        if time_region == 'JJA':
          time_region = {'month':[5,6,7]}
        elif time_region == 'SON':
          time_region = {'month':[9,10,11]}
        elif time_region == 'DJF':
          time_region = {'month':[12,1,2]}
        elif time_region == 'FAM':
          time_region = {'month':[2,3,4]}
        elif time_region == 'MAM':
          time_region = {'month':[3,4,5]}
        elif time_region == 'JJAS':
          time_region = {'month':[6,7,8,9]}
        elif time_region == 'DJFM':
          time_region = {'month':[12,1,2,3]}
        elif time_region == 'MAMJ':
          time_region = {'month':[3,4,5,6]}
        elif time_region == 'SOND':
          time_region = {'month':[9,10,11,12]}
        elif time_region == 'SONDJF':
          time_region = {'month':[9,10,11,12,1,2]}
        elif time_region == 'MAMJJA':
          time_region = {'month':[3,4,5,6,7,8]}
        elif time_region == 'all':
          time_region = None 
        else:
          logger.error('time_region %s not found' % time_region )
          

        ############################
        ### ponderation by latitude
        ############################

#        model_ponderate = wr.get_ponderate(model_anomal)

        ############################################
        ### call the R scripts
        ############################################
        import shlex
        import subprocess
        from flyingpigeon import config
        from os.path import curdir, exists, join

        try:
          rworkspace = curdir
          Rsrc = config.Rsrc_dir() 
          Rfile = 'weatherregimes_model.R'
          
          infile = model_anomal  #model_subset #model_ponderate 
          modelname = model
          yr1 = start.year
          yr2 = end.year
          ip, output_graphics = mkstemp(dir=curdir ,suffix='.pdf')
          ip, file_pca = mkstemp(dir=curdir ,suffix='.dat')
          ip, file_class = mkstemp(dir=curdir ,suffix='.Rdat')
          
          # cmd = 'Rscipt  %s %s/ %s/ %s %s %s %s %s %s' % (join(Rsrc,Rfile), curdir, Rsrc, infile, variable, modelname, yr1,yr2, output_graphics)
          #args =shlex.split(cmd)
          
          args = ['Rscript', join(Rsrc,Rfile), '%s/' % curdir, 
                  '%s/' % Rsrc, '%s'% infile, '%s' % variable, 
                  '%s' % output_graphics, '%s' % file_pca,
                   '%s' % file_class, '%s' % time_region, 
                   '%s' % start.year, '%s' % end.year,
                   '%s' % model_var]
          logger.info('Rcall builded')
        except Exception as e: 
          msg = 'failed to build the R command %s' % e
          logger.error(msg)  
          raise Exception(msg)
        try:
          output,error = subprocess.Popen(args, stdout = subprocess.PIPE, stderr= subprocess.PIPE).communicate() #, shell=True
          
          logger.info('R outlog info:\n %s ' % output)
          logger.debug('R outlog errors:\n %s ' % error)
          
          if len(output) > 0:            
            self.status.set('**** weatherregime in R suceeded', 90)
          else:
            logger.error('NO! output returned from R call')
        except Exception as e: 
          msg = 'weatherregime in R %s ' % e
          logger.error(msg)  
          raise Exception(msg)

        ############################################
        ### set the outputs
        ############################################

        self.Routput_graphic.setValue( output_graphics )
        self.output_pca.setValue( file_pca )
        self.output_classification.setValue( file_class )