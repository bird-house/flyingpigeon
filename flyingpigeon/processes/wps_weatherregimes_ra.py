"""
Processes for Weather Classification  
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""
from flyingpigeon.datafetch import _PRESSUREDATA_
from pywps.Process import WPSProcess
# from datetime import  date 

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
                {"title":"Weather Regimes -- Reanalyses data"},
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

        self.season = self.addLiteralInput(
            identifier="season",
            title="Time region",
            abstract="Select the months to define the time region (all == whole year will be analysed)",
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

        self.kappa = self.addLiteralInput(
            identifier="kappa",
            title="Nr of Weather regimes",
            abstract="Set the number of clusters to be detected",
            default=4,
            type=type(1),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=range(1,11)
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

        self.output_netcdf = self.addComplexOutput(
            identifier="output_netcdf",
            title="netCDF fiel",
            abstract="Prepared netCDF file as input for weatherregime calculation",
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True,
            )

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
            season = self.getInputValues(identifier='season')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            model_var = self.getInputValues(identifier='reanalyses')[0]
            period = self.getInputValues(identifier='period')[0]            
            anualcycle = self.getInputValues(identifier='anualcycle')[0]
            model, var = model_var.split('_')
            
            bbox = [float(b) for b in bbox.split(',')]

            start = dt.strptime(period.split('-')[0] , '%Y%m%d')
            end = dt.strptime(period.split('-')[1] , '%Y%m%d')

            kappa = int(self.getInputValues(identifier='kappa')[0])
            
            logger.info('bbox %s' % bbox)
            logger.info('period %s' % str(period))
            logger.info('season %s' % str(season))
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
        
        ###########################
        ### set the environment
        ###########################
        
        try:            
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

        time_range = [start, end]
        model_subset = call(resource=model_nc, variable=variable, 
          geom=bbox, spatial_wrapping='wrap', time_range=time_range, # conform_units_to=conform_units_to
          )
        logger.info('Dataset subset done: %s ' % model_subset)
        
        ##############################################
        ### computing anomalies 
        ##############################################
        
        cycst = anualcycle.split('-')[0]
        cycen = anualcycle.split('-')[0]
        reference = [dt.strptime(cycst,'%Y%m%d'), dt.strptime(cycen,'%Y%m%d')]
        model_anomal = wr.get_anomalies(model_subset, reference=reference)

        #####################
        ### extracting season
        #####################
        model_season = wr.get_season(model_anomal, season=season)

        #######################
        ### call the R scripts
        #######################
        import shlex
        import subprocess
        from flyingpigeon import config
        from os.path import curdir, exists, join

        try:
          rworkspace = curdir
          Rsrc = config.Rsrc_dir() 
          Rfile = 'weatherregimes_model.R'
          
          infile = model_season  #model_subset #model_ponderate 
          modelname = model
          yr1 = start.year
          yr2 = end.year
          ip, output_graphics = mkstemp(dir=curdir ,suffix='.pdf')
          ip, file_pca = mkstemp(dir=curdir ,suffix='.dat')
          ip, file_class = mkstemp(dir=curdir ,suffix='.Rdat')
                    
          args = ['Rscript', join(Rsrc,Rfile), '%s/' % curdir, 
                  '%s/' % Rsrc, '%s'% infile, '%s' % variable, 
                  '%s' % output_graphics, '%s' % file_pca,
                   '%s' % file_class, '%s' % season, 
                   '%s' % start.year, '%s' % end.year,
                   '%s' % model_var, '%s' % kappa]
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
        self.output_netcdf.setValue( model_season )