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
        #self.BBox = self.addBBoxInput(
            #identifier="BBox",
            #title="Bounding Box",
            #abstract="coordinates to define the region for weather classification",
            #minOccurs=1,
            #maxOccurs=1,
            #crss=['EPSG:4326']
            #)

        self.BBox = self.addLiteralInput(
            identifier="BBox",
            title="Region",
            abstract="coordinates to define the region: (minlon,maxlon,minlat,maxlat)",
            default='-80,50,22.5,70', #  cdo syntax: 'minlon,maxlon,minlat,maxlat' ; ocgis syntax (minlon,minlat,maxlon,maxlat)
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        self.time_region = self.addLiteralInput(
            identifier="time_region",
            title="Time region",
            abstract="Select the months to define the time region (None == whole year will be analysed)",
            default="12,1,2",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues= ["10,11,12,1,2,3", "4,5,6,7,8,9", "12,1,2", "3,4,5", "6,7,8", "9,10,11", "None"] #GROUPING
            )

        self.datemodelst = self.addLiteralInput(
            identifier="datemodelst",
            title="Start of period",
            abstract="Date to start analysing the reanalyses data (if not set, the first date of the dataset will be taken)",
            default="1970-01-01",
            type=type(date(2013,01,01)),
            minOccurs=0,
            maxOccurs=1,
            )

        self.datemodelen = self.addLiteralInput(
            identifier="datemodelen",
            title="End of period",
            abstract="Date to end analysing the reanalyses data (if not set, the first date of the dataset will be taken)",
            default="2010-12-31",
            type=type(date(2014,12,31)),
            minOccurs=0,
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
            datemodelst = self.getInputValues(identifier='datemodelst')[0]            
            datemodelen = self.getInputValues(identifier='datemodelen')[0]
            
            model, var = model_var.split('_')
            
            logger.info('bbox %s' % bbox)
            logger.info('datemodelst %s' % str(datemodelst))
            logger.info('time_region %s' % str(time_region))
            logger.info('bbox is set to %s' % bbox)
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
        
        
        ###########################
        ### set the environment
        ###########################
        
        try:  
          if datemodelst != None and datemodelen != None : 
            startyr = dt.strptime(datemodelst, '%Y-%m-%d')
            endyr = dt.strptime(datemodelen, '%Y-%m-%d')
            time_range = [startyr,endyr]
          else: 
            time_range = None
            
          if model == 'NCEP': 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
#              vmin=-200
#              vmax=200
            else:
              variable='slp'
              level=None
              conform_units_to='hPa'
#              vmin=-35
#              vmax=35

          elif '20CRV2' in model: 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
#              vmin=-200
#              vmax=200
            else:
              variable='prmsl'
              level=None
              conform_units_to='hPa'
#              vmin=-35
#              vmax=35
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
          model_nc = rl(start=int(datemodelst.split('-')[0]), 
                              end=int(datemodelen.split('-')[0]), 
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
        time_region = {'month':[6,7,8]}

        model_grouped = call(resource=model_nc, variable=variable, time_region=time_region, conform_units_to=conform_units_to)

        ip, model_subset = mkstemp(dir='.',suffix='.nc')
        
        model_subset  = cdo.sellonlatbox('%s' % bbox, input=model_grouped, output=model_subset)
        logger.info('subset done: %s ' % model_subset)
        #if level != None:
        #  nc_level = get_level( nc_subset, level) 
        #  nc_subset =  nc_level

        ########################
        ### computing anomalies 
        ########################
         
        model_anomal = wr.get_anomalies(model_subset)

        ############################
        ### ponderation by latitude
        ############################

        model_ponderate = wr.get_ponderate(model_anomal)

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
          Rfile = 'regimes_NCEP.R'
          
          infile = model_ponderate # nc_subset
          modelname = model
          yr1 = datemodelst.split('-')[0]
          yr2 = datemodelen.split('-')[0]
          ip, output_graphics = mkstemp(dir=curdir ,suffix='.pdf')
          
          #cmd = 'Rscipt  %s %s/ %s/ %s %s %s %s %s %s' % (join(Rsrc,Rfile), curdir, Rsrc, infile, variable, modelname, yr1,yr2, output_graphics)
          #args =shlex.split(cmd)
          
          args = ['Rscript', join(Rsrc,Rfile), '%s/' % curdir, 
                  '%s/' % Rsrc, '%s'% infile, '%s' % variable, 
                  '%s' % modelname, '%s' % yr1, '%s' % yr2, 
                  '%s' % output_graphics]
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
