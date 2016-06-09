"""
Processes for Weather Classification  
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps.Process import WPSProcess
from datetime import  date #datetime,

import logging
logger = logging.getLogger(__name__)

class WeatherRegimesRProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "weatherregimes_R",
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
        
        self.dateobsst = self.addLiteralInput(
            identifier="dateobsst",
            title="Start of period",
            abstract="Year to start analysing the observation data (if not set, the first date of the dataset will be taken)",
            default="2010",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        self.dateobsen = self.addLiteralInput(
            identifier="dateobsen",
            title="End of period",
            abstract="Year to end analysing the observation data (if not set, the first date of the dataset will be taken)",
            default="2014",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            )

        self.observation = self.addLiteralInput(
            identifier="observation",
            title="Observation Data",
            abstract="Choose an observation dataset for comparison",
            default="NCEP_slp",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['NCEP_slp', 'NCEP_z1000',   'NCEP_z925',   'NCEP_z850',   'NCEP_z700',   'NCEP_z600',   'NCEP_z500',   'NCEP_z400',   'NCEP_z300',
         'NCEP_z250', 'NCEP_z200',   'NCEP_z150',   'NCEP_z100',    'NCEP_z70',    'NCEP_z50',    'NCEP_z30',    'NCEP_z20', 'NCEP_z10',
         '20CRV2_prmsl',
         '20CRV2_z1000',   '20CRV2_z950',   '20CRV2_z900',   '20CRV2_z850',   '20CRV2_z800',   '20CRV2_z750',   '20CRV2_z700',   '20CRV2_z650',
         '20CRV2_z600',   '20CRV2_z550',   '20CRV2_z500',   '20CRV2_z450',   '20CRV2_z400',   '20CRV2_z350',   '20CRV2_z300',   '20CRV2_z250',
         '20CRV2_z200',   '20CRV2_z150',   '20CRV2_z100',    '20CRV2_z70',    '20CRV2_z50',    '20CRV2_z30',    '20CRV2_z20',    '20CRV2_z10',
         '20CRV2c_prmsl',
         '20CRV2c_z1000',   '20CRV2c_z950',   '20CRV2c_z900',   '20CRV2c_z850',   '20CRV2c_z800',   '20CRV2c_z750',   '20CRV2c_z700',   '20CRV2c_z650',
         '20CRV2c_z600',   '20CRV2c_z550',   '20CRV2c_z500',   '20CRV2c_z450',   '20CRV2c_z400',   '20CRV2c_z350',   '20CRV2c_z300',   '20CRV2c_z250',
         '20CRV2c_z200',   '20CRV2c_z150',   '20CRV2c_z100',    '20CRV2c_z70',    '20CRV2c_z50',    '20CRV2c_z30',    '20CRV2c_z20',    '20CRV2c_z10',
         ] #  '', '20CR_z200', '20CR_z500', '20CR_z1000'
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
        
        self.output_info = self.addComplexOutput(
            identifier="output_info",
            title="Weather Regime per date",
            abstract="Tar file containing tables of dates with appropriate weather regime association",
            formats=[{"mimeType":"application/x-tar"}],
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
            time_region = self.getInputValues(identifier='time_region')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            obs_var = self.getInputValues(identifier='observation')[0]
            dateobsst = self.getInputValues(identifier='dateobsst')[0]            
            dateobsen =self.getInputValues(identifier='dateobsen')[0]
            
            obs, var = obs_var.split('_')
            
            logger.info('bbox %s' % bbox)
            logger.info('dateobsst %s' % str(dateobsst))
            logger.info('time_region %s' % str(time_region))
            logger.info('bbox is set to %s' % bbox)
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
        
        
        ###########################
        ### set the environment
        ###########################
        try:  
          if dateobsst != None and dateobsen != None : 
            start = int(dateobsst[0])# dt.strptime(dateobsst, '%Y-%m-%d')
            end = int(dateobsen[0])# dt.strptime(dateobsen, '%Y-%m-%d')
            time_range = [start,end]
          else: 
            time_range = None
            
          if obs == 'NCEP': 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
              vmin=-200
              vmax=200
            else:
              variable='slp'
              level=None
              conform_units_to='hPa'
              vmin=-35
              vmax=35

          elif '20CRV2' in obs: 
            if 'z' in var:
              variable='hgt'
              level=var.strip('z')
              conform_units_to=None
              vmin=-200
              vmax=200
            else:
              variable='prmsl'
              level=None
              conform_units_to='hPa'
              vmin=-35
              vmax=35
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
        
        try:
          nc_obs = wr.get_OBS(start=int(dateobsst[0]), 
                              end=int(dateobsen[0]), 
                              dataset=obs, variable=var)
          logger.info('observation data fetched')
        except Exception as e:
          msg = 'failed to get Observation data  %s' % e
          logger.debug(msg)
          raise Exception(msg)
                
        ############################################    
        ### get the required bbox from resource data
        ############################################
        
        from flyingpigeon.weatherregimes import get_level
        from flyingpigeon.ocgis_module import call
        from cdo import Cdo
        cdo = Cdo()

        nc_grouped = call(resource=nc_obs, variable=variable, conform_units_to='hPa')

        ip, nc_subset = mkstemp(dir='.',suffix='.nc')
        nc_subset  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output=nc_subset)
        logger.info('subset done: %s ' % nc_subset)
        if level != None:
          nc_level = get_level( nc_subset, level) 
          nc_subset =  nc_level
        
              
        ############################################
        ### call the R scripts
        ############################################
        import shlex
        import subprocess
        from flyingpigeon import config
        from os.path import curdir

        try:
          rworkspace = curdir()
          Rsrc = config.Rsrc_dir() 
          infile <- nc_subset
          #variable <- args[4]
          modelname <- obs
          yr1 <- dateobsst
          yr2 <- dateobsst
          ip, output_graphics = mkstemp(dir=curdir(),suffix='.pdf')

          cmd = 'Rscript --vanilla %s/regimes_NCEP.R %s' % (path.relpath(Rscr), rworkspace, Rsrc, infile, 
                                                             variable, modelname, yr1, yr2, output_graphics)
          args = shlex.split(cmd)
          output,error = subprocess.Popen(args, stdout = subprocess.PIPE, stderr= subprocess.PIPE).communicate()
          logger.info('R outlog info:\n %s ' % output)
          logger.debug('R outlog errors:\n %s ' % error)
          self.status.set('**** weatherregime in R suceeded', 90)
        except Exception as e: 
          msg = 'weatherregime in R %s ' % e
          logger.error(msg)  
          raise Exception(msg)

        ############################################
        ### set the outputs
        ############################################

        self.Routput_graphic.setValue( output_graphics )
        self.output_info.setValue('info.tar')