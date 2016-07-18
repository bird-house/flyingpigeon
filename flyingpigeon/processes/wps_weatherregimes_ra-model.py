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
            identifier = "weatherregimes_projection",
            title = "Weather Regimes -- Projection of Weather Regimes",
            version = "0.1",
            metadata=[
                {"title":"Weather Regimes -- Projection of Weather Regimes"},
                ],
            abstract="Weather Regimes detection based on trained reference statistics",
            statusSupported=True,
            storeSupported=True
            )


        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.Rdat = self.addLiteralInput(
            identifier="Rdat",
            title="R - workspace",
            abstract="R workspace as outputed from weatherregieme reference process",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
            )

        self.dat = self.addLiteralInput(
            identifier="dat",
            title="R - datafile",
            abstract="R datafile as outputed from weatherregieme reference process",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
            )

        self.netCDF = self.addLiteralInput(
            identifier="netCDF",
            title="netCDF reference",
            abstract="netCDF file as outputed from weatherregieme reference process",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
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

        # self.BBox = self.addLiteralInput(
        #     identifier="BBox",
        #     title="Region",
        #     abstract="coordinates to define the region: (minlon,maxlon,minlat,maxlat)",
        #     default='-80,22.5,50,70', #  cdo syntax: 'minlon,maxlon,minlat,maxlat' ; ocgis syntax (minlon,minlat,maxlon,maxlat)
        #     type=type(''),
        #     minOccurs=1,
        #     maxOccurs=1,
        #     )

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
            resource = self.getInputValues(identifier='resource')
            url_Rdat = self.getInputValues(identifier='Rdat')[0]
            url_dat = self.getInputValues(identifier='dat')[0]
            url_ref_file = self.getInputValues(identifier='netCDF')[0]
            season = self.getInputValues(identifier='season')[0]
            period = self.getInputValues(identifier='period')[0]            
            anualcycle = self.getInputValues(identifier='anualcycle')[0]
            
            start = dt.strptime(period.split('-')[0] , '%Y%m%d')
            end = dt.strptime(period.split('-')[1] , '%Y%m%d')

            kappa = int(self.getInputValues(identifier='kappa')[0])
            
            logger.info('bbox %s' % bbox)
            logger.info('period %s' % str(period))
            logger.info('season %s' % str(season))
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
           
        ############################
        # fetching trainging data 
        ############################
        
        from flyingpigeon.utils import download
        from os.path import abspath
        
        try:
          dat = abspath(download(url_dat))
          Rdat = abspath(download(url_Rdat))
          ref_file = download(url_ref_file)
          logger.info('training data fetched')
        except Exception as e:
          logger.error('failed to fethch training data %s' % e)
          
        ############################################################    
        ### get the required bbox and time region from resource data
        ############################################################        
        # from flyingpigeon.weatherregimes import get_level
        
        from flyingpigeon.ocgis_module import call 
        from flyingpigeon.utils import get_variable
        time_range = [start, end]

        variable = get_variable(resource)
        model_subset = call(resource=resource, variable=variable, 
          time_range=time_range,  # conform_units_to=conform_units_to, geom=bbox, spatial_wrapping='wrap',
          regrid_destination=ref_file, regrid_options='bil')
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
          Rfile = 'weatherregimes_projection.R'
          
          yr1 = start.year
          yr2 = end.year
          ip, output_graphics = mkstemp(dir=curdir ,suffix='.pdf')
          ip, file_pca = mkstemp(dir=curdir ,suffix='.dat')
          ip, file_class = mkstemp(dir=curdir ,suffix='.Rdat')
                    
          args = ['Rscript', join(Rsrc,Rfile), '%s/' % curdir, 
                  '%s/' % Rsrc, 
                  '%s' % model_season, 
                  '%s' % variable, 
                  '%s' % output_graphics,
                  '%s' % dat, 
                  '%s' % Rdat, 
                  '%s' % file_pca,
                  '%s' % file_class, '%s' % season, 
                  '%s' % start.year, '%s' % end.year,
                  '%s' % 'MODEL']
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