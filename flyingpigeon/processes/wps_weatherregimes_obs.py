"""
Processes for Weather Classification  
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
Author: Cathy Nangini 
"""

from pywps.Process import WPSProcess
from datetime import  date #datetime,

import logging
logger = logging.getLogger(__name__)

class WeatherRegimesObsProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "weatherregimes_obs",
            title = "Weather Regimes for Obervation data",
            version = "0.1",
            metadata=[
                {"title":"Weather Regimes for Obervation data"},
                ],
            abstract="Weather Regimes based on pressure patterns (kmean method)",
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
            abstract="Date to start analysing the observation data (if not set, the first date of the dataset will be taken)",
            default="2010-01-01",
            type=type(date(2013,01,01)),
            minOccurs=0,
            maxOccurs=1,
            )

        self.dateobsen = self.addLiteralInput(
            identifier="dateobsen",
            title="End of period",
            abstract="Date to end analysing the observation data (if not set, the first date of the dataset will be taken)",
            default="2014-12-31",
            type=type(date(2014,12,31)),
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
         'NCEP_z250',   'NCEP_z200',   'NCEP_z150',   'NCEP_z100',    'NCEP_z70',    'NCEP_z50',    'NCEP_z30',    'NCEP_z20', 'NCEP_z10'] #  '20CR_ps', '20CR_z200', '20CR_z500', '20CR_z1000'
            )
        
        ######################
        ### define the outputs
        ######################

        self.output_clusters = self.addComplexOutput(
            identifier="output_clusters",
            title="Weather Classification Clusters",
            abstract="Weather Classification Clusters",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

        self.output_maps = self.addComplexOutput(
            identifier="output_maps",
            title="Pressure pattern",
            abstract="Corresponding pressure maps for Weather Regimes",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

        #self.output_matrix = self.addComplexOutput(
            #identifier="output_matrix",
            #title="Pressure pattern sorted by R value",
            #abstract="Pressure pattern with highest R value for appropriate Observation",
            #formats=[{"mimeType":"image/png"}],
            #asReference=True,
            #)
        
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
      
        try: 
            logger.info('read in the arguments')
           # resources = self.getInputValues(identifier='resources')
          #  method = self.getInputValues(identifier='method')
            time_region = self.getInputValues(identifier='time_region')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            obs_var = self.getInputValues(identifier='observation')[0]
            dateobsst = self.getInputValues(identifier='dateobsst')[0]            
            dateobsen =self.getInputValues(identifier='dateobsen')[0]
            
            obs, var = obs_var.split('_')
            
            logger.info('bbox %s' % bbox)
            logger.info('dateobsst %s' % str(dateobsst))
            logger.info('time_region %s' % str(time_region))
           # logger.info('method: %s' % str(method))
            
        except Exception as e: 
            logger.debug('failed to read in the arguments %s ' % e)
        
        #bbox = '-80,22.5,50,70'
        logger.info('bbox is set to %s' % bbox)     

        #####################    
        ### get the required bbox from resource
        #####################
        # from flyingpigeon.ocgis_module import call 
        
        from flyingpigeon.utils import sort_by_filename, get_time, get_coordinates, get_values  #calc_grouping
        from flyingpigeon import weatherregimes as wr
        from flyingpigeon.visualisation import plot_kMEAN, concat_images, plot_pressuremap
        
        from numpy import savetxt, column_stack, ma, mean, reshape, ones, empty
        from scipy import stats
        import tarfile

        try:
          png_clusters = []
          txt_info = []
          png_pressuremaps = []
          png_sorted = []
          
          #regime_dic = {}
          # open tar files
          tar_info = tarfile.open('info.tar', "w")
          logger.info('tar files prepared')
        except:
          msg = 'tar file preparation failed'
          logger.debug(msg)
          raise Exception(msg)
        
        ############################
        ### fetch Data
        ############################
        
        try:
          nc_obs = wr.get_OBS(start=int(dateobsst.split('-')[0]), 
                              end=int(dateobsen.split('-')[0]), 
                              dataset=obs, variable=var)
          
          logger.info('observation data fetched')
        except Exception as e:
          msg = 'failed to get Observation data  %s' % e
          logger.debug(msg)
          raise Exception(msg)
                  
        try:  
          if dateobsst != None and dateobsen != None : 
            start = dt.strptime(dateobsst, '%Y-%m-%d')
            end = dt.strptime(dateobsen, '%Y-%m-%d')
            time_range = [start,end]
          else: 
            time_range = None
            
          if obs == 'NCEP' and 'z' in var:
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
          
          subset_obs = wr.subset(nc_obs, bbox=bbox, time_region=time_region, time_range=time_range, 
                                 variable=variable, level=level, conform_units_to=conform_units_to)
          logger.info('observation data prepared')
        except Exception as e:
          msg = 'failed to subset dataset %s' % e
          logger.debug(msg)
          raise Exception(msg)
        
        try:
          pca_obs = wr.get_pca(subset_obs)
          centroids_obs, distance_obs, regime_obs = wr.calc_kMEAN(pca_obs)
          logger.info('PCA and centroids calculated')
        except Exception as e:
          msg = 'failed to calculate weather regimes %s' % e
          logger.debug(msg)
          raise Exception(msg)

        try:
          lats, lons = get_coordinates(subset_obs)
          data_obs = get_values(subset_obs)
          times = get_time(subset_obs)
          # = [t for t in times]
          logger.info('read in lat lon and time values for plotting')
        except Exception as e:
          msg = 'failed to get data lats/lons or times %s' % e
          logger.debug(msg)
          raise Exception(msg)

        try:
          tc = column_stack([times, regime_obs])
          fn = 'OBS_data.csv'
          
          savetxt(fn, tc, fmt='%s', delimiter=',', header='Date Time,WeatherRegime')
          tar_info.add(fn)
          logger.info('text file written')
        except Exception as e:
          msg = 'failed to write OBS csv file %s' % e
          logger.debug(msg)
          raise Exception(msg)
            
        ###############################
        # plot weather regimes for NCEP 
        ###############################
        try:
          title = 'Month time_region: %s [BBox: %s]' % (time_region,bbox)
          sub_title = 'Dataset: %s Variable: %s Period: %s-%s,' % (obs,var, dateobsst ,dateobsen)
        
          png_clusters.append(plot_kMEAN(centroids_obs, pca_obs, 
            title=title, 
            sub_title=sub_title))
          logger.info('kMEAN calculated for NCEP Data')
        except Exception as e:
          msg = 'failed to plot NCEP cluster %s' % e
          logger.debug(msg)
          raise Exception(msg)
        subplots = []
        obs_pattern = []

        if var == 'slp':
          vmin = -30
          vmax = 30
        else : 
          vmin = None
          vmax = None
        
        for i in range(4):
          try:
            d_mask = ma.masked_array(distance_obs[:,i], mask=(regime_obs==i))
            best_pattern = d_mask.argsort()[0:10]
            pattern = mean(data_obs[best_pattern], axis = 0)
            obs_pattern.append(pattern) 
            subplots.append(plot_pressuremap(pattern, 
              lats=lats, lons=lons, vmin=vmin, vmax=vmax,
              #facecolor = '#E0E0E0', # grey background 
              title=title, #'Weather Regime %s: Month %s ' % (i, time_region), 
              sub_title=sub_title))
          except Exception as e:
            msg = 'failed to plot NCEP weather regime pattern %s' % e
            logger.debug(msg)
            raise Exception(msg)

        try:  
          png_pressuremaps.append(concat_images(subplots, orientation='h'))
          #png_sorted.append(concat_images(subplots, orientation='h'))
        except Exception as e:
          msg = 'failed to concatinate NCEP weather regimes %s' % e
          logger.debug(msg)
          raise Exception(msg)
          
        ######################
        # concatinate pictures
        ######################
          
        #try:
          #c_clusters = None
          #c_maps = None
          #c_matrix = None
          #if len(png_clusters) >1:
            #c_clusters = concat_images(png_clusters, orientation='v')
          #else: 
            #c_clusters = png_clusters
          #c_maps = concat_images(png_pressuremaps, orientation='v')
          #c_matrix = concat_images(png_sorted, orientation='v')
        #except Exception as e:
          #logger.debug('failed to concat plots %s ' % e)
        
        try:
          tar_info.close()  
          logger.info('tar files closed')
        except Exception as e:
          logger.debug('tar file closing failed %s' % e)

        self.output_clusters.setValue( png_clusters[0] )
        self.output_maps.setValue( png_pressuremaps[0] )
        #self.output_matrix.setValue( c_matrix )
        self.output_info.setValue('info.tar')