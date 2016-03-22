"""
Processes for Weather Classification  
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
Author: Cathy Nangini 
"""

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class WClassProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "WClass",
            title = "Weather Classification",
            version = "0.1",
            metadata=[
                {"title":"Weather Classification"},
                ],
            abstract="Weather Classification based on pressure patterns (kmean method)",
            statusSupported=True,
            storeSupported=True
            )


        # Literal Input Data
        # ------------------

        self.resources = self.addComplexInput(
            identifier="resources",
            title="NetCDF File",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=500,
            maxmegabites=50000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        # self.bbox = self.addBBoxInput(
        #     identifier="bbox",
        #     title="Bounding Box",
        #     abstract="Region for weather classification",
        #     minOccurs=1,
        #     maxOccurs=1,
        #     crss=['EPSG:4326']
        #     )


        self.BBox = self.addLiteralInput(
            identifier="BBox",
            title="Region",
            abstract="coordinates to define the region: (minlon,minlat,maxlon,maxlat)",
            default="-80,22.5,50,70",
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
            allowedValues= ["10,11,12,1,2,3","4,5,6,7,8,9","12,1,2","3,4,5","6,7,8","9,10,11", "None"] #GROUPING
            )

        self.method = self.addLiteralInput(
            identifier="method",
            title="Method",
            abstract="Choose a clustering method",
            default="kMEAN",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['tSNE', 'kMEAN']
            )

        
        ######################
        ### define the outputs
        ######################

        #self.output_nc = self.addComplexOutput(
            #identifier="output_nc",
            #title="netCDF of required region",
            #abstract="3D timeseries",
            #formats=[{"mimeType":"application/netCDF"}],
            #asReference=True,
            #)

        self.output_clusters = self.addComplexOutput(
            identifier="output_clusters",
            title="Weather Classification Clusters",
            abstract="Weather Classification Clusters",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

    def execute(self):
        logger.info('Start process')
      
        try: 
            logger.info('read in the arguments')
            resources = self.getInputValues(identifier='resources')
            method = self.getInputValues(identifier='method')
            time_region = self.getInputValues(identifier='time_region')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            
            logger.info('bbox %s' % str(bbox))
            logger.info('time_region %s' % str(time_region))
            logger.info('method: %s' % str(method))
            

        except Exception as e: 
            logger.error('failed to read in the arguments %s ' % e)
        
        #bbox = '-80,22.5,50,70'
        logger.info('bbox is set to %s' % bbox)     

        #####################    
        ### get the required bbox from resource
        #####################
        # from flyingpigeon.ocgis_module import call 
        
        from flyingpigeon.utils import sort_by_filename # , calc_grouping
        from flyingpigeon import weatherclass as wc
        
        from flyingpigeon.visualisation import plot_tSNE, plot_kMEAN, concat_images
        
        from cdo import *
        cdo = Cdo()        
        
        # grouping = calc_grouping(time_region)
        ncs = sort_by_filename(resources, historical_concatination=True)

        png_clusters = []
        
        for key in ncs.keys():
          if len(ncs[key])>1:
            input = cdo.timmerge(input=ncs[key], output='merge.nc' )
          elif len(ncs[key])==1:
            input = ncs[key]
          else:
            logger.debug('invalid number of input files for dataset %s' % key)            
 
          #for tr in time_region:
          if not time_region == 'None':
            nc_grouped = cdo.selmon(time_region, input=input, output='grouped.nc')
          else:
            nc_grouped = input 
          
      #     for bb in bbox:    
          nc  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output='subset.nc')
          logger.info('nc subset: %s ' % nc)
          
          try:
            pca = wc.get_pca(nc)
            logger.info('PCa calculated')
          except:
            logger.debug('failed to calculate PCs')
            raise
          
          for md in method:
            try:
              if md == 'tSNE':
                data = wc.calc_tSNE(pca)
                png_clusters.append(plot_tSNE(data,title='tSNE month: %s [lonlat: %s]' % (time_region,bbox), sub_title='file: %s' % key))
                logger.info('tSNE calculated for %s ' % key)
              if md == 'kMEAN':
                kmeans = wc.calc_kMEAN(pca)
                png_clusters.append(plot_kMEAN(kmeans, pca, title='kMEAN month: %s [lonlat: %s]' % (time_region,bbox), sub_title='file: %s' % key))
                logger.info('kMEAN calculated for %s ' % key)
            except:
              logger.debug('faild to calculate cluster for %s' % key )
              raise

        image = concat_images(png_clusters)

        # call 
        # self.output_nc.setValue( nc )
        self.output_clusters.setValue( image )