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
            title = "Weather Regimes",
            version = "0.1",
            metadata=[
                {"title":"Weather Regimes"},
                ],
            abstract="Weather Regimes based on pressure patterns (kmean method)",
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
            default="-80,50,22.5,70",
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

        # self.method = self.addLiteralInput(
        #     identifier="method",
        #     title="Method",
        #     abstract="Choose a clustering method",
        #     default="kMEAN",
        #     type=type(''),
        #     minOccurs=1,
        #     maxOccurs=1,
        #     allowedValues=['tSNE', 'kMEAN']
        #     )

        self.observation = self.addLiteralInput(
            identifier="observation",
            title="Observation Data",
            abstract="Choose an observation dataset for comparison",
            default="NCEP",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['None', 'NCEP']
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

        self.output_maps = self.addComplexOutput(
            identifier="output_maps",
            title="Pressure pattern",
            abstract="Corresponding pressure maps for Weather Regimes",
            formats=[{"mimeType":"image/png"}],
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
      
        try: 
            logger.info('read in the arguments')
            resources = self.getInputValues(identifier='resources')
          #  method = self.getInputValues(identifier='method')
            time_region = self.getInputValues(identifier='time_region')[0]
            bbox = self.getInputValues(identifier='BBox')[0]
            obs = self.getInputValues(identifier='observation')[0]

            logger.info('bbox %s' % str(bbox))
            logger.info('time_region %s' % str(time_region))
           # logger.info('method: %s' % str(method))
            

        except Exception as e: 
            logger.error('failed to read in the arguments %s ' % e)
        
        #bbox = '-80,22.5,50,70'
        logger.info('bbox is set to %s' % bbox)     

        #####################    
        ### get the required bbox from resource
        #####################
        # from flyingpigeon.ocgis_module import call 
        
        from flyingpigeon.utils import sort_by_filename, get_time, get_coordinates  #calc_grouping
        from flyingpigeon import weatherclass as wc
        from flyingpigeon.visualisation import plot_kMEAN, concat_images, plot_pressuremap
        
        from datetime import datetime as dt
        from numpy import savetxt, column_stack
        
        import tarfile
        
        from cdo import *
        cdo = Cdo()        
        
        # grouping = calc_grouping(time_region)

        
        ncs = sort_by_filename(resources, historical_concatination=True)

        png_clusters = []
        txt_info = []
        png_pressuremaps = []
        
        ### Calculate reference for NCEP Data
        if obs == 'NCEP': 
          nc_ncep = wc.get_NCEP()

          subset = wc.subset(nc_ncep, time_region='12,1')
          
          data_ncep , pca_ncep = wc.get_pca(subset)
          
          kmeans_ncep = wc.calc_kMEAN(pca_ncep)
          c_ncep = kmeans_ncep.predict(pca_ncep)

          png_clusters.append(plot_kMEAN(kmeans_ncep, pca_ncep, title='kMEAN month: %s [lonlat: %s]' % (time_region,bbox), sub_title='file: NCEP Data'))
          logger.info('kMEAN calculated for NCEP Data')

          times = get_time(subset)
          lats, lons = get_coordinates(subset)

          subplots = []
          
          for i in range(4): 
              subplots.append(plot_pressuremap((data_ncep[c_ncep==i]), lats=lats, lons=lons, 
                title='Weather Regime %s: Month %s ' % (i, time_region), 
                sub_title='NCEP slp mean'))
          
          from PIL import Image
          import sys
          from tempfile import mkstemp

          open_subplots = map(Image.open, subplots)
          w = max(i.size[0] for i in open_subplots)
          h = max(i.size[1] for i in open_subplots)
          
          result = Image.new("RGB", (w*2, h*2))
          # p = h / len(open_subplots)
          c = 0 
          for i ,iw in enumerate([0,w]):
              for j, jh in enumerate([0,h]):
                  oi = open_subplots[c] 
                  c = c +1
              
                  cw = oi.size[0]
                  ch = oi.size[1]

                  box = [iw,jh,iw+cw,jh+ch]
                  result.paste(oi, box=box)

          ip, pressuremap = mkstemp(dir='.',suffix='.png')
          
          result.save(pressuremap)
          png_pressuremaps.append(pressuremap)

        try:
          # open tar files
          tar_info = tarfile.open('info.tar', "w")
          logger.info('tar files prepared')
        except:
          msg = 'tar file preparation failed'
          logger.exception(msg)
          raise Exception(msg)

        
        for key in ncs.keys():
          if len(ncs[key])>1:
            input = cdo.cat(input=ncs[key], output='merge.nc' )
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
            vals, pca = wc.get_pca(nc)            
            logger.info('PCa calculated')
          except:
            logger.debug('failed to calculate PCs')
            raise
          
          try:
            # if md == 'tSNE':
            #   data = wc.calc_tSNE(pca)
            #   png_clusters.append(plot_tSNE(data,title='tSNE month: %s [lonlat: %s]' % (time_region,bbox), sub_title='file: %s' % key))
            #   logger.info('tSNE calculated for %s ' % key)
            #if md == 'kMEAN':
            kmeans = wc.calc_kMEAN(pca)
            c = kmeans.predict(pca)
            times = get_time(nc)
            timestr = [t for t in times] # str(t).replace(' ','_') #dt.strftime(t, format='%Y-%d-%m_%H:%M:%S')
            tc = column_stack([timestr, c])
            fn = '%s.csv' % key
            
            savetxt(fn, tc, fmt='%s', delimiter=',', header='Date Time,WeatherRegime')

            tar_info.add(fn) #, arcname = basename(nc) 
            
            png_clusters.append(plot_kMEAN(kmeans, pca, title='kMEAN month: %s [lonlat: %s]' % (time_region,bbox), sub_title='file: %s' % key))
            logger.info('kMEAN calculated for %s ' % key)
            
            subplots = []
            lats, lons = get_coordinates(nc)
            for i in range(4): 
                subplots.append(plot_pressuremap((vals[c==i]/100),lats=lats, lons=lons, title='Weather Regime %s: Month %s ' % (i, time_region), sub_title='file: %s' % key))
            
            from PIL import Image
            import sys
            from tempfile import mkstemp

            open_subplots = map(Image.open, subplots)
            w = max(i.size[0] for i in open_subplots)
            h = max(i.size[1] for i in open_subplots)
            
            result = Image.new("RGB", (w*2, h*2))
            # p = h / len(open_subplots)
            c = 0 
            for i ,iw in enumerate([0,w]):
                for j, jh in enumerate([0,h]):
                    oi = open_subplots[c] 
                    c = c +1
                
                    cw = oi.size[0]
                    ch = oi.size[1]

                    box = [iw,jh,iw+cw,jh+ch]
                    result.paste(oi, box=box)

            ip, pressuremap = mkstemp(dir='.',suffix='.png')
            
            result.save(pressuremap)
            png_pressuremaps.append(pressuremap)
              
          except:
            logger.debug('faild to calculate cluster for %s' % key )
            raise

        c_clusters = concat_images(png_clusters)
        c_maps = concat_images(png_pressuremaps)
        
              
        try:
          tar_info.close()  
          logger.info('tar files closed')
        except Exception as e:
          logger.exception('tar file closing failed')


    

        # call 
        # self.output_nc.setValue( nc )
        self.output_clusters.setValue( c_clusters  )
        self.output_maps.setValue( c_maps  )
        self.output_info.setValue('info.tar')