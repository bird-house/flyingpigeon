"""
Processes for Species distribution 
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class GAMProcess(WPSProcess):
    
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "sdm",
            title = "Species distribution model",
            version = "0.3",
            metadata=[
                {"title":"SDM"},
                ],
            abstract="Species distribution model (SDM) ",
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

        self.gbif = self.addLiteralInput(
            identifier="gbif",
            title="GBIF zip file",
            abstract="GBIF zip url containing a CSV files with tree locations (e.g.: http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip)",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            #maxmegabites=50,
            #formats=[{"mimeType":"application/zip"}],
            )
        
        self.indices = self.addLiteralInput(
            identifier="indices",
            title="Indices",
            abstract="Climate indices related to growth condition of tree species",
            default="TG_678",
            type=type(''),
            minOccurs=1,
            maxOccurs=3,
            allowedValues=['TG_678', 'TN_1', 'RR_678']
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate condition",
            default="1971/2000",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['1951/1980', '1961/1990', '1971/2000','1981/2010']
            )
        
  
        self.out_csv = self.addComplexOutput(
            identifier="out_csv",
            title="Tree species table",
            abstract="Extracted CSV file containing the tree species table ",
            formats=[{"mimeType":"text/csv"}],
            asReference=True,
            )
        
        self.output_graphic = self.addComplexOutput(
            identifier="output_graphic",
            title="Graphic",
            abstract="PNG graphic file showing the presents of tree species acording to CSV file",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

        self.PA_graphic = self.addComplexOutput(
            identifier="PA_graphic",
            title="Graphic of PA mask",
            abstract="PNG graphic file showing PA mask generated based on netCDF spatial increment",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )
        
    def execute(self):
      
      from flyingpigeon import sdm

      # get the appropriate files
      logger.info('Start process')
      
      logger.info('read in the arguments')
      nc_files = self.getInputValues(identifier='resources')
      gbif = self.getInputValues(identifier='gbif')
      period = self.getInputValues(identifier='period')
      
      indices = self.getInputValues(identifier='indices')
      
      logger.info('extract csv file from url: %s ' % (gbif))
      
      csv_file = sdm.get_csv(gbif[0]) 
      
      logger.info('extract lat lon coordinates')
      latlon = sdm.get_latlon(csv_file)
      
      try: 
        import matplotlib.pyplot as plt
        from cartopy import config
        from cartopy.util import add_cyclic_point
        import cartopy.crs as ccrs
        logger.info('libraries loaded')
      except Exception as e: 
        logger.error('failed to load libraries: %s' % e)
      
      fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')
      ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
      ax.coastlines()
      ax.set_global()
      
      cs = plt.scatter(latlon[:,0], latlon[:,1], transform=ccrs.PlateCarree())
      graphic = 'tree_presents.png'
      fig.savefig(graphic)
      plt.close()
      
      from scipy import spatial
      import numpy as np
      from netCDF4 import Dataset
      from flyingpigeon import config
      
      DIR_MASKS = config.masks_dir()
      
      
      nc = DIR_MASKS + '/sftlf_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_KNMI-RACMO22E_v1_fx.nc'

      ds = Dataset(nc, mode='r')

      lats = ds.variables['lat']
      lons = ds.variables['lon']

      domain = lats.shape
      
      lats1D = np.array(lats).ravel()
      lons1D = np.array(lons).ravel()
      
      tree = spatial.KDTree(zip(lons1D,lats1D))
      
      #tree.data
      l, i = tree.query(latlon)
      
      fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')

      PA = np.zeros(len(lats1D)) 
      PA[i] = 1

      ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
      ax.coastlines()
      cs = plt.scatter(np.array(lons).ravel(), np.array(lats).ravel(), c=PA, lw=0,  transform=ccrs.PlateCarree())

      png_PA_mask = 'PA_mask.png'
      fig.savefig(png_PA_mask)
      plt.close()      
      
      
      
      self.out_csv.setValue( csv_file )
      self.output_graphic.setValue( graphic )
      self.PA_graphic.setValue( png_PA_mask )




