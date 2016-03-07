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
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.gbif = self.addLiteralInput(
            identifier="gbif",
            title="GBIF zip file",
            abstract="GBIF zip file containing a CSV files with tree locations",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            #maxmegabites=50,
            #formats=[{"mimeType":"application/zip"}],
            )



        #self.period_in = self.addLiteralInput(
            #identifier="period",
            #title="Select period",
            #abstract="Select between reference or projection period",
            #default="reference",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['reference','projection']
            #)
        
  
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
        
    def execute(self):
      
     
      from flyingpigeon import sdm

      #cdo = Cdo()
      
      # get the appropriate files
      logger.info('Start process')
      
      logger.info('read in the arguments')
      nc_files = self.getInputValues(identifier='resources')
      gbif = self.getInputValues(identifier='gbif')
      
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
      
      self.out_csv.setValue( csv_file )
      self.output_graphic.setValue( graphic )




