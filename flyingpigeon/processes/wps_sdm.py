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
            default="TG_JJA",
            type=type(''),
            minOccurs=1,
            maxOccurs=3,
            allowedValues=['TG_JJA', 'TNn_Jan', 'PRCPTOT_JJA']
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate condition (all = entire timeserie)",
            default="all",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            allowedValues=['all','1951-1980', '1961-1990', '1971-2000','1981-2010']
            )
        
        ###########
        ### OUTPUTS
        ###########
        
        self.output_csv = self.addComplexOutput(
            identifier="output_csv",
            title="Tree species table",
            abstract="Extracted CSV file containing the tree species table ",
            formats=[{"mimeType":"text/csv"}],
            asReference=True,
            )
        
        self.output_gbif = self.addComplexOutput(
            identifier="output_gbif",
            title="Graphic of GBIF coordinates",
            abstract="PNG graphic file showing the presents of tree species acording to CSV file",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

        self.output_PA = self.addComplexOutput(
            identifier="output_PA",
            title="Graphic of PA mask",
            abstract="PNG graphic file showing PA mask generated based on netCDF spatial increment",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )
         
        self.output_indices = self.addComplexOutput(
            identifier="output_indices",
            title="Climate indices for growth condition of reference period",
            abstract="Tar file containing calculated climate indices as  netCDF files",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )         
        
        self.output_reference = self.addComplexOutput(
            identifier="output_reference",
            title="reference growth condition",
            abstract="Tar archive containing the graphic of the reference growing condition",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )

        self.output_prediction = self.addComplexOutput(
            identifier="output_prediction",
            title="predicted growth condition",
            abstract="Tar archive containing the netCDF files of the predicted growing condition",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )

        self.output_prediction = self.addComplexOutput(
            identifier="output_prediction",
            title="predicted growth condition",
            abstract="Tar archive containing the netCDF files of the predicted growing condition",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )

        self.output_info = self.addComplexOutput(
            identifier="output_info",
            title="GAM statistics information",
            abstract="Tar archive containing the mashine learning statistics",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )

    def execute(self):
      
      from flyingpigeon import sdm
      logger.info('Start process')
      
      try: 
        logger.info('read in the arguments')
        resources = self.getInputValues(identifier='resources')
        gbif = self.getInputValues(identifier='gbif')
        period = self.getInputValues(identifier='period')
        indices = self.getInputValues(identifier='indices')
        logger.info('extract csv file from url: %s ' % (gbif))
      except Exception as e: 
        logger.debug('failed to read in the arguments %s ' % e)
      
      try:
        csv_file = sdm.get_csv(gbif[0])
        logger.info('extract csv file with tree observations')
      except Exception as e: 
        logger.debug('failed to extract csv file from url %s' % e)
      
      try: 
        latlon = sdm.get_latlon(csv_file)
        logger.info('read in latlon coordinates of tree observations')
      except Exception as e: 
        logger.debug('failed to extract the latlon points %s' % e)
      
      try: 
        import matplotlib.pyplot as plt
        from cartopy import config
        from cartopy.util import add_cyclic_point
        import cartopy.crs as ccrs
        logger.info('libraries loaded')
      except Exception as e: 
        logger.debug('failed to load libraries: %s' % e)
      
      try: 
        fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')
        ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
        ax.coastlines()
        ax.set_global()
        cs = plt.scatter(latlon[:,1], latlon[:,0], transform=ccrs.PlateCarree())
        tree_presents = 'tree_presents.png'
        fig.savefig(tree_presents)
        plt.close()
        logger.info('Points of tree observation plotted')
      except Exception as e: 
        logger.debug('plotting points failed %s' % e)
      
      try:
        logger.info('generating the PA mask')
        PAmask = sdm.get_PAmask(coordinates=latlon)
        logger.info('PA mask sucessfully generated')
      except Exception as e: 
        logger.debug('failed to generate the PA mask %s' % e )
      
      try: 
        fig = plt.figure(figsize=(20,10), dpi=300, facecolor='w', edgecolor='k')
        cs = plt.contourf(PAmask)
        png_PA_mask = 'PA_mask.png'
        fig.savefig(png_PA_mask)
        plt.close()
        logger.info('PA mask plotted')
      except Exception as e: 
        logger.debug('failed to plot the PA mask %s' % e)
      
      #################################
      ### calculate the climate inidces
      #################################
      
      # get the indices
      try:
        logger.info('calculation of climate indices for reference period ')
        ncs_indices = sdm.get_indices(resources=resources, indices=indices)
        logger.info('indice calculation done')
      except Exception as e: 
        logger.debug('failed to calculate indices %s' % e)
      
      #################################
      ### mashine learning gam 
      #################################
      
      try: 
        # sort indices
        indices_dic = sdm.sort_indices(ncs_indices)
        logger.info('indice files sorted')
      except Exception as e: 
        logger.debug('failed to sort indices %s' % e)

      try:
        import tarfile
        import os
        
        # open 3 tar files
        tar_present = tarfile.open('present.tar', "w")
        tar_prdiction = tarfile.open('prediction.tar', "w")
        tar_present = tarfile.open('info.tar', "w")
        tar_indices = tarfile.open('indices.tar', "w")
        
        logger.info('tar files prepared')
      except Exception as e: 
        logger.debug('tar file preparation failed: %s' % e)

      for key in indices_dic.keys():
        try:
          ncs_indices = indices_dic[key]
          ncs_reference = sdm.get_reference(ncs_indices=ncs_indices, refperiod=period)
          
          for nc_indice in ncs_indices: 
            tar_indices.add(nc_indice, 
                          arcname = result.replace(os.path.abspath(os.path.curdir), ""))
        
          for nc_reference in ncs_reference: 
            tar_present.add(nc_reference, 
                arcname = result.replace(os.path.abspath(os.path.curdir), ""))
          
          logger.info('reference climate condition calculated %s ' % key)
        except Exception as e: 
          logger.debug('failed to calculate ncs_reference %s ' % e)

        try:
          gam_model, predict_gam, gam_info = sdm.get_gam(ncs_references,PApoints)
          tar_info.add(gam_info, 
                          arcname = result.replace(os.path.abspath(os.path.curdir), ""))
          logger.info('GAM sucessfully trained')
        except Exception as e: 
          logger.debug('failed to train GAM %s ' % e)

        try:
          prediction = sdm.get_prediction(gam_model, ncs_indices)
          logger.info('prediction done')
        except Exception as e: 
          logger.debug('failed to predict %s ' % e)
          
        try:
          from numpy import invert, isnan, nan, broadcast_arrays, array, zeros, linspace, meshgrid
          mask = invert(isnan(PApoints))
          mask = broadcast_arrays(prediction, mask)[1]
          prediction[mask==False] = nan
          logger.info('land sea mask for predicted data')
        except Exception as e: 
          logger.debug('failed to mask predicted data')

        try: 
          species_file = sdm.write_to_file(ncs_indices[0], prediction)
          logger.info('Favourabillity written to file')
          tar_prediction.add(species_file, 
                          arcname = result.replace(os.path.abspath(os.path.curdir), ""))
        except Exception as e:
          logger.debug('failed to write species file %s ' % e)

      try:
        tar_present.close()
        tar_prediction.close()
        tar_info.close()
        tar_indices.close()
        logger.info('tar files closed')
      except Exception as e:
        logger.debug('tar file closing failed %s' % e)
        
      self.output_csv.setValue( csv_file )
      self.output_gbif.setValue( tree_presents )
      self.output_PA.setValue( png_PA_mask )
      self.output_indices.setValue( 'indices.tar' )
      self.output_reference.setValue ('present.tar')
      self.output_prediction.setValue ('prediction.tar')
      self.output_info.setValue('info.tar')
      