"""
Processes for Species distribution 
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

import tarfile
import os
from os.path import basename

from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)


class SDMProcess(WPSProcess):
    
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "sdm",
            title = "Species distribution model",
            version = "0.4",
            metadata=[
                {"title":"SDM", "href":"http://flyingpigeon.readthedocs.org/en/latest/"},
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
            default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            #maxmegabites=50,
            #formats=[{"mimeType":"application/zip"}],
            )
        
        self.indices = self.addLiteralInput(
            identifier="indices",
            title="Indices",
            abstract="Climate indices related to growth condition of tree species",
            #default="TG_JJA",
            default="all",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['all', 'TG_JJA', 'TNn_Jan'] # 'PRCPTOT_JJA'
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
            title="Climate indices for growth condition over all timestepps (3D)",
            abstract="Tar file containing calculated climate indices as  netCDF files",
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )         
         
        self.output_reference = self.addComplexOutput(
            identifier="output_reference",
            title="Climate indices for growth condition of reference period (2D)",
            abstract="Tar file containing calculated climate indices as  netCDF files",
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
      self.status.set('Start process', 0)
      
      try: 
        logger.info('read in the arguments')
        resources = self.getInputValues(identifier='resources')
        gbif = self.getInputValues(identifier='gbif')
        period = self.getInputValues(identifier='period')
        period = period[0]
        indices = self.getInputValues(identifier='indices')
        if 'all' in indices:
            indices = ['TG_JJA', 'TNn_Jan'] # 'PRCPTOT_JJA'
      except Exception as e: 
        logger.error('failed to read in the arguments %s ' % e)
      logger.info('indices %s ' % indices)
      try:
        self.status.set('extract csv file with tree observations', 5)
        csv_file = sdm.get_csv(gbif[0])
      except Exception as e: 
        logger.exception('failed to extract csv file from url.')

      try:
        self.status.set('read in latlon coordinates of tree observations', 10)
        latlon = sdm.get_latlon(csv_file)
      except Exception as e: 
        logger.exception('failed to extract the latlon points')

      tree_presents = 'tree_presents.png'
      try:
        self.status.set('plotting Tree presents based on coordinates', 15)
        import matplotlib.pyplot as plt
        from cartopy import config
        from cartopy.util import add_cyclic_point
        import cartopy.crs as ccrs
      
        fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')
        ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
        ax.coastlines()
        ax.set_global()
        cs = plt.scatter(latlon[:,1], latlon[:,0], transform=ccrs.PlateCarree())
        fig.savefig(tree_presents)
        plt.close()
      except Exception as e:
        msg = 'plotting points failed'   
        logger.exception(msg)
        with open(tree_presents, 'w') as fp:
            # TODO: needs to be a png file
            fp.write(msg)
      
      try:
        self.status.set('generating the PA mask', 20)
        PAmask = sdm.get_PAmask(coordinates=latlon)
        logger.info('PA mask sucessfully generated')
      except Exception as e: 
        logger.exception('failed to generate the PA mask')
        
      png_PA_mask = 'PA_mask.png'
      try: 
        self.status.set('Ploting PA mask', 25)
        fig = plt.figure(figsize=(20,10), dpi=300, facecolor='w', edgecolor='k')
        cs = plt.contourf(PAmask)
        fig.savefig(png_PA_mask)
        plt.close()
      except Exception as e:
        msg = 'failed to plot the PA mask'
        logger.exception(msg)
        with open(png_PA_mask, 'w') as fp:
            # TODO: needs to be a png file
            fp.write(msg)
      
      #################################
      ### calculate the climate inidces
      #################################
      
      # get the indices
      ncs_indices = None
      try:
        self.status.set('start calculation of climate indices for %s' % indices, 30 )
        ncs_indices = sdm.get_indices(resources=resources, indices=indices)
        logger.info('indice calculation done')
      except Exception as e:
        msg = 'failed to calculate indices'
        logger.exception(msg)
        raise Exception(msg)

      indices_dic = None
      try: 
        # sort indices
        indices_dic = sdm.sort_indices(ncs_indices)
        logger.info('indice files sorted for %s Datasets' % len(indices_dic.keys()))
      except Exception as e:
        msg = 'failed to sort indices'
        logger.exception(msg)
        raise Exception(msg)

      try:
        # open tar files
        tar_reference = tarfile.open('reference.tar', "w")
        tar_indices = tarfile.open('indices.tar', "w")

        tar_info = tarfile.open('info.tar', "w")
        tar_prediction = tarfile.open('prediction.tar', "w")
        
        logger.info('tar files prepared')
      except Exception as e:
        msg = 'tar file preparation failed'
        logger.exception(msg)
        raise Exception(msg)

      for count,key in enumerate(indices_dic.keys()):
        try:
          self.status.set('Start processing of %s ' % key, 40 + count * 10)
          
          ncs = indices_dic[key]
          
          logger.info('with %s files' % len(ncs))
          
          try:
            for nc in ncs: 
              tar_indices.add(nc, 
                            arcname = basename(nc) )# .replace(os.path.abspath(os.path.curdir), ""))
            logger.info('indices added to tarfile for %s' % key)
          except:
            msg = 'failed adding indices to tar'  
            logger.exception(msg)
            raise Exception(msg)
            
          try: 
            ncs_references = sdm.get_reference(ncs_indices=ncs, period=period)
            logger.info('reference indice calculated %s ' % ncs_references)
          except:
            msg = 'failed adding ref indices to tar'
            logger.exception(msg)
            raise Exception(msg)
          
          for nc_reference in ncs_references:
            tar_reference.add(nc_reference, 
                arcname = basename(nc_reference))# nc_reference.replace(os.path.abspath(os.path.curdir), ""))
          
          logger.info('reference indices added to tarfile')
        except Exception as e:
          msg = 'failed to calculate reference indices.'
          logger.exception(msg)
          raise Exception(msg)

        try:
          gam_model, predict_gam, gam_info = sdm.get_gam(ncs_references,PAmask)
          tar_info.add(gam_info, arcname = "%s.pdf" % key)
          self.status.set('GAM sucessfully trained', 70)
        except Exception as e:
          msg = 'failed to train GAM'  
          logger.exception(msg)
          raise Exception(msg)

        try:
          prediction = sdm.get_prediction(gam_model, ncs_indices)
          self.status.set('prediction done', 80)
        except Exception as e:
          msg = 'failed to predict'   
          logger.exception(msg)
          raise Exception(msg)
          
        try:
          from numpy import invert, isnan, nan, broadcast_arrays, array, zeros, linspace, meshgrid
          mask = invert(isnan(PApoints))
          mask = broadcast_arrays(prediction, mask)[1]
          prediction[mask==False] = nan
          self.status.set('land sea mask for predicted data', 90)
        except Exception as e: 
          logger.exception('failed to mask predicted data')

        try: 
          species_file = sdm.write_to_file(ncs_indices[0], prediction)
          logger.info('Favourabillity written to file')
          tar_prediction.add(species_file, 
                          arcname = basename(species_file))#.replace(os.path.abspath(os.path.curdir), ""))
        except Exception as e:
          logger.exception('failed to write species file')

      try:
        tar_indices.close()
        tar_reference.close()

        tar_prediction.close()
        tar_info.close()
        
        logger.info('tar files closed')
      except Exception as e:
        logger.exception('tar file closing failed')
        
      self.output_csv.setValue( csv_file )
      self.output_gbif.setValue( tree_presents )
      self.output_PA.setValue( png_PA_mask )
      self.output_indices.setValue( 'indices.tar' )
      self.output_reference.setValue ('reference.tar')
      self.output_prediction.setValue ('prediction.tar')
      self.output_info.setValue('info.tar')

      self.status.set('done', 100)
      
