"""
Processes for Species distribution 
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""
#import tarfile
#import os

from pywps.Process import WPSProcess
import logging
logger = logging.getLogger(__name__)

from flyingpigeon.sdm import _SDMINDICES_

class SDMProcess(WPSProcess):
    
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "sdm_gbifsearch",
            title = "SDM -- GBIF search",
            version = "0.9",
            metadata= [
                {"title": "Bayerische Landesanstalt fuer Wald und Forstwirtschaft", "href": "http://www.lwf.bayern.de/"},
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],

            abstract="Species distribution model for tree species based on GBIF presence/absence data and climate indices",
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

        self.taxon_name = self.addLiteralInput(
            identifier="taxon_name",
            title="Tree Species",
            abstract="Scientific name of tree species",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            default='Fagus sylvatica'
            )
        
        self.input_indices = self.addLiteralInput(
            identifier="input_indices",
            title="Indices",
            abstract="Climate indices related to growth conditions of tree species",
            default=['TG_JJA', 'TNn_Jan'],
            type=type(''),
            minOccurs=1,
            maxOccurs=10,
            allowedValues=_SDMINDICES_ 
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate condition (all = entire timeseries)",
            default="all",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['all','1951-1980', '1961-1990', '1971-2000','1981-2010']
            )

        self.archive_format = self.addLiteralInput(
            identifier="archive_format",
            title="Archive format",
            abstract="Result files will be compressed into archives. Choose an appropriate format",
            default="tar",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['zip','tar']
            )

        
        ###########
        ### OUTPUTS
        ###########
        
        #self.output_csv = self.addComplexOutput(
            #identifier="output_csv",
            #title="Tree species table",
            #abstract="Extracted CSV file containing the tree species table ",
            #formats=[{"mimeType":"text/csv"}],
            #asReference=True,
            #)
        
        self.output_gbif = self.addComplexOutput(
            identifier="output_gbif",
            title="Graphic of GBIF coordinates",
            abstract="PNG graphic file showing the presence of tree species according to CSV file",
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
            title="Climate indices for growth conditions over all timesteps (3D)",
            abstract="Archive (tar/zip) containing calculated climate indices as netCDF files",
            formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
            asReference=True,
            )         
         
        self.output_reference = self.addComplexOutput(
            identifier="output_reference",
            title="Climate indices for growth conditions of reference period (2D)",
            abstract="Archive (tar/zip) containing calculated climate indices as netCDF files",
            formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
            asReference=True,
            )         
        
        self.output_prediction = self.addComplexOutput(
            identifier="output_prediction",
            title="predicted growth conditions",
            abstract="Archive (tar/zip) containing the netCDF files of the predicted growth conditions",
            formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
            asReference=True,
            )

        self.output_info = self.addComplexOutput(
            identifier="output_info",
            title="GAM statistics information",
            abstract="Graphics and information of the learning statistics",
            formats=[{"mimeType":"image/png"}],
            asReference=True,
            )

    def execute(self):
      from os.path import basename

      from flyingpigeon import sdm
      from flyingpigeon.utils import archive

      self.status.set('Start process', 0)
      
      try: 
        logger.info('reading the arguments')
        resources = self.getInputValues(identifier='resources')
        taxon_name = self.getInputValues(identifier='taxon_name')[0]
        #period = self.period.getValue()
        period = self.getInputValues(identifier='period')
        period = period[0]
        
        #indices = self.input_indices.getValue()
        indices = self.getInputValues(identifier='input_indices')
        logger.debug("indices = %s for %s ", indices, taxon_name)
        
        archive_format = self.archive_format.getValue()
      except Exception as e: 
        logger.error('failed to read in the arguments %s ' % e)
      logger.info('indices %s ' % indices)
      
      try:
        self.status.set('Fetching GBIF Data', 10)
        latlon = sdm.gbif_serach(taxon_name)
      except Exception as e: 
        logger.exception('failed to search gbif %s' % e)
        
      #try:
        #self.status.set('extract csv file with tree observations', 5)
        #csv_file = sdm.get_csv(taxon_name[0])
      #except Exception as e: 
        #logger.exception('failed to extract csv file from url.')

      #try:
        #self.status.set('read in latlon coordinates of tree observations', 10)
        #latlon = sdm.get_latlon(csv_file)
      #except Exception as e: 
        #logger.exception('failed to extract the latlon points')

      
      try:
        from flyingpigeon.visualisation import map_gbifoccurrences
        self.status.set('plotting Tree presents based on coordinates', 15)
        tree_presents = map_gbifoccurrences(latlon)
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
        import matplotlib.pyplot as plt
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
      ### calculate the climate indices
      #################################
      
      # get the indices
      ncs_indices = None
      try:
        self.status.set('start calculation of climate indices for %s' % indices, 30 )
        ncs_indices = sdm.get_indices(resources=resources, indices=indices)
        logger.info('indice calculation done')
      except:
        msg = 'failed to calculate indices'
        logger.exception(msg)
        raise Exception(msg)

      try:
        archive_indices = archive(ncs_indices , format=archive_format)
        logger.info('indices 3D added to tarfile')
      except:
        msg = 'failed adding indices to tar'  
        logger.exception(msg)
        raise Exception(msg)  

      indices_dic = None
      try: 
        # sort indices
        indices_dic = sdm.sort_indices(ncs_indices)
        logger.info('indice files sorted for %s Datasets' % len(indices_dic.keys()))
      except:
        msg = 'failed to sort indices'
        logger.exception(msg)
        raise Exception(msg)

      # try:
      #   # open tar files
      #   tar_reference = tarfile.open('reference.tar', "w")
      #   tar_indices = tarfile.open('indices.tar', "w")

      #   tar_info = tarfile.open('info.tar', "w")
      #   tar_prediction = tarfile.open('prediction.tar', "w")
        
      #   logger.info('tar files prepared')
      # except:
      #   msg = 'tar file preparation failed'
      #   logger.exception(msg)
      #   raise Exception(msg)


      ncs_references = []
      species_files = []
      statistics_info = []

      for count,key in enumerate(indices_dic.keys()):
        try:
          self.status.set('Start processing of %s ' % key, 40 + count * 10)
          
          ncs = indices_dic[key]
          
          logger.info('with %s files' % len(ncs))
            
          try: 
            ncs_references.extend(sdm.get_reference(ncs_indices=ncs, period=period))
            logger.info('reference indice calculated %s ' % ncs_references)
          except:
            msg = 'failed adding ref indices to tar'
            logger.exception(msg)
            raise Exception(msg)
          
          # for nc_reference in ncs_references:
          #   tar_reference.add(nc_reference, 
          #       arcname = basename(nc_reference))# nc_reference.replace(os.path.abspath(os.path.curdir), ""))
          
          # logger.info('reference indices added to tarfile')
          
        except:
          msg = 'failed to calculate reference indices.'
          logger.exception(msg)
          raise Exception(msg)

        try:
          gam_model, predict_gam, gam_info = sdm.get_gam(ncs_references,PAmask)
          statistics_info.append(gam_info)
          self.status.set('GAM sucessfully trained', 70)
        except:
          msg = 'failed to train GAM'  
          logger.exception(msg)
          raise Exception(msg)

        try:
          prediction = sdm.get_prediction(gam_model, ncs_indices)
          self.status.set('prediction done', 80)
        except:
          msg = 'failed to predict'   
          logger.exception(msg)
          raise Exception(msg)
          
        try:
          from numpy import invert, isnan, nan, broadcast_arrays, array, zeros, linspace, meshgrid
          mask = invert(isnan(PAmask))
          mask = broadcast_arrays(prediction, mask)[1]
          prediction[mask==False] = nan
          self.status.set('land sea mask for predicted data', 90)
        except: 
          logger.exception('failed to mask predicted data')

        try: 
          species_files.append(sdm.write_to_file(ncs_indices[0], prediction))

          logger.info('Favourabillity written to file')
          #tar_prediction.add(species_file, 
           #               arcname = basename(species_file))#.replace(os.path.abspath(os.path.curdir), ""))
        except:
          msg = 'failed to write species file'
          logger.exception(msg)
          raise Exception(msg)

      from flyingpigeon.visualisation import concat_images
      statistics_infos = None
      try: 
        statistics_infos = concat_images(statistics_info, orientation='v')
      except:
        msg = 'failed to concat images'  
        logger.exception(msg)
        raise Exception(msg)  

      archive_references = None
      try:
        archive_references = archive(ncs_references , format=archive_format)
        logger.info('indices 2D added to archive')
      except:
        msg = 'failed adding 2D indices to archive'  
        logger.exception(msg)
        raise Exception(msg)  

      archive_predicion = None
      try:
        archive_predicion = archive(species_files , format=archive_format)
        logger.info('species_files added to archive')
      except:
        msg = 'failed adding species_files indices to archive'  
        logger.exception(msg)
        raise Exception(msg)  

      # try:
      #   #tar_indices.close()
      #   #tar_reference.close()

      #   tar_prediction.close()
      #   #tar_info.close()
        
      #   logger.info('tar files closed')
      # except:
      #   logger.exception('tar file closing failed')
      #   raise Exception
           #self.output_csv.setValue( csv_file )
      self.output_gbif.setValue( tree_presents )
      self.output_PA.setValue( png_PA_mask )
      self.output_indices.setValue( archive_indices )
      self.output_reference.setValue (archive_references)
      self.output_prediction.setValue (archive_predicion)
      self.output_info.setValue(statistics_infos)

      self.status.set('done', 100)
      
