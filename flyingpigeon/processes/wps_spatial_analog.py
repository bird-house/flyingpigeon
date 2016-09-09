"""
Processes for spatial analog calculation 
Author: Nils Hempelmann (info@nilshempelmann.de)
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
            identifier = "spatial_analog",
            title = "Spatial Analog",
            version = "0.9",
            #metadata= [
            #    {"title": "Bayerische Landesanstalt fuer Wald und Forstwirtschaft", "href": "http://www.lwf.bayern.de/"},
            #    {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
            #   ],

            abstract="Spatial analogs based on climate indices",
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

        # self.BBox = self.addBBoxInput(
        #     identifier="BBox",
        #     title="Bounding Box",
        #     abstract="coordinates to define the region to be analysed",
        #     minOccurs=1,
        #     maxOccurs=1,
        #     crss=['EPSG:4326']
        #     )


        self.coords = self.addLiteralInput(
          identifier="coords",
          title="Coordinates",
          abstract="a comma seperated touple of WGS85 lon,lat decimal coorinate",
          default="2.356138, 48.846450",
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
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
             
        self.output_indices = self.addComplexOutput(
            identifier="output_indices",
            title="Climate indices over all timesteps (3D)",
            abstract="Archive (tar/zip) containing calculated climate indices as netCDF files",
            formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
            asReference=True,
            )         
         
        self.output_analogs = self.addComplexOutput(
            identifier="output_analogs",
            title="Spatial Analogs",
            abstract="Archive (tar/zip) containing calculated climate indices as netCDF files",
            formats=[{"mimeType":"application/x-tar"}, {"mimeType":"application/zip"}],
            asReference=True,
            )

        self.output_example = self.addComplexOutput(
            identifier="output_example",
            title="Example Climate Analog",
            abstract="an example netCDF file picked from the Archive to be displayed on WMS",
            formats=[{"mimeType":"application/x-netcdf"}],
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
      from flyingpigeon import spatial_analog as sa
      from flyingpigeon.utils import archive

      self.status.set('Start process', 0)
      
      try: 
        logger.info('reading the arguments')
        resources = self.getInputValues(identifier='resources')
        #taxon_name = self.getInputValues(identifier='taxon_name')[0]
        #period = self.period.getValue()
        coords = self.getInputValues(identifier='coords')[0]
        period = self.getInputValues(identifier='period')[0]
        coordinate = [float(n) for n in coords.split(',')]
        
        #indices = self.input_indices.getValue()
        indices = self.getInputValues(identifier='input_indices')
        logger.info("indices = %s ", indices)
        
        archive_format = self.archive_format.getValue()
      except Exception as e: 
        logger.error('failed to read in the arguments %s ' % e)
     
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
        logger.debug(msg)
        # raise Exception(msg)

      try:
        archive_indices = archive(ncs_indices , format=archive_format)
        logger.info('indices 3D added to tarfile')
      except:
        msg = 'failed adding indices to tar'  
        logger.debug(msg)
        # raise Exception(msg)  

      indices_dic = None
      try: 
        # sort indices
        indices_dic = sdm.sort_indices(ncs_indices)
        logger.info('indice files sorted for %s Datasets' % len(indices_dic.keys()))
      except:
        msg = 'failed to sort indices'
        logger.debug(msg)
        # raise Exception(msg)

      ncs_references = []
      analogs = []
      statistics_info = []

      for count, key in enumerate(indices_dic.keys()):
        try:
          self.status.set('Start processing of %s ' % key, 40 + count * 10)
          ncs = indices_dic[key]
          logger.info('with %s files' % len(ncs))

          gam_model, statistic_plot = sa.get_gam(ncs, coordinate)
          statistics_info.append(statistic_plot)
          self.status.set('GAM sucessfully trained', 70)
        except:
          msg = 'failed to train GAM'  
          logger.debug(msg)
          # raise Exception(msg)

        try:
          prediction = sdm.get_prediction(gam_model, ncs_indices)
          self.status.set('prediction done', 80)
        except:
          msg = 'failed to predict'   
          logger.debug(msg)
          # raise Exception(msg)
          
      #   try:
      #     from numpy import invert, isnan, nan, broadcast_arrays, array, zeros, linspace, meshgrid
      #     mask = invert(isnan(PAmask))
      #     mask = broadcast_arrays(prediction, mask)[1]
      #     prediction[mask==False] = nan
      #     self.status.set('land sea mask for predicted data', 90)
      #   except: 
      #     logger.debug('failed to mask predicted data')

        try: 
          analogs.append(sdm.write_to_file(ncs_indices[0], prediction))

          logger.info('Analog written to file')
          #tar_prediction.add(species_file, 
           #               arcname = basename(species_file))#.replace(os.path.abspath(os.path.curdir), ""))
        except:
          msg = 'failed to write species file'
          logger.debug(msg)
          # raise Exception(msg)

      from flyingpigeon.visualisation import concat_images
      statistics_infos = None
      try: 
        statistics_infos = concat_images(statistics_info, orientation='v')
        logger.info('statistc graphics concatinated')
      except:
        msg = 'failed to concat images'  
        logger.debug(msg)
        # raise Exception(msg)  

      # # archive_references = None
      # # try:
      # #   archive_references = archive(ncs_references , format=archive_format)
      # #   logger.info('indices 2D added to archive')
      # # except:
      # #   msg = 'failed adding 2D indices to archive'  
      # #   logger.debug(msg)
      # #   # raise Exception(msg) 
      # archive_analogs = None
      
      try:
        archive_analogs = archive(analogs , format=archive_format)
        logger.info('analog file added to archive')
      except:
        msg = 'failed adding analog file to archive'  
        logger.debug(msg)
        # raise Exception(msg)  

      self.output_indices.setValue( archive_indices )
      self.output_analogs.setValue( archive_analogs )
      i = next((i for i, x in enumerate(analogs) if x), None)
      self.output_example.setValue (analogs[i])
      self.output_info.setValue(statistics_infos)

      self.status.set('done', 100)
      
