"""
Processes for Species distribution 
Author: Nils Hempelmann (info@nilshempelmann.de)
"""

from pywps.Process import WPSProcess
import logging
logger = logging.getLogger(__name__)

class GETGBIFProcess(WPSProcess):
    
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "sdm_getgbif",
            title = "SDM -- GBIF search",
            version = "0.1",
            metadata= [
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                {"title" : "GBIF", "href": "http://gbif.org/"}
                ],
            abstract="Species occurence search in Global Biodiversity Infrastructure Facillity (GBIF)",
            statusSupported=True,
            storeSupported=True
            )

        # Literal Input Data
        # ------------------

        self.taxon_name = self.addLiteralInput(
            identifier="taxon_name",
            title="Tree Species",
            abstract="Scientific name of tree species",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            default='Fagus sylvatica'
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

    def execute(self):
      self.status.set('Start process', 0)
      from flyingpigeon import sdm

      try: 
        logger.info('reading the arguments')
        taxon_name = self.getInputValues(identifier='taxon_name')[0]
        logger.debug("Taxon Name = %s", taxon_name)
      except Exception as e: 
        logger.error('failed to read in the arguments %s ' % e)
      
      try:
        self.status.set('Fetching GBIF Data', 10)
        latlon = sdm.gbif_serach(taxon_name)
      except Exception as e: 
        logger.exception('failed to search gbif %s' % e)
        
      # try:
      #   self.status.set('extract csv file with tree observations', 5)
      #   csv_file = sdm.get_csv(taxon_name)
      # except Exception as e: 
      #   logger.exception('failed to extract csv file from url.')

      # try:
      #   self.status.set('read in latlon coordinates of tree observations', 10)
      #   latlon = sdm.get_latlon(csv_file)
      # except Exception as e: 
      #   logger.exception('failed to extract the latlon points')
      
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

      self.output_gbif.setValue( tree_presents )
      self.output_PA.setValue( png_PA_mask )
      self.status.set('done', 100)