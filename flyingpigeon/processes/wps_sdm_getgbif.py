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
        
        self.output_csv = self.addComplexOutput(
            identifier="output_csv",
            title="Tree species table",
            abstract="Extracted CSV file containing the tree species table ",
            formats=[{"mimeType":"text/csv"}],
            asReference=True,
            )
        
        self.output_map = self.addComplexOutput(
            identifier="output_map",
            title="Graphic of species occurences",
            abstract="PNG graphic file showing the presence of tree species according to GBIF data fetch",
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
        gbifdic = sdm.get_gbif(taxon_name)
      except Exception as e: 
        logger.exception('failed to search gbif %s' % e)
      
      try:
        self.status.set('write csv file', 12)
        gbifcsv = sdm.gbifdic2csv(gbifdic)
      except Exception as e: 
        logger.exception('failed to write csv file %s' % e)

      try:
        self.status.set('plot map', 80)
        from flyingpigeon.visualisation import map_gbifoccurrences
        
        latlon = sdm.latlon_gbifdic(gbifdic)
        occurence_map = map_gbifoccurrences(latlon)
      except Exception as e: 
        logger.exception('failed to plot occurence map %s' % e)

      self.output_map.setValue( occurence_map )
      self.output_csv.setValue( gbifcsv )
      self.status.set('done', 100)
