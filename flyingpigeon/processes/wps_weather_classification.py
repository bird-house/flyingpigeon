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
            title="Bounding Box",
            abstract="coordinates to define the region: (minlon,minlat,maxlon,maxlat)",
            default="-80,22.5,50,70",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        
        ######################
        ### define the outputs
        ######################

        self.output_nc = self.addComplexOutput(
            identifier="output_nc",
            title="netCDF of required region",
            abstract="3D timeseries",
            formats=[{"mimeType":"application/netCDF"}],
            asReference=True,
            )

    def execute(self):
        
        logger.info('Start process')
      
        try: 
            logger.info('read in the arguments')
            
            resources = self.getInputValues(identifier='resources')
            bbox = self.getInputValues(identifier='bbox')
            logger.info('bbox %s' % str(bbox))
            

        except Exception as e: 
            logger.error('failed to read in the arguments %s ' % e)
        
        bbox = '-80,22.5,50,70'
        logger.info('bbox is set to %s' % bbox)     

        #####################    
        ### get the required bbox from resource
        #####################
        # from flyingpigeon.ocgis_module import call 
        
        from flyingpigeon.utils import sort_by_filename()
        from flyingpigeon import weatherclass as wc
        from cdo import *
        cdo = Cdo()        
        
        nsc = sort_by_filename(resource, historical_concatination=True)
        
        for key in ncs.keys():
          if len(ncs[key])>1:
            input = cdo.timmerge(input=ncs[key], output='merge.nc' )
          elif len(ncs[key])==1:
            input = ncs[key]
          else:
            logger.debug('invalid number of input files for dataset %s' % key)            
          nc  = cdo.sellonlatbox(bbox, input=input, output='subset.nc' )
          
          imgage = wc.tSNE(nc)
          
        # call 
        self.output_nc.setValue( nc )