from pywps.Process import WPSProcess

from flyingpigeon.subset import countries #REGION_EUROPE

import logging
logger = logging.getLogger(__name__)

class SegetalfloraProcess(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "segetalflora",
      title="Segetal Flora",
      version = "0.9",
      
      metadata= [
                {"title": "Julius Kuehn Institut", "href": "http://www.jki.bund.de/"},
                {"title": "Documentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],
      abstract="Species biodiversity of segetal flora. Imput files: variable:tas , domain: EUR-11 or EUR-44",
      statusSupported=True,
      storeSupported=True
      )

    self.resource = self.addComplexInput(
      identifier="resource",
      title="NetCDF Files",
      abstract="NetCDF File",
      minOccurs=1,
      maxOccurs=1000,
      maxmegabites=500000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.climate_type = self.addLiteralInput(
      identifier="climate_type",
      title="Climate type",
      abstract="Select climate type",
      default='3',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["1", "2", "3", "4", "5", "6", "7", "all"] # sem
      )

    self.culture_type = self.addLiteralInput(
      identifier="culture_type",
      title="Culture type",
      abstract="Select culture type",
      default='fallow',
      type=type(''),
      minOccurs=1,
      maxOccurs=8,
      allowedValues=["fallow", "intensive", "extensive"] # sem
      )
    
    #self.region = self.addLiteralInput(
      #identifier="region",
      #title="Region",
      #abstract="European Regions ...",
      #default='FRA',
      #type=type(''),
      #minOccurs=0,
      #maxOccurs=25,
      #allowedValues=countries() 
      #)


    #complex output
    #-------------
    #self.logout = self.addComplexOutput(
      #identifier="logout",
      #title="Indice log-file",
      #abstract="logfile for segetalflora process",
      #metadata=[],
      #formats=[{"mimeType":"text/plain"}],
      #asReference=True,
      #)
    
    self.out_tasmean = self.addComplexOutput(
      title="Yearly mean temperature",
      abstract="Tar archive containing the netCDF EUR tas mean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_tasmean",
      )

    self.out_segetalflora = self.addComplexOutput(
      title="Segetalflora",
      abstract="Tar archive containing the segetalflora data ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_segetalflora",
      )

    #self.out_fieldmeans = self.addComplexOutput(
      #title="fieldmeans",
      #abstract="Tar archive containing the netCDF EU-countries fieldmeans segetalflora ",
      #formats=[{"mimeType":"application/x-tar"}],
      #asReference=True,
      #identifier="out_fieldmeans",
      #)

    #self.out_plots = self.addComplexOutput(
      #title="Graphics",
      #abstract="Tar archive containing graphics for segetalflora ",
      #formats=[{"mimeType":"application/x-tar"}],
      #asReference=True,
      #identifier="out_plots",
      #)

# calculation of number of segetal flora species
  def execute(self):
    
    from os import mkdir, path, listdir 
    
    # import tarfile
    # from tempfile import  mkstemp #, mkdtemp
    # from datetime import datetime
    
    from flyingpigeon import segetalflora as sf
    
    logging.debug('starting segetalflora process execution')
    self.status.set('starting calcualtion segetalflora', 5)
    
    ############################
    # read argments to variables
    ############################
    try:  
      resource = self.getInputValues(identifier='resource')
      climate_type = self.climate_type.getValue()
      culture_type = self.culture_type.getValue()
      
      logging.info('urls for %s ncs found' % (len(resource)))
      logging.info('culture type: %s ' % (culture_type))
    except Exception as e: 
      logger.debug('failed to read in the arguments: %s ' % e)

    try:  
      if type(climate_type) != list:
        climate_type = list([climate_type])
      if type(culture_type) != list:
        culture_type = list([culture_type])
      logger.info('arguments are lists')  
    except Exception as e: 
      logger.debug('failed to transform arguments to lists: %s ' % e)

    #############################
    # get yearly mean temperature 
    #############################
    
    nc_tasmean = sf.get_yrmean(resource)
    
    #######################################
    # main call for segetalflora processing    
    #######################################
    
    nc_sf = sf.get_segetalflora(resource=nc_tasmean, culture_type=culture_type, climate_type=climate_type) 
    
    #stepps = len(culture_type) * len(climate_type)
    #for a, cult in enumerate(culture_type): 
      #for b, clim in enumerate(climate_type):
        #start = (a + 1) * (b + 1) 
        #per = (start / stepps) * 95
        #self.status.set('%s/%s processing for %s climate type: %s' %(start, stepps, culture_type, climate_type), per)
        #try:
          #sf_files =  sf.get_segetalflora(resource=ncs, culture_type=cult, climate_type=clim) # dir_output=dir_tas,
          #self.status.set("processing of %s segetalflora files done " % (len(sf_files)) , 95)
        #except Exception as e:
          #logging.exception('segetalflora calculation failed %s %s' % ( climate_type, culture_type))
          #raise
        
## === fieldmeans         
    #from flyingpigeon import timeseries as ts
    #self.status.set('processing fieldmeans' , 97)
    #try:
      #ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora)]
      #ncs_fld = ts.fldmean(ncs, dir_output=dir_fieldmean)
      #logging.debug('%s fieldmeans processed' % (len(ncs_fld)))
    #except Exception as e:
      #logging.exception('fieldmeans failed')
      #raise
    
## === visualisation 
    #from flyingpigeon import visualisation as vs
    #from os import rename
    
    #self.status.set('processing visualisation' , 98)
    
    ## sort files for plotting
    #try:
      #ncs = listdir(dir_segetalflora)
      #set_var = set()
      #set_contry = set()
      #for nc in ncs: 
        #set_var = set_var.union([nc.split('_')[0]])
        #set_contry = set_contry.union([nc.split('_')[1]])
      #logging.debug('%s files to plots sorted' % (len(ncs)))
    #except Exception as e:
      #logging.exception('files sorting failed')
      #raise
      
    ## plot sorted files 
    #try:
      #plots = []
      #for v in set_var: 
        #for c in set_contry: 
          #ncs = [path.join(dir_segetalflora,nc) for nc in listdir(dir_segetalflora) if v in nc and c in nc ]
          #p = vs.spaghetti(ncs,
                           #variable=v,
                           #title='Segetalflora %s in %s' % (v, c),
                           #dir_out=dir_plots)
          #newname = path.dirname(p)+'/%s_%s_birdhouse_output.html' %(v,c)
          #rename(p,newname)
          #plots.append(newname)
          #logging.debug('plot created and renamed for %s %s' % (v, c )) 
    #except Exception as e:
      #logging.exception('ploting failed')
      #raise

    ####################
    # tar file archiving
    ####################
    from flyingpigeon.utils import archive
    
    try: 
      self.status.set('files to tar archives', 99)
      tar_sf = archive(nc_sf, format='tar', dir_output='.', mode='w')
      tar_tasmean = archive(nc_tasmean, format='tar', dir_output='.', mode='w')
      logging.info('Archives prepared')
    except Exception as e: 
      logger.debug('failed to archive files %s' % e )    
   
# === set output parameter   
    #self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_segetalflora.setValue( tar_sf )
    self.out_tasmean.setValue( tar_tasmean )
    #self.out_plots.setValue( tarf_plots )
    self.status.set("processing done", 100)
