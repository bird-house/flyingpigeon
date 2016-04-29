from datetime import date
 
from pywps.Process import WPSProcess

import logging
logger = logging.getLogger(__name__)

class AnalogsProcess(WPSProcess):
  
  def __init__(self):
    # definition of this process
    WPSProcess.__init__(self, 
      identifier = "analogs",
      title="Days with analog pressure pattern",
      version = "0.2",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}
              ],
      abstract="Search for days with analog pressure pattern",
      statusSupported=True,
      storeSupported=True
      )

    self.resource = self.addComplexInput(
      identifier="resource",
      title="Resource",
      abstract="URL to netCDF file",
      minOccurs=0,
      maxOccurs=1000,
      maxmegabites=5000,
      formats=[{"mimeType":"application/x-netcdf"}],
      )

    self.experiment = self.addLiteralInput(
      identifier="experiment",
      title="Data experiment",
      abstract="Choose the experiment (if 'None' is selected, provide a resource)",
      default="NCEP",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['None','NCEP']
      )

    
    self.region = self.addLiteralInput(
      identifier="region",
      title="Region",
      abstract="coordinates to define the region: (minlon,minlat,maxlon,maxlat)",
      default='-80,50,22.5,70', #"-80,22.5,50,70",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      )

    # self.region = self.addLiteralInput(
    #   identifier="region",
    #   title="Select Region",
    #   abstract="Select a predifined region",
    #   default="NA",
    #   type=type(''),
    #   minOccurs=1,
    #   maxOccurs=1,
    #   allowedValues=['NA']
    #   )
         
    self.dateSt = self.addLiteralInput(
      identifier="dateSt",
      title="Start date of analyse period",
      abstract="This is a Date: 2013-07-15",
      default="2013-07-15",
      type=type(date(2013,7,15)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.dateEn = self.addLiteralInput(
      identifier="dateEn",
      title="End date of analyse period",
      abstract="This is a Date: 2013-12-31",
      default="2014-12-31",
      type=type(date(2014,12,31)),
      minOccurs=1,
      maxOccurs=1,
      )

    self.refSt = self.addLiteralInput(
      identifier="refSt",
      title="Start reference period",
      abstract="Start YEAR of reference period",
      default="1955-01-01",
      type=type(date(1955,01,01)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.refEn = self.addLiteralInput(
      identifier="refEn",
      title="End reference period",
      abstract="End YEAR of reference period",
      default="1957-12-31",
      type=type(date(1957,12,31)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.normalize = self.addLiteralInput(
      identifier="normalize",
      title="Normalize",
      abstract="Normalize by substraction of annual cycle",
      default=False,
      type=type(False),
      minOccurs=1,
      maxOccurs=1,
        )

    self.timewin = self.addLiteralInput(
      identifier="timewin",
      title="Time window",
      abstract="Nr of days following the analog day",
      default=1,
      type=type(1),
      minOccurs=0,
      maxOccurs=1,
      )

    self.variable = self.addLiteralInput(
      identifier="variable",
      title="Variable",
      abstract="Variable name in resource",
      default='slp',
      type=type(''),
      minOccurs=0,
      maxOccurs=1,
      )

    # self.seacyc = self.addLiteralInput(
    #   identifier="seacyc",
    #   title="Seasonal Cycle",
    #   abstract="normalized by the Seasonal Cycle",
    #   default=True,
    #   type=type(boolean),
    #   minOccurs=0,
    #   maxOccurs=1,
    #   )

      # #seacyc=True, 
      # cycsmooth=91, 
      # nanalog=20, 
      # seasonwin=30, 
      # distfun='rms', 
      # calccor=True,
      # silent=False)

    ### ###################
    # define the outputs
    ### ###################


    self.config = self.addComplexOutput(
      identifier="config",
      title="Config File",
      abstract="Config file used for the Fortran process",
      formats=[{"mimeType":"text/plain"}],
      asReference=True,
      )

    self.analogs = self.addComplexOutput(
      identifier="analogs",
      title="Analogs File",
      abstract="mulit-column text file",
      formats=[{"mimeType":"text/plain"}],
      asReference=True,
      )

  def execute(self):

    from os import system, path
    from tempfile import mkstemp
    from flyingpigeon import analogs
    import datetime as dt

    from flyingpigeon.ocgis_module import call
    from flyingpigeon.weatherregimes import get_NCEP

    self.status.set('execution started at : %s '  % dt.datetime.now() , 5)

    refSt = self.getInputValues(identifier='refSt')
    refEn = self.getInputValues(identifier='refEn')
    dateSt = self.getInputValues(identifier='dateSt')
    dateEn = self.getInputValues(identifier='dateEn')
    
    normalize = self.getInputValues(identifier='normalize')[0]
    
    refSt = dt.datetime.strptime(refSt[0],'%Y-%m-%d')
    refEn = dt.datetime.strptime(refEn[0],'%Y-%m-%d')
    dateSt = dt.datetime.strptime(dateSt[0],'%Y-%m-%d')
    dateEn = dt.datetime.strptime(dateEn[0],'%Y-%m-%d')

    timewin = 1 #int(self.getInputValues(identifier='timewin')[0])
    
    start = min( refSt, dateSt )
    end = max( refEn, dateEn )

    #################
    # get input data
    #################

    try: 
      experiment = self.getInputValues(identifier='experiment')[0]
      if experiment == 'NCEP':
        input = get_NCEP(start = start.year, end = end.year )
      elif   experiment == 'None':
        input = self.getInputValues(identifier='resource')
      else:
        logger.error('input experiment not found')

      region = self.getInputValues(identifier='region')[0]
      nc_subset = analogs.subset(resource=input, 
                                 bbox=region, 
                                 normalize=normalize)
    except Exeption as e :
      msg = 'failed to fetch input files %s' % e
      logger.error(msg)
      raise Exeption(msg)
        
    ########################
    # input data preperation 
    ########################

    try: 
      archive = call(resource=nc_subset, time_range=[refSt , refEn]) 
      simulation = call(resource=nc_subset, time_range=[dateSt , dateEn]) 
    except Exception as e:
      msg = 'failed to prepare archive and simulation files %s ' % e
      logger.debug(msg)
      raise Exception(msg)
      
    ip, output = mkstemp(dir='/home/nils/data/analogs',suffix='.txt')
    output_file =  path.abspath(output)
    files=[path.abspath(archive), path.abspath(simulation), output_file]

    ############################
    # generating the config file
    ############################

    try:  
      config_file = analogs.get_configfile(files=files, 
        timewin=timewin, 
        varname='slp', 
        seacyc=normalize, 
        cycsmooth=91, 
        nanalog=20, 
        seasonwin=30, 
        distfun='rms', 
        calccor=True,
        silent=False)
    except Exception as e:
      msg = 'failed to generate config file %s ' % e
      logger.debug(msg)
      raise Exception(msg)
      
    #######################
    # CASTf90 call 
    #######################

    try:
      #self.status.set('execution of CASTf90', 50)
      cmd = 'analogue.out %s' % path.relpath(config_file)
      system(cmd)
    except Exception as e: 
      msg = 'CASTf90 failed %s ' % e
      logger.error(msg)  
      raise Exception(msg)

    self.status.set('preparting output', 99)
    self.config.setValue( config_file )
    self.analogs.setValue( output_file )
    self.status.set('execution ended', 100)