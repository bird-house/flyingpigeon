from datetime import date 
from pywps.Process import WPSProcess

from flyingpigeon.datafetch import _PRESSUREDATA_

import logging
logger = logging.getLogger(__name__)

class AnalogsProcess(WPSProcess):
  
  def __init__(self):
    # definition of this process
    WPSProcess.__init__(self, 
      identifier = "analogs_detection",
      title="Analogs -- Detection",
      version = "0.2",
      metadata= [
              {"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"},
              {"title": "Dokumentation", "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#analog-pressure-pattern"}
              ],
      abstract="Search for days with analog pressure pattern",
      statusSupported=True,
      storeSupported=True
      )

    #self.resource = self.addComplexInput(
      #identifier="resource",
      #title="Resource",
      #abstract="URL to netCDF file",
      #minOccurs=0,
      #maxOccurs=1000,
      #maxmegabites=5000,
      #formats=[{"mimeType":"application/x-netcdf"}],
      #)

    self.experiment = self.addLiteralInput(
      identifier="experiment",
      title="Data experiment",
      abstract="Choose the experiment (if 'None' is selected, provide a resource)",
      default="NCEP_slp",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=_PRESSUREDATA_
      )

    #self.region = self.addBBoxInput(
      #identifier="region",
      #title="Region",
      #abstract="coordinates to define the region: (minlon,minlat,maxlon,maxlat)",
      ##default='-80,50,22.5,70', #"-80,22.5,50,70",
      ##type=type(''),
      #minOccurs=1,
      #maxOccurs=1,
      #crss=["EPSG:4326"]
      #)

    self.region = self.addLiteralInput(
      identifier="region",
      title="Region",
      abstract="coordinates to define the region: (minlon,minlat,maxlon,maxlat)",
      default='-80,22.5,50,70', #"-80,22.5,50,70",
      minOccurs=1,
      maxOccurs=1,
      type=type(''),
      )
         
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
      default="2013-01-01",
      type=type(date(1955,01,01)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.refEn = self.addLiteralInput(
      identifier="refEn",
      title="End reference period",
      abstract="End YEAR of reference period",
      default="2014-12-31",
      type=type(date(1957,12,31)),
      minOccurs=1,
      maxOccurs=1,
      )
    
    self.normalize = self.addLiteralInput(
      identifier="normalize",
      title="normalisation",
      abstract="Normalize by subtraction of annual cycle",
      default='base',
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['None','base','sim','own']
        )

    self.distance = self.addLiteralInput(
      identifier="dist",
      title="Distance",
      abstract="Distance function to define analogues",
      default='euclidean',
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['euclidean','mahalanobis','cosine','of']
        )

    self.outformat = self.addLiteralInput(
      identifier="outformat",
      title="output file format",
      abstract="Choose the output format for the analog output file",
      default="ascii",
      type=type(''),
      minOccurs=1,
      maxOccurs=1,
      allowedValues=['ascii','netCDF4']
      )

    self.timewin = self.addLiteralInput(
      identifier="timewin",
      title="Time window",
      abstract="Nr of days following the analog day",
      default=30,
      type=type(1),
      minOccurs=0,
      maxOccurs=1,
      )

    # self.variable = self.addLiteralInput(
    #   identifier="variable",
    #   title="Variable",
    #   abstract="Variable name in resource",
    #   default='slp',
    #   type=type(''),
    #   minOccurs=0,
    #   maxOccurs=1,
    #   )

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
    import time # performance test
    process_start_time = time.time() # measure process execution time ...
     
    from os import path
    from tempfile import mkstemp
    from flyingpigeon import analogs
    from datetime import datetime as dt

    from flyingpigeon.ocgis_module import call
    
    #from flyingpigeon.weatherregimes import get_NCEP
    from flyingpigeon.datafetch import reanalyses
    
    self.status.set('execution started at : %s '  % dt.now(),5)

    start_time = time.time() # measure init ...
    
    refSt = self.getInputValues(identifier='refSt')
    refEn = self.getInputValues(identifier='refEn')
    dateSt = self.getInputValues(identifier='dateSt')
    dateEn = self.getInputValues(identifier='dateEn')

    refSt = dt.strptime(refSt[0],'%Y-%m-%d')
    refEn = dt.strptime(refEn[0],'%Y-%m-%d')
    dateSt = dt.strptime(dateSt[0],'%Y-%m-%d')
    dateEn = dt.strptime(dateEn[0],'%Y-%m-%d')
    
    normalize = self.getInputValues(identifier='normalize')[0]
    if normalize == 'None': 
      seacyc = False
    else: 
      seacyc = True

    distance = self.getInputValues(identifier='dist')[0]
    outformat = self.getInputValues(identifier='outformat')[0]
    
    if outformat == 'ascii': 
      outformat = '.txt'
    elif outformat == 'netCDF':
      outformat = '.nc'
    else:
      logger.error('output format not valid')
    
    
    timewin = int(self.getInputValues(identifier='timewin')[0])
    start = min( refSt, dateSt )
    end = max( refEn, dateEn )
    region = self.getInputValues(identifier='region')[0]
    bbox = [float(b) for b in region.split(',')]
    experiment = self.getInputValues(identifier='experiment')[0]      
    dataset , var = experiment.split('_')

    try:            
      if dataset == 'NCEP': 
        if 'z' in var:
          variable='hgt'
          level=var.strip('z')
          #conform_units_to=None
        else:
          variable='slp'
          level=None
          #conform_units_to='hPa'
      elif '20CRV2' in var: 
        if 'z' in level:
          variable='hgt'
          level=var.strip('z')
          #conform_units_to=None
        else:
          variable='prmsl'
          level=None
          #conform_units_to='hPa'
      else:
        logger.error('Reanalyses dataset not known')          
      logger.info('environment set')
    except Exception as e: 
      msg = 'failed to set environment %s ' % e
      logger.error(msg)  
      raise Exception(msg)

    logger.debug("init took %s seconds.", time.time() - start_time)
    self.status.set('Read in the arguments', 5)
    #################
    # get input data
    #################

    start_time = time.time()  # measure get_input_data ...

    self.status.set('fetching input data', 7)
    try:
      input = reanalyses(start = start.year, end = end.year, variable=var, dataset=dataset)

      # if experiment == 'NCEP':
      # elif   experiment == 'None':
      #   input = self.getInputValues(identifier='resource')
      # else:
      #   logger.error('input experiment not found')
      #nc_subset = analogs.subset(resource=input, )
      nc_subset = call(resource=input, variable=var, geom=bbox, spatial_wrapping='wrap')
    except Exception as e :
      msg = 'failed to fetch or subset input files %s' % e
      logger.error(msg)
      raise Exception(msg)
    logger.debug("get_input_subset_dataset took %s seconds.", time.time() - start_time)
    self.status.set('**** Input data fetched', 10)
    
    ########################
    # input data preperation 
    ########################
    self.status.set('Start preparing input data', 12)
    start_time = time.time()  # mesure data preperation ...
    
    try: 
      archive = call(resource=nc_subset, time_range=[refSt , refEn]) 
      simulation = call(resource=nc_subset, time_range=[dateSt , dateEn])
      if seacyc == True:
        analogs.seacyc(archive, simulation, method=normalize)
    except Exception as e:
      msg = 'failed to prepare archive and simulation files %s ' % e
      logger.debug(msg)
      raise Exception(msg)
      
    ip, output = mkstemp(dir='.',suffix='.txt')
    output_file =  path.abspath(output)
    files=[path.abspath(archive), path.abspath(simulation), output_file]

    logger.debug("data preperation took %s seconds.", time.time() - start_time)

    ############################
    # generating the config file
    ############################
    
    self.status.set('writing config file', 15)
    start_time = time.time() # measure write config ...
    
    try:  
      config_file = analogs.get_configfile(files=files, 
        timewin=timewin, 
        varname=var, 
        seacyc=seacyc, 
        cycsmooth=91, 
        nanalog=20, 
        seasonwin=30, 
        distfun=distance,
        outformat=outformat,
        calccor=True,
        silent=False, 
        period=[dt.strftime(refSt,'%Y-%m-%d'),dt.strftime(refEn,'%Y-%m-%d')], 
        bbox="%s,%s,%s,%s" % (bbox[0],bbox[2],bbox[1],bbox[3]))
    except Exception as e:
      msg = 'failed to generate config file %s ' % e
      logger.debug(msg)
      raise Exception(msg)

    logger.debug("write_config took %s seconds.", time.time() - start_time)
      
    #######################
    # CASTf90 call 
    #######################
    import subprocess
    import shlex

    start_time = time.time() # measure call castf90
    
    self.status.set('Start CASTf90 call', 20)
    try:
      #self.status.set('execution of CASTf90', 50)
      cmd = 'analogue.out %s' % path.relpath(config_file)
      #system(cmd)
      args = shlex.split(cmd)
      output,error = subprocess.Popen(args, stdout = subprocess.PIPE, stderr= subprocess.PIPE).communicate()
      logger.info('analogue.out info:\n %s ' % output)
      logger.debug('analogue.out errors:\n %s ' % error)
      self.status.set('**** CASTf90 suceeded', 90)
    except Exception as e: 
      msg = 'CASTf90 failed %s ' % e
      logger.error(msg)  
      raise Exception(msg)

    logger.debug("castf90 took %s seconds.", time.time() - start_time)
    
    self.status.set('preparting output', 99)
    self.config.setValue( config_file )
    self.analogs.setValue( output_file )
    self.status.set('execution ended', 100)

    logger.debug("total execution took %s seconds.", time.time() - process_start_time)
