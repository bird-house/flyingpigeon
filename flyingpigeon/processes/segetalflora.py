from malleefowl.process import WPSProcess
import subprocess

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)


class segetalflora(WPSProcess):
  """This process calculates the relative humidity"""
  def __init__(self):
    WPSProcess.__init__(self, 
      identifier = "segetalflora",
      title="Segetal Flora",
      version = "0.1",
      metadata=[{"title": "Institut Pierre Simon Laplace", "href": "https://www.ipsl.fr/en/"}],
      abstract="Species biodiversity of segetal flora",
      )

    self.netcdf_file = self.addComplexInput(
      identifier="netcdf_file",
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
      allowedValues=["fallow", "intensiv", "extensiv", "all"] # sem
      )

    #complex output
    #-------------
    self.logout = self.addComplexOutput(
      identifier="logout",
      title="Indice log-file",
      abstract="logfile for segetalflora process",
      metadata=[],
      formats=[{"mimeType":"text/plain"}],
      asReference=True,
      )
    
    self.out_tas = self.addComplexOutput(
      title="tas_EUR",
      abstract="Tar archive containing the netCDF EUR tas mean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_tas",
      )

    self.out_polygons = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_polygons",
      )

    self.out_fieldmeans = self.addComplexOutput(
      title="fieldmeans",
      abstract="Tar archive containing the netCDF EU-countries fieldmeans segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="tar_fieldmeans",
      )

# calculation of number of segetal flora species
  def execute(self):
    
    from os import mkdir
    from os.path import curdir, join
    import tarfile
    import tempfile
    from datetime import datetime
    
    import utils
    
    logger.debug('starting segetalflora process execution')
    self.show_status('starting calcualtion segetalflora', 5)
    outlog = "Starting the segetalflora calculation at: %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
    
    ## prepare environment

    # create the tar files
    try: 
      (fp_tarf_tas, tarf_tas) = tempfile.mkstemp(dir=".", suffix='.tar')
      (fp_tarf_polygons, tarf_polygons) = tempfile.mkstemp(dir=".", suffix='.tar')
      (fp_tarf_fieldmeans, tarf_fieldmeans) = tempfile.mkstemp(dir=".", suffix='.tar')
      tar_tas = tarfile.open(tarf_tas, "w")
      tar_polygons = tarfile.open(tarf_polygons, "w")
      tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
      logger.debug('tar files initialized')
      
      # create output folders
      dir_tas = (curdir+'/dir_tas/')
      dir_polygons = (curdir+'/dir_polygons/')
      dir_fieldmeans = (curdir+'/dir_fieldmeans/')
      mkdir(dir_tas)
      mkdir(dir_polygons)
      mkdir(dir_fieldmeans)
      logger.debug('out directories created')
    except  Exception as e:
      msg = 'tar file or mkdir failed!: %s ' % (e)
      logger.error(msg)
      
    countries = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP','EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL','ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT','ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD','MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS']

    calc = [{'func':'mean','name':'tas'}]
    calc_grouping = ['year']
    
    # read argments to variables
    #try:  
    ncs = self.getInputValues(identifier='netcdf_file')
    climate_type = self.climate_type.getValue()
    culture_type = self.culture_type.getValue()
    
    if type(climate_type) != list:
      climate_type = list([climate_type])
    if type(culture_type) != list:
      climate_type = list([culture_type])
      
    logger.debug('urls for %s ncs found' % (len(ncs)))
    logger.debug('culture type: %s ' % (culture_type))
    #except  Exception as e:
      #msg = 'read in the argments failed: %s ' % (e)
      #logger.error(msg)
     
    # recreate the filenames for CORDEX convention
    #try:
    ncs_renamed = utils.filename_creator(ncs)
    logger.debug('urls for ncs_renamed: %s' % (len(ncs_renamed)))
    ncs_dic = utils.sort_by_filename(ncs_renamed)
    logger.debug('filename created and files sorted')
    self.show_status('working dir environment prepared', 7)
    #  logger.error(msg)
    
    ##
    # segetal flora calculation
    from flyingpigeon import segetalflora as sg
    from flyingpigeon import clipping
    from flyingpigeon import timeseries
    
    from cdo import *
    cdo = Cdo()

    for c, key in enumerate(ncs_dic):
      self.show_status('clipping Europe from %s/%s' %(c+1, len(ncs_dic)), 20)
      try: 
        prefix = key
        ncs_sort = utils.sort_by_time(ncs_dic[key])
        
        EUR_tas_mean = clipping.clip_continent(urls=ncs_sort, calc=calc, 
                                               calc_grouping=calc_grouping, prefix=prefix,
                                               continent='Europe', dir_output=dir_tas)
        fldmean = timeseries.fldmean(EUR_tas_mean, dir_output = dir_fieldmeans)
        outlog = outlog + '*** tas mean calculated for Europe with %s' %(ncs_dic[key])    
        for cult in range(len(culture_type)): 
          for clim in range(len(climate_type)):
            try: 
              eq = sg.get_equation(culture_type= culture_type[cult] , climate_type=climate_type[clim])
              sf_prefix = prefix.replace('tas_', 'sf_%s_%s_' %(culture_type[cult], climate_type[clim]))
              output = join(dir_polygons, sf_prefix+'.nc' ) 
              cdo.expr(eq , input=EUR_tas_mean, output=output)
              fldmean = timeseries.fldmean(output, dir_output = dir_fieldmeans)
              for country in countries:
                try:
                  self.show_status('processing model %s/%s cult %s climate %s country %s' %(c+1,len(ncs_dic), culture_type[cult], climate_type[clim], country ), 50)
                  EUR_seglo = clipping.clip_counties_EUR(urls=output, prefix= sf_prefix.replace('_EUR', '_%s'% (country)), dir_output = dir_polygons, country=country)
                  fldmean = timeseries.fldmean(EUR_seglo, dir_output = dir_fieldmeans)
                  outlog = outlog + '*** Processed for model  cult %s climate %s country %s' %(ncs_dic[key], culture_type[cult], climate_type[clim], country )
                except Exception as e:
                  msg = 'ocgis calculations failed : %s \n' % (e)
                  logger.exception(msg)
                  outlog = outlog + msg
            except Exception as e:
                  msg = 'subset calculation failed for %s, %s, %s: %s \n' % (key, culture_type[cult], climate_type[clim], e)
                  logger.exception(msg)
                  outlog = outlog + msg
      except Exception as e:
          msg = 'segeltalflora processing failed for %s, %s \n'% (key, e)
          logger.exception(msg)
          outlog = outlog + msg
          
    
    self.show_status('files to tar archives', 75)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(curdir, ""))
    tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(curdir, ""))
    tar_fieldmeans.add(dir_fieldmeans, arcname = dir_fieldmeans.replace(curdir, ""))
    tar_tas.close()
    tar_fieldmeans.close()
    tar_polygons.close()
    logger.debug('tar ncfiles closed')
    
    outlog = outlog + "Finishing the segetalflora calculation at: %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
    
    logfile = self.mktempfile(suffix='.txt')
    with open(logfile, 'w') as fp:
        fp.write(outlog)
    
    self.logout.setValue( logfile )
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_polygons.setValue( tarf_polygons )
    self.out_tas.setValue( tarf_tas )
    self.show_status("processing done", 100)
