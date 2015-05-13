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
      identifier="out_tas",
      )

    self.out_polygons = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_polygons",
      )

    self.out_fieldmeans = self.addComplexOutput(
      title="fieldmeans",
      abstract="Tar archive containing the netCDF EU-countries fieldmeans segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_fieldmeans",
      )

# calculation of number of segetal flora species
  def execute(self):
    
    from os import mkdir, path
    import tarfile
    from tempfile import  mkstemp
    from datetime import datetime
    
    from flyingpigeon import utils
    from flyingpigeon import timeseries
    from flyingpigeon import subsetting as sub
    
    logger.debug('starting segetalflora process execution')
    self.show_status('starting calcualtion segetalflora', 5)
    outlog = "Starting the segetalflora calculation at: %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
    
    ## prepare environment
    # create the tar files
    
    try: 
      (fp_tarf_tas, tarf_tas) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_polygons, tarf_polygons) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_fieldmeans, tarf_fieldmeans) = mkstemp(dir=".", suffix='.tar')
      tar_tas = tarfile.open(tarf_tas, "w")
      tar_polygons = tarfile.open(tarf_polygons, "w")
      tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
      logger.debug('tar files initialized')
      
      # create output folders
      dir_tas = (path.curdir+'/dir_tas/')
      dir_polygons = (path.curdir+'/dir_polygons/')
      dir_fieldmeans = (path.curdir+'/dir_fieldmeans/')
      mkdir(dir_tas)
      mkdir(dir_polygons)
      mkdir(dir_fieldmeans)
      logger.debug('out directories created')
    except  Exception as e:
      msg = 'tar file or mkdir failed!: %s ' % (e)
      logger.error(msg)
      
    countries = ['AUT','BEL','BGR','CYP','CZE','DEU','DNK','ESP',
                 'EST','FIN','FRA','GBR','GRC','HUN','HRV','IRL',
                 'ITA','LVA','LTU','LUX','MLT','NLD','POL','PRT',
                 'ROU','SVK','SVN','SWE','NOR','CHE','ISL','MKD',
                 'MNE','SRB','MDA','UKR','BIH','ALB','BLR','KOS']

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
    
    # first processing step: masking Europe for each file
    self.show_status('subset Europe', 7)
    ncs_europe = []
    
    if type(ncs) != list: 
      ncs = list([ncs])
    outlog = outlog + '*** %s file(s) will be processed \n' % (len(ncs))
    
    for c, nc in enumerate(ncs):
      try: 
        n = sub.masking(nc, 'Europe')
        nc_p = path.dirname(path.abspath(nc))
        name = utils.drs_filename(n, variable = 'tas', rename_file=True)# filename_creator(n, var='tas')
        ncs_europe.append(path.join(nc_p, name))
        logger.debug('*** subsetting done for %s at : %s \n' % (ncs_europe[-1], datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y')))
        outlog = outlog + '*** subsetting done for %s at: %s \n' % (ncs_europe[-1], datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
      except Exception as e:
        msg = 'subsetting failed for %s : %s \n' % (nc, e)
        logger.exception(msg)
        outlog = outlog + msg
    
    logger.debug('*** %s nc files masked and renamed \n' % (len( ncs_europe )))
    outlog = outlog + '*** %s nc files masked and renamed \n' % ( ncs_europe )
    
    try:
      nc_sorted = utils.sort_by_filename(ncs_europe)
      outlog = outlog + '### nc_sorted: %s \n ' % (nc_sorted)
      # ncs_merge = timeseries.merge(ncs_europe)
      msg = '*** sort and merge done for %s experiments at: %s \n' % (len(nc_sorted), datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
      logger.debug(msg)
      outlog = outlog + msg
    except Exception as e:
      msg = 'sort or merge failed: %s \n' % (e)
      logger.exception(msg)
      outlog = outlog + msg
  
    #  logger.error(msg)
    ##
    # segetal flora calculation
    from flyingpigeon import segetalflora as sg
    from flyingpigeon import clipping
    
    from cdo import *
    cdo = Cdo()
    
    #for c, nc in enumerate(ncs_merge):
      #self.show_status('timeseries Europe from %s/%s' %(c+1, len(ncs_dic)), 20)
      #try:
        #prefix = path.split(nc)[1].strip('.nc')
        #output = path.join(dir_polygons, prefix+'.nc' )
        #cdo.yearmean( input= nc, output=output)
        
        #fldmean = timeseries.fldmean(output, dir_output = dir_fieldmeans)
        #outlog = outlog + '*** tas mean calculated for Europe with %s' %(nc)    
        #for cult in range(len(culture_type)): 
          #for clim in range(len(climate_type)):
            #try: 
              #eq = sg.get_equation(culture_type= culture_type[cult] , climate_type=climate_type[clim])
              #sf_prefix = prefix.replace('tas_', 'sf_%s_%s_' %(culture_type[cult], climate_type[clim]))
              #output = path.join(dir_polygons, sf_prefix+'.nc' ) 
              #cdo.expr(eq , input=EUR_tas_mean, output=output)
              #fldmean = timeseries.fldmean(output, dir_output = dir_fieldmeans)
              #for country in countries:
                #try:
                  #self.show_status('processing model %s/%s cult %s climate %s country %s' %(c+1,len(ncs_dic), culture_type[cult], climate_type[clim], country ), 50)
                  #EUR_seglo = clipping.clip_counties_EUR(urls=output, prefix= sf_prefix.replace('_EUR', '_%s'% (country)), dir_output = dir_polygons, country=country)
                  #fldmean = timeseries.fldmean(EUR_seglo, dir_output = dir_fieldmeans)
                  #outlog = outlog + '*** Processed for model  cult %s climate %s country %s' %(ncs_dic[key], culture_type[cult], climate_type[clim], country )
                #except Exception as e:
                  #msg = 'ocgis calculations failed : %s \n' % (e)
                  #logger.exception(msg)
                  #outlog = outlog + msg
            #except Exception as e:
                  #msg = 'subset calculation failed for %s, %s, %s: %s \n' % (key, culture_type[cult], climate_type[clim], e)
                  #logger.exception(msg)
                  #outlog = outlog + msg
      #except Exception as e:
          #msg = 'segeltalflora processing failed for %s, %s \n'% (key, e)
          #logger.exception(msg)
          #outlog = outlog + msg
          
    
    self.show_status('files to tar archives', 75)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(path.curdir, ""))
    tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(path.curdir, ""))
    tar_fieldmeans.add(dir_fieldmeans, arcname = dir_fieldmeans.replace(path.curdir, ""))
    tar_tas.close()
    tar_fieldmeans.close()
    tar_polygons.close()
    logger.debug('tar ncfiles closed')
    
    outlog = outlog + "Finishing the segetalflora calculation at: %s \n" % (datetime.strftime(datetime.now(), '%H:%M:%S %d-%m-%Y'))
    
    l1, logfile = mkstemp(dir=".", suffix='.txt') 
    with open(logfile, 'w') as fp:
        fp.write(outlog)
    
    self.logout.setValue( logfile )
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_polygons.setValue( tarf_polygons )
    self.out_tas.setValue( tarf_tas )
    self.show_status("processing done", 100)
