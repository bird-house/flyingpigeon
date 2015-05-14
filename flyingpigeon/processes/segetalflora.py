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
    outlog = "%s : Starting the segetalflora calculation \n" % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'))
    
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
      dir_tas = path.abspath(path.curdir+'/dir_tas/')
      dir_polygons = path.abspath(path.curdir+'/dir_polygons/')
      dir_fieldmeans = path.abspath(path.curdir+'/dir_fieldmeans/')
      #dir_tmp = path.abspath(path.curdir+'/dir_tmp/')
      
      mkdir(dir_tas)
      mkdir(dir_polygons)
      mkdir(dir_fieldmeans)
      #mkdir(dir_tmp)
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
    
    if type(ncs) != list:
      ncs = list([ncs])
    if type(climate_type) != list:
      climate_type = list([climate_type])
    if type(culture_type) != list:
      culture_type = list([culture_type])
      
    logger.debug('urls for %s ncs found' % (len(ncs)))
    logger.debug('culture type: %s ' % (culture_type))
    
    # first processing step: masking Europe for each file
    
    ncs_europe = []
    
    if type(ncs) != list: 
      ncs = list([ncs])
    outlog = outlog + '%s : %s file(s), type: %s will be processed \n' % ( datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), len(ncs), type(ncs))
    
    for c, nc in enumerate(ncs):
      try:
        self.show_status('Mask Europe %s/%s ' % (c+1, len(ncs)), 7)
        nc_dir = path.dirname(path.abspath(nc))
        domain = utils.drs_filename(nc, variable = 'tas').split('_')[1]
        n = sub.masking(nc, domain)
        name = utils.drs_filename(n, variable = 'tas', rename_file=True) # filename_creator(n, var='tas')
        ncs_europe.append(path.join(path.dirname(path.abspath(nc)), name) )
        logger.debug('%s : subsetting done for %s \n' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S') , name ))
        outlog = outlog + '%s : subsetting done for %s \n' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), name )
        
      except Exception as e:
        msg = 'subsetting failed for %s : %s \n' % (nc, e)
        logger.exception(msg)
        outlog = outlog + msg
    
    logger.debug('%s nc file(s) masked and renamed \n' % (len( ncs_europe )))
    outlog = outlog + '%s : %s nc file(s) masked and renamed \n' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), len(ncs_europe))
    
    try:
      self.show_status('Sort and merge %s files %s according to experiments' % (len(ncs_europe), type(ncs_europe)), 10)
      ncs_merge = timeseries.merge(ncs_europe, dir_output = dir_tas )
      msg = '%s : sort and merge done for %s experiment(s) \n' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), len(ncs_merge))
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
    
    for c, nc in enumerate(ncs_merge):
      self.show_status('timeseries Europe from %s/%s nc type %s' %(c+1 , len(ncs_merge), type(nc)), 20)
      try:
        basename = path.basename(path.abspath(nc))
        self.show_status('basename: %s' %(basename), 20)
        EUR_tas_mean = path.join(dir_polygons, basename )
        cdo.yearmean( input = nc, output = EUR_tas_mean)
        fld = timeseries.fldmean( EUR_tas_mean , dir_output = dir_fieldmeans)
        fldmean = utils.drs_filename(fld[0], variable = 'tas', rename_file=True)
        outlog = outlog + '%s : tas year mean calculated %s \n' %(datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), basename)
        
        for cult in culture_type: 
          for clim in climate_type:
            try:
              self.show_status('processing model %s/%s cult %s climate %s \n' %(c+1,len(ncs_merge), cult , clim ), 50)
              eq = sg.get_equation( culture_type=cult, climate_type=clim) 
              sf_prefix = basename.strip('.nc').replace('tas_', 'sf-%s-%s_' % (cult, clim))
              output = path.join(dir_polygons, sf_prefix+'.nc' ) 
              cdo.expr(eq , input=EUR_tas_mean, output=output)
              fldmean = timeseries.fldmean(output, dir_output = dir_fieldmeans)
              outlog = outlog + '%s : segetalflora processed for model %s \n' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), sf_prefix )
              for country in countries:
                try:
                  sf_country_prefix = sf_prefix.replace('_EUR-', '_%s-' % (country))
                  EUR_seglo = clipping.clip_counties_EUR(urls=output, prefix= sf_country_prefix, dir_output = dir_polygons, country=country)
                  fldmean = timeseries.fldmean(EUR_seglo, dir_output = dir_fieldmeans)
                  outlog = outlog + '%s : Processed for model %s \n' %(datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), sf_country_prefix)
                except Exception as e:
                  msg = 'Processed for model %s failed: %s \n' %( sf_country_prefix, e) 
                  logger.exception(msg)
                  outlog = outlog + msg
            except Exception as e:
                  msg = '%s : segetalflora failed for model %s : %s' % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), sf_prefix, e ) 
                  logger.exception(msg)
                  outlog = outlog + msg
      except Exception as e:
        msg = 'tas year mean calculation failed %s : %s\n' %( nc, e) 
        logger.exception(msg)
        outlog = outlog + msg
        
    
    self.show_status('files to tar archives', 75)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(path.abspath(path.curdir), ""))
    tar_polygons.add(dir_polygons, arcname = dir_polygons.replace(path.abspath(path.curdir), ""))
    tar_fieldmeans.add(dir_fieldmeans, arcname = dir_fieldmeans.replace(path.abspath(path.curdir), ""))
    tar_tas.close()
    tar_fieldmeans.close()
    tar_polygons.close()
    logger.debug('tar ncfiles closed')
    
    outlog = outlog + "%s Finishing the segetalflora calculation \n" % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'))
    
    l1, logfile = mkstemp(dir=".", suffix='.txt') 
    with open(logfile, 'w') as fp:
        fp.write(outlog)
    
    self.logout.setValue( logfile )
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_polygons.setValue( tarf_polygons )
    self.out_tas.setValue( tarf_tas )
    self.show_status("processing done", 100)
