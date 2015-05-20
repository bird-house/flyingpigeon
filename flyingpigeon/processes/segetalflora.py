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
      allowedValues=["fallow", "intensiv", "extensiv"] # sem
      )

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
    
    self.out_tas = self.addComplexOutput(
      title="tas_EUR",
      abstract="Tar archive containing the netCDF EUR tas mean files",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_tas",
      )

    self.out_segetalflora = self.addComplexOutput(
      title="polygons",
      abstract="Tar archive containing the netCDF EU-countries polygons segetalflora ",
      formats=[{"mimeType":"application/x-tar"}],
      asReference=True,
      identifier="out_segetalflora",
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
    from tempfile import  mkstemp #, mkdtemp
    from datetime import datetime
    
    #from cdo import *
    #cdo = Cdo()
    
    #from flyingpigeon import utils
    #from flyingpigeon import timeseries
    #from flyingpigeon import subsetting as sub
    from flyingpigeon import segetalflora as sf
    #from flyingpigeon import clipping
    
    logger.debug('starting segetalflora process execution')
    self.show_status('starting calcualtion segetalflora', 5)
    #outlog = "%s : Starting the segetalflora calculation \n" % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'))
    
    ## prepare environment
    # create the tar files
    
    try: 
      (fp_tarf_tas, tarf_tas) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_segetalflora, tarf_segetalflora) = mkstemp(dir=".", suffix='.tar')
      (fp_tarf_fieldmeans, tarf_fieldmeans) = mkstemp(dir=".", suffix='.tar')
      tar_tas = tarfile.open(tarf_tas, "w")
      tar_segetalflora = tarfile.open(tarf_segetalflora, "w")
      tar_fieldmeans = tarfile.open(tarf_fieldmeans, "w")
      logger.debug('tar files initialized')
      
      # create output folders
      dir_tas = path.abspath(path.curdir+'/dir_tas/')
      dir_segetalflora = path.abspath(path.curdir+'/dir_segetalflora/')
      dir_fieldmean = path.abspath(path.curdir+'/dir_fieldmean/')
      #dir_tmp = path.abspath(path.curdir+'/dir_tmp/')
      
      mkdir(dir_tas)
      mkdir(dir_segetalflora)
      mkdir(dir_fieldmean)
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

    #calc = [{'func':'mean','name':'tas'}]
    #calc_grouping = ['year']
    
    # read argments to variables
    #try:  
    ncs = self.getInputValues(identifier='netcdf_file')
    climate_type = self.climate_type.getValue()
    culture_type = self.culture_type.getValue()
    
    if type(climate_type) != list:
      climate_type = list([climate_type])
    if type(culture_type) != list:
      culture_type = list([culture_type])
      

      
    logger.debug('urls for %s ncs found' % (len(ncs)))
    logger.debug('culture type: %s ' % (culture_type))
    
    for cult in culture_type: 
      for clim in climate_type: 
        try:
          sf_files =  sf.get_segetalflora(ncs, culture_type=cult, climate_type=clim , countries=countries, dir_tas=dir_tas , dir_segetalflora=dir_segetalflora, dir_fieldmean=dir_fieldmean)
          self.show_status("processing of %s segetalflora files done " % (len(sf_files)) , 80)
        except Exception as e:
          msg = 'segetalflora calculation failed %s %s : %s\n' %( climate_type, culture_type, e) 
          logger.exception(msg)
          #outlog = outlog + msg
  
    # first processing step: masking Europe for each file
    
    #ncs_europe = []
    
    #if type(ncs) != list: 
      #ncs = list([ncs])
    ##outlog = outlog + '%s : %s file(s), type: %s will be processed \n' % ( datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), len(ncs), type(ncs))
      
    #ncs_year_mean = []  
    #dir_temp = mkdtemp()  
    
    #for c, nc in enumerate(ncs):
      #self.show_status('Calculate year mean of tas for  %s/%s:' % (c+1, len(ncs)), 7)
      #output = path.join(dir_temp, path.basename(nc))
      #cdo.yearmean(input = nc , output= output)
      #ncs_year_mean.append(output)
    
    #ncs_merge = []
    #self.show_status('All Yearmean calculated' , 7)
    #ncs_sort = utils.sort_by_filename(ncs_year_mean)
    #for key in ncs_sort:
      #input = str(" ".join(ncs_sort[key]))
      #output = path.join(dir_tas, key+'.nc')
      #cdo.mergetime(input= input, output=output)
      #nc_merge = utils.drs_filename(output, variable = 'tas', rename_file=True)
      #ncs_merge.append(path.join(dir_tas,nc_merge))
      
    self.show_status('files merged' , 7)

    #  logger.error(msg)
    ##
    # segetal flora calculation
    
    #ncs_europe = []
    #for c, nc in enumerate(ncs_merge):
      #for cult in culture_type:
        #for clim in climate_type:
          #try: 
            #basename = path.basename(nc)
            #self.show_status('Segetalflora from %s/%s %s ' %(c+1 , len(ncs_merge), basename), 20)
            #eq = sg.get_equation( culture_type=cult, climate_type=clim)
            #sf_var = 'sf%s%s' % (cult, clim)
            #o1, output = mkstemp(dir=dir_temp, suffix='.nc')
            #cdo.expr(eq , input=nc , output = output)
            #msg = 'CDO expression calculation for %s \n' %( basename) 
            #logger.exception(msg)
            ##outlog = outlog + msg
          #except Exception as e:
            #msg = 'CDO expression calculation failed %s : %s\n' %( nc, e) 
            #logger.exception(msg)
            ##outlog = #outlog + msg
          
          #try:
            #domain = basename.split('_')[1]
            #prefix = basename.strip('.nc').replace('tas_', sf_var+'_')
            #output_mask = sub.masking(output, domain, prefix=prefix, dir_output=dir_polygons)
            #ncs_europe.append(output_mask)
            #cdo.fldmean(input=output_mask , output=path.join(dir_fieldmean, path.basename(output_mask)) )
            #msg = 'CDO fieldmean calculation for %s \n' %( basename) 
            #logger.debug(msg)
            ##outlog = outlog + msg
          #except Exception as e:
            #msg = 'CDO fieldmean calculation failed %s : %s\n' %( nc, e) 
            #logger.exception(msg)
            ##outlog = outlog + msg
          

          #for country in countries:
            #try:
              #sf_country_prefix = prefix.replace('_EUR-', '_%s-' % (country))
              #EUR_seglo = clipping.clip_counties_EUR(urls=output, variable= sf_var,
                                                      #prefix= sf_country_prefix, dir_output = dir_polygons, country=country)
              ##outlog = outlog + '%s : CLIPPING FAILED! for model %s \n' %(datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'), sf_country_prefix)
              
              #msg = 'Clipping done for %s : %s\n' %(sf_country_prefix ) 
              #logger.exception(msg)
              ##outlog = outlog + msg
            #except Exception as e:
              #msg = 'Clipping failed %s %s:\n %s\n' %(country, path.basename, e) 
              #logger.exception(msg)
              ##outlog = outlog + msg    

   
    self.show_status('files to tar archives', 75)
    tar_tas.add(dir_tas, arcname = dir_tas.replace(path.abspath(path.curdir), ""))
    tar_segetalflora.add(dir_segetalflora, arcname = dir_segetalflora.replace(path.abspath(path.curdir), ""))
    tar_fieldmeans.add(dir_fieldmean, arcname = dir_fieldmean.replace(path.abspath(path.curdir), ""))
    tar_tas.close()
    tar_fieldmeans.close()
    tar_segetalflora.close()
    logger.debug('tar ncfiles closed')
    
    #outlog = outlog + "%s Finishing the segetalflora calculation \n" % (datetime.strftime(datetime.now(), '%d-%m-%Y %H:%M:%S'))
    
    #l1, logfile = mkstemp(dir=".", suffix='.txt') 
    #with open(logfile, 'w') as fp:
        #fp.write(outlog)
    
   # self.logout.setValue( logfile )
    self.out_fieldmeans.setValue( tarf_fieldmeans )
    self.out_segetalflora.setValue( tarf_segetalflora )
    self.out_tas.setValue( tarf_tas )
    self.show_status("processing done", 100)
