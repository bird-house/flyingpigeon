import logging
logger = logging.getLogger(__name__)


def get_configfile(files, 
                   timewin=1, 
                   varname='slp',
                   seacyc=False,
                   cycsmooth=91, 
                   nanalog=20, 
                   seasonwin=30, 
                   distfun='rms', 
                   outformat='.txt',
                   period=["1973-01-01","2012-12-31"], 
                   bbox="-180.0,-90.0,180,90.0",
                   calccor=True, 
                   silent=False, ): 
  """
  Generating the configuration file for CASTf90 calculation

  :param files: input files (reference period and period for analyses)
  :param timewin: number of days the distance is averaged
  :param varname: variable name in input files
  :param seacyc: remove the smoothed seasonal cycle from the input fields (True/False)
  :param cycsmooth: smoothing window for the seasonal cycle in days (should be an odd integer)
  :param nanalog: Number of analogs to be detect
  :param distfun: Name of the distance function used to calculate the analogs. 
   (Supported values: 'rms' 'mahalanobis', 'S1' (Teweles and wobus), 'cosine' (correlation) 
   and - still experimental - 'of' (displacement and amplitude score based on optical flow image distortion)
  :param outformat: file format for output ('txt' or 'nc' (default))
  :param period: reference period for analogs to be picked (for netcdf output attributes)
  :param bbox: cooridates for the region to be analysed
  :param calccor: calculate rank correlation for analog fields (True/False)
  :param silent: handling of log file output

  :returns: configuration file
  """
  from datetime import datetime as dt
  from os.path import abspath
  from tempfile import mkstemp
  
  date_stamp = dt.strftime(dt.now(), format='%Y%m%d_%H%M%S')
  logger.info('start configuraion file preparation at: %s' %(date_stamp))

  # convert True/False to Fortran syntax
  seacyc=str(seacyc)
  calccor=str(calccor)
  silent=str(silent)

  # write stuff to configuration file
  # NB: if order or format or number changes, need to edit wps_analogs_viewer.py
  # and template_analogviewer.html where these scripts read in the config params
  ip, config_file = mkstemp(dir='.',suffix='.txt')

  config = open(config_file, "w")
  
  config.write('!Configuration file for flyingpigeon analogs process\n')
  config.write('!Created : %s \n' % ( date_stamp ))
  config.write('!Version : 0.1 \n')
  config.write('&FILES \n')
  config.write(' my_files%archivefile = "{file}" \n'.format(file=files[0]) ) 
  config.write(' my_files%simulationfile = "{file}" \n'.format(file=files[1]) )
  config.write(' my_files%outputfile = "{file}" \n'.format(file=files[2]) )
  config.write('/ \n')
  config.write('&PARAM \n')
  config.write(' my_params%timewin = {timewin} \n'.format(timewin=timewin))
  config.write(' my_params%varname = "{varname}" \n'.format(varname=varname))
  config.write(' my_params%seacyc = .{seacyc}. \n'.format(seacyc=seacyc.upper()))
  config.write(' my_params%cycsmooth = {cycsmooth} \n'.format(cycsmooth=cycsmooth))
  config.write(' my_params%nanalog = {nanalog} \n'.format(nanalog=nanalog))
  config.write(' my_params%seasonwin = {seasonwin} \n'.format(seasonwin=seasonwin))
  config.write(' my_params%distfun = "{distfun}" \n'.format(distfun=distfun))
  config.write(' my_params%calccor = .{calccor}. \n'.format(calccor=calccor.upper()))
  config.write(' my_params%oformat = "{outformat}" \n'.format(outformat=outformat)) # ".txt" # ! if equals ".nc"
  config.write(' my_params%silent = .{silent}.\n'.format(silent=silent.upper()))
  config.write('/\n')
  config.write('&ATTS\n')
  config.write(' my_atts%simsource = "NCEP" \n') # model name
  config.write(' my_atts%predictorvar = "{varname}" \n'.format(varname=varname))
  config.write(' my_atts%archisource = "NCEP" \n')
  config.write(' my_atts%archiperiod = "{start},{end}" \n'.format(start=period[0], end=period[1]))
  config.write(' my_atts%predictordom = "{bbox}" \n'.format(bbox=bbox))
  config.write('/\n')
  
  config.close()
  return abspath(config_file)

def subset(resource=[], bbox='-80,50,22.5,70'):
  """
  returns a subset

  :param resource: netCDF input files of one dataset
  :param bbox: bounding box

  :return: subset netCDF file 
  """
  from tempfile import mkstemp
  from cdo import Cdo 
  cdo = Cdo()
  resource.sort()

  ip, nc_concat = mkstemp(dir='.',suffix='.nc')
  nc_concat = cdo.cat(input=resource, output=nc_concat)

  ip, nc_subset = mkstemp(dir='.',suffix='.nc')
  nc_subset = cdo.sellonlatbox('%s' % bbox, input=nc_concat, output=nc_subset)
  logger.info('subset done: %s ' % nc_subset)
  
  return nc_subset

def seacyc(archive, simulation, method='base'):
  """
  substracts the seasonal cycle

  :param archive: netCDF file containing the reference period
  :param simulation: netCDF file containg the period to be analysed
  :param method: method to generat the seasonal cycle files
                 base = seasonal cycle generated from reference period
                 sim = seasonal cycle generated from period to be analysed
                 own = seasonal cycle generated for both time windows

  :returns: two netCDF files for analyse and reference period     
  """
  from shutil import copy
  from cdo import Cdo 
  cdo = Cdo()

  if method == 'base':
    seasoncyc_base = cdo.ydaymean(input=archive, output='seasoncyc_base.nc' )
    seasoncyc_sim = 'seasoncyc_sim.nc'
    copy(seasoncyc_base, seasoncyc_sim)
  if method == 'sim':
    seasoncyc_sim  = cdo.ydaymean(input=simulation, output='seasoncyc_sim.nc' )
    seasoncyc_base = 'seasoncyc_base.nc'
    copy(seasoncyc_sim, seasoncyc_base)
  if method == 'own':
    seasoncyc_base = cdo.ydaymean(input=archive, output='seasoncyc_base.nc' )
    seasoncyc_sim  = cdo.ydaymean(input=simulation, output='seasoncyc_sim.nc' )
  return seasoncyc_base, seasoncyc_sim