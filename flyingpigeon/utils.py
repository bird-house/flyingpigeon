import urlparse
import os
import wget
from ocgis import RequestDataset
from netCDF4 import Dataset, MFDataset, num2date
from flyingpigeon import config

import logging
logger = logging.getLogger(__name__)

GROUPING = [ "day", "mon", "sem", "yr", "ONDJFM", "AMJJAS", "DJF", "MAM", "JJA", "SON" ,
            "Jan", 'Feb', "Mar", "Apr", "May", "Jun", 'Jul', "Aug", 'Sep', 'Okt', 'Nov', 'Dec' ]

def download(url, cache=False):
    """
    Downloads URL using the Python wget module to the current directory.
    :param cache: if True then files will be downloaded to a cache directory.
    """
    if cache:
        parsed_url = urlparse.urlparse(url)
        filename = os.path.join(config.cache_path(), parsed_url.netloc, parsed_url.path.strip('/'))
        if os.path.exists(filename):
            logger.debug('file already in cache: %s', filename)
        else:
            if not os.path.exists(os.path.dirname(filename)):
                os.makedirs(os.path.dirname(filename))
            logger.info('downloading: %s', url)
            filename = wget.download(url, out=filename, bar=None)
        # make softlink to current dir
        #os.symlink(filename, os.path.basename(filename))
        #filename = os.path.basename(filename)
    else:
        filename = wget.download(url, bar=None)
    return filename

def archive(resources, format='tar', dir_output='.', mode='w'): 
  """
  compressing a list of files into an archive

  :param resources: list of files to be stored in archive
  :param format: archive format. options: tar(default), zip
  :param dir_output: path to output folder (default current direcory)
  :param mode:    for format='tar':
                  'w' or 'w:'  open for writing without compression
                  'w:gz'       open for writing with gzip compression
                  'w:bz2'      open for writing with bzip2 compression
                  'w|'         open an uncompressed stream for writing
                  'w|gz'       open a gzip compressed stream for writing
                  'w|bz2'      open a bzip2 compressed stream for writing
                  
                  for foramt='zip':
                  read "r", write "w" or append "a"
  :return archive: archive path/filname.ext
  """
  from tempfile import mkstemp
  from os.path import basename
  
  logger.info('compressing files to archive')

  if type(resources) == str: 
    resources = list([resources])

  resources_filter =  [x for x in resources if x is not None]
  resources =  resources_filter

  if format == 'tar':
    import tarfile
    try: 
      o1 , archive = mkstemp(dir=dir_output, suffix='.tar')
      tar = tarfile.open(archive, mode)

      for f in resources:
        try: 
          tar.add(f, arcname = basename(f))
        except:
          msg = 'archiving failed for %s' % f
          logger.debug( msg )
          raise Exception
      tar.close()
    except:
      msg = 'failed to compress into archive'
      logger.exception(msg)
      raise Exception(msg)
  elif format == 'zip': 
    import zipfile

    logger.info('creating zip archive')
    try :
      o1 , archive = mkstemp(dir=dir_output, suffix='.zip')
      zf = zipfile.ZipFile(archive, mode=mode)
      try:
          for f in resources: 
            zf.write(f, basename(f))
          zf.close()
      except Exception as e:
        msg = 'failed to create zip archive: %s' % msg
        logger.debug(msg)
        raise
      #logger.info(print_info('zipfile_write.zip'))
    except Exception as e: 
      msg = 'failed to create zip archive'
      logger.debug(msg)
      raise
  else: 
    logger.error('no common archive format like: zip / tar')
  return archive

def local_path(url):
  from urllib2 import urlparse
  url_parts = urlparse.urlparse(url)
  return url_parts.path

def calc_grouping(grouping):
  """
  translate time grouping abreviation (e.g 'JJA') into the apprpriate ocgis calc_grouping syntax

  :param grouping: time  group abreviation allowed values: "yr", "mon", "sem", "ONDJFM", "AMJJAS", "DJF", "MAM", "JJA", "SON"
  :return: calc_grouping
  """
  calc_grouping = ['year'] # default year
  if grouping == 'yr':
      calc_grouping = ['year']
  elif grouping == 'sem':
      calc_grouping = [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] 
  elif grouping == 'ONDJFM':
      calc_grouping = [ [10,11,12,1,2,3], 'unique'] 
  elif grouping == 'AMJJAS':
      calc_grouping = [ [4,5,6,7,8,9], 'unique'] 
  elif grouping == 'DJF':
      calc_grouping = [[12,1,2], 'unique']    
  elif grouping == 'MAM':
      calc_grouping = [[3,4,5], 'unique']    
  elif grouping == 'JJA':
      calc_grouping = [[6,7,8], 'unique']    
  elif grouping == 'SON':
      calc_grouping = [[9,10,11], 'unique']
  elif grouping == 'day':
    calc_grouping = ['year', 'month', 'day']
  elif grouping == 'mon':
      calc_grouping = ['year', 'month'] 
  elif grouping == 'Jan':
      calc_grouping = [[1], 'unique']
  elif grouping == 'Feb':
      calc_grouping = [[2], 'unique']
  elif grouping == 'Mar':
      calc_grouping = [[3], 'unique']
  elif grouping == 'Apr':
      calc_grouping = [[4], 'unique']
  elif grouping == 'May':
      calc_grouping = [[5], 'unique']
  elif grouping == 'Jun':
      calc_grouping = [[6], 'unique']
  elif grouping == 'Jul':
      calc_grouping = [[7], 'unique']
  elif grouping == 'Aug':
      calc_grouping = [[8], 'unique']
  elif grouping == 'Sep':
      calc_grouping = [[9], 'unique']
  elif grouping == 'Oct':
      calc_grouping = [[10], 'unique']
  elif grouping == 'Nov':
      calc_grouping = [[11], 'unique']
  elif grouping == 'Dec':
      calc_grouping = [[12], 'unique']
  elif grouping in ['year', 'month']:
      calc_grouping = [grouping]
  else:
      msg = 'Unknown calculation grouping: %s' % grouping
      logger.debug(msg)
      raise Exception(msg)
  return calc_grouping

def drs_filename(nc_file, skip_timestamp=False, skip_format=False , 
                 variable=None, rename_file=False, add_file_path=False ):
    """
    generates filename according to the data reference syntax (DRS) 
    based on the metadata in the nc_file.
    
    http://cmip-pcmdi.llnl.gov/cmip5/docs/cmip5_data_reference_syntax.pdf
    https://pypi.python.org/pypi/drslib

    :param add_file_path: if add_file_path=True, path to file will be added (default=False)
    :param nc_file: netcdf file
    :param skip_timestamp: if True then from/to timestamp is not added to the filename
                           (default: False)
    :param variable: apprpriate variable for filename, if not set (default), variable will 
                      be determinated. for files with more than one data variables 
                      the variable parameter has to be defined (default: )
                      example: variable='tas'
    :param rename_file: rename the file. (default: False)                   
    :return: DRS filename
    """
    from os import path, rename
    
    ds = Dataset(nc_file)
    if variable == None: 
      variable = get_variable(nc_file)

    # CORDEX example: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    cordex_pattern = "{variable}_{domain}_{driving_model}_{experiment}_{ensemble}_{model}_{version}_{frequency}"
    # CMIP5 example: tas_MPI-ESM-LR_historical_r1i1p1
    cmip5_pattern = "{variable}_{model}_{experiment}_{ensemble}"

    filename = nc_file
    try:
        if ds.project_id == 'CORDEX' or ds.project_id == 'EOBS' :
            filename = cordex_pattern.format(
                variable = variable,
                domain = ds.CORDEX_domain,
                driving_model = ds.driving_model_id,
                experiment = ds.experiment_id,
                ensemble = ds.driving_model_ensemble_member,
                model = ds.model_id,
                version = ds.rcm_version_id,
                frequency = ds.frequency)
        elif ds.project_id == 'CMIP5':
            # TODO: attributes missing in netcdf file for name generation?
            filename = cmip5_pattern.format(
                variable = variable,
                model = ds.model_id,
                experiment = ds.experiment,
                ensemble = ds.parent_experiment_rip
                )
        else:
            raise Exception('unknown project %s' % ds.project_id)
        ds.close()

        # add from/to timestamp if not skipped
        if skip_timestamp == False:
            from_timestamp, to_timestamp = get_timerange(nc_file)
            filename = "%s_%s-%s" % (filename, int(from_timestamp), int(to_timestamp))

        # add format extension
        if skip_format == False:
            filename = filename + '.nc'
        
        pf = path.dirname(nc_file)
        # add file path 
        if add_file_path == True:
          filename = path.join( pf , filename )
        
        # rename the file
        if rename_file==True:
          if path.exists(path.join(nc_file)):
            rename(nc_file, path.join(pf, filename ))      
    except:
        logger.exception('Could not generate DRS filename for %s', nc_file)
    
    return filename

def get_variable(nc_file):
    """
    returns the variable name (str)
    :param nc_file: NetCDF file
    """
    rd = RequestDataset(nc_file)
    
    return rd.variable

def get_coordinates(nc_file): 
  """
  returns the values of latitude and longitude
  :param nc_file: netCDF resource file
  :returns lats, lons: netCDF4 data objectes
  """
  lats = None 
  lons = None
  try:
    ds = Dataset(nc_file)
    lats = ds.variables['lat']
    lons = ds.variables['lon']
  except:
    msg = 'failed to extract coordinates'
    logger.debug(msg)
    raise Exception(msg)
  
  return lats, lons
  

def get_domain(nc_file):
  """
  returns the domain
  :param nc_file: NetCDF file
  :return: domain
  """
  ds = Dataset(nc_file)

  try:
    if 'CMIP' in ds.project_id or 'EUCLEIA' in ds.project_id :
      domain = None
      logger.debug('nc_file belongs to an global experiment project')
    elif 'CORDEX' in ds.project_id: 
      domain = ds.CORDEX_domain
      logger.info('nc_file belongs to CORDEX')
    else: 
      logger.debug('No known project_id found in meta data')

  except Exception as e :
      logger.debug('Could not specify domain for %s: %s' % (nc_file, e) )

  return domain

def get_frequency(nc_file):
  """
  returns the frequency (see also metadata.get_frequency)
  :param nc_file: NetCDF file
  :return: frequency
  """
  ds = MFDataset(nc_file)

  try:
    frequency = ds.frequency
    logger.info('frequency written in the meta data:  %s ' % (frequency))
  except Exception as e :
      logger.exception('Could not specify frequency for %s' % nc_file)
      raise
  else:
    ds.close()
    return frequency

def get_values(nc_files, variable=None):
  """
  returns the values for a list of files of files belonging to one Dataset
  :param nc_files: list of files
  :param variable: variable to be picked from the files (if not set, variable will be detected)
  """
  
 # from netCDF4 import MFDataset
  from numpy import squeeze
  if variable == None:
    if type(nc_files) == str:
      variable = get_variable(nc_files)
    else:
      variable = get_variable(nc_files[0])
      
  mds = MFDataset(nc_files)
  vals = squeeze(mds.variables[variable][:])
  
  return vals

def get_timerange(resources):
  """
  returns from/to timestamp of given netcdf file(s).
  :param resources: path to netCDF file(s)
  :returns netcdf.dateime.dateime: start, end
  """
  try:  
    ds = MFDataset(resources)
    time = ds.variables['time']
    if (hasattr(time , 'units') and hasattr(time , 'calendar')) == True:
      s = num2date(time[0], time.units , time.calendar)
      e = num2date(time[-1], time.units , time.calendar)
    elif hasattr(time , 'units'):
      s = num2date(time[0], time.units , time.calendar)
      e = num2date(time[-1], time.units , time.calendar)
    else: 
      s = num2date(time[0], time.units , time.calendar)
      e = num2date(time[-1], time.units , time.calendar)
    # to do: include frequency
    start = '%s%s%s'  % (s.year, str(s.month).zfill(2) ,str(s.day).zfill(2)) 
    end = '%s%s%s'  %   (e.year,  str(e.month).zfill(2) ,str(e.day).zfill(2))
    ds.close()
  except Exception as e: 
    msg = 'failed to get time: %s' % e
    logger.debug(msg)
    raise Exception(msg)
  return start, end

def get_timestamps(nc_file):
    """
    replaced by get_timerange
    """
    try: 
        start = get_time(nc_file)[0]
        end = get_time(nc_file)[-1]
        
        from_timestamp = '%s%s%s'  % (start.year, str(start.month).zfill(2) ,str(start.day).zfill(2)) 
        to_timestamp = '%s%s%s'  %   (end.year,  str(end.month).zfill(2) ,str(end.day).zfill(2))
    except Exception as e: 
      #logger.debug('failed to get_timestamps %s't % e)
      raise Exception
     
    return (from_timestamp, to_timestamp)

def get_time(nc_file):
    """
    returns all timestamps of given netcdf file as datetime list.
    
    :param nc_file: NetCDF file(s)
    :return format: netcdftime._datetime.datetime
    """
    try:  
      ds = MFDataset(nc_file)
      time = ds.variables['time']
      if (hasattr(time , 'units') and hasattr(time , 'calendar')) == True:
        timestamps = num2date(time[:], time.units , time.calendar)
      elif hasattr(time , 'units'):
        timestamps = num2date(time[:], time.units) 
      else: 
        timestamps = num2date(time[:])
      ds.close()
    except: 
      logger.debug('failed to get time')
      raise Exception
    return timestamps
    
def aggregations(nc_files):
    """
    aggregates netcdf files by experiment. Aggregation examples:
    
    CORDEX: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    CMIP5:
    We collect for each experiment all files on the time axis:
    200101-200512, 200601-201012, ...

    Time axis is sorted by time.

    :param nc_files: list of netcdf files
    :return: dictonary with key=experiment
    """
    
    aggregations = {}
    for nc_file in nc_files:
        key = drs_filename(nc_file, skip_timestamp=True, skip_format=True)

        # collect files of each aggregation (time axis)
        if aggregations.has_key(key):
            aggregations[key]['files'].append(nc_file)
        else:
            aggregations[key] = dict(files=[nc_file])

    # collect aggregation metadata
    for key in aggregations.keys():
        # sort files by time
        aggregations[key]['files'] = sort_by_time(aggregations[key]['files'])
        # start timestamp of first file
        start, end = get_timerange(aggregations[key]['files'])
        # end timestamp of last file
        #_, end = get_timestamps(aggregations[key]['files'][-1])
        aggregations[key]['from_timestamp'] = start
        aggregations[key]['to_timestamp'] = end
        aggregations[key]['start_year'] = int(start[0:4])
        aggregations[key]['end_year'] = int(end[0:4])
        aggregations[key]['variable'] = get_variable(aggregations[key]['files'][0])
        aggregations[key]['filename'] = "%s_%s-%s.nc" % (key, start, end)
    return aggregations

def sort_by_time(resource):
    from ocgis.util.helpers import get_sorted_uris_by_time_dimension
    
    if type(resource) is list and len(resource) > 1:
        sorted_list = get_sorted_uris_by_time_dimension(resource)
    elif type(resource) is str:
        sorted_list = [resource]
    else: 
        sorted_list = resource
    return sorted_list

def sort_by_filename(resource, historical_concatination = False):
  """ Sort a list of files with Cordex conform file names. 
  returns a dictionary with name:list_of_sorted_files"""
  from os  import path
  
  logger.info('sort_by_filename module start sorting %s files' % len(resource))

  ndic = {}
  tmp_dic = {}
  try: 
    if type(resource) == list:
      #logger.debug('resource is list with %s files' % len(resource))  
      try:  #if len(resource) > 1:
        # collect the different experiment names
        for nc in resource:
          logger.info('file: %s' % nc)
          p, f = path.split(path.abspath(nc)) 
          n = f.split('_')
          bn = '_'.join(n[0:-1]) # skipping the date information in the filename
          ndic[bn] = [] # dictionary containing all datasets names
        logger.info('found %s datasets' % len(ndic.keys()))
      except Exception as e: 
        logger.debug('failed to find names of datasets: %s ' % e)
      
      logger.info('check for historical / rcp datasets')
      try:
        if historical_concatination == True:
          # select only necessary names
          if any("_rcp" in s for s in ndic.keys()): 
            for key in ndic.keys(): 
              if 'historical' in key: 
                ndic.pop(key)
            logger.info('historical data set names removed from dictionary')
          else: 
            logger.info('no rcp dataset names found in dictionary')
      except Exception as e:
        logger.debug('failed to pop historical data set names: %s ' % e )  
      
      logger.info('start sorting the files')
      try:  
        for key in ndic:
          try:
            if historical_concatination == False:
              for n in resource:
                if '%s_' % key in n: 
                  ndic[key].append(path.join(p,n))

            elif historical_concatination == True:
              key_hist = key.replace('rcp26','historical').replace('rcp45','historical').replace('rcp65','historical').replace('rcp85','historical')
              for n in resource:
                if '%s_' % key in n or '%s_' % key_hist in n: 
                  ndic[key].append(path.join(p,n))
            else:
              logger.debug('append filespathes to dictionary for key %s failed' % (key))
            ndic[key].sort()
          except Exception as e: 
            logger.debug('failed for %s : %s' % (key, e))  
      except Exception as e:
        logger.debug('failed to populate the dictionary with approriate files: %s' % e) 

      try:
        # add date information to the key:
        for key in ndic.keys(): 
          ndic[key].sort()
          start, end = get_timerange(ndic[key])
          newkey = key+'_'+start+'-'+end
          tmp_dic[newkey] = ndic[key]
      except Exception as e: 
        logger.debug('failed to sort the list of resources and add dates to keyname: %s' % e)
        raise

    elif type(resource) == str:
      p, f = path.split(path.abspath(resource))
      tmp_dic[f.replace('.nc','')] = resource
    else:      
      logger.debug('sort_by_filename module failed: resource is not str or list')
    logger.info('sort_by_filename module done: %s Datasets found' % len(ndic))  
  except Exception as e: 
    logger.debug('failed to sort files by filename')
    raise

  return tmp_dic # rndic

def has_variable(resource, variable):
    success = False
    try:
        rd = RequestDataset(uri=resource)
        success = rd.variable == variable
    except:
        logger.exception('has_variable failed.')
        raise
    return success

def filename_creator(nc_files, var=None):
  """ use drs_filename instead """
  
  from os import path , rename
  from netCDF4 import Dataset
  from datetime import datetime, timedelta
    
  if type(nc_files) != list:
    nc_files = list([nc_files])
  newnames = []
  for i, nc in enumerate(nc_files):
    fp ,fn = path.split(nc)
    # logger.debug('fn_creator for: %s' % fn)
    
    ds = Dataset(nc)
    rd = []
    rd = RequestDataset(nc)
    ts = ds.variables['time']
    reftime = reftime = datetime.strptime('1949-12-01', '%Y-%m-%d')
    st = datetime.strftime(reftime + timedelta(days=ts[0]), '%Y%m%d') 
    en = datetime.strftime(reftime + timedelta(days=ts[-1]), '%Y%m%d') 
    
    if var == None:
      var = str(rd.variable)
    frq = str(ds.frequency)
    exp = str(ds.experiment_id)
    
    if (str(ds.project_id) == 'CMIP5'):
    #day_MPI-ESM-LR_historical_r1i1p1
      gmodel = str(ds.model_id)
      ens = str(ds.parent_experiment_rip)
      filename = var + '_' + str( gmodel + '_' + exp + '_' + ens + '_' + str(int(st)) + '-' + str(int(en)) + '.nc')  
    elif (str(ds.project_id) == 'CORDEX'):
    #EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
      dom = str(ds.CORDEX_domain)
      gmodel = str(ds.driving_model_id)
      ens = str(ds.driving_model_ensemble_member)
      rmodel = str(ds.model_id)
      ver = str(ds.rcm_version_id)
      filename = str(var + '_'+ dom + '_' + gmodel + '_' + exp + '_' + ens + '_' + rmodel + '_' + ver + \
        '_' + frq + '_' + str(int(st)) + '-' + str(int(en)) + '.nc' )
    else:
      filename = fn 
      logger.debug('WPS name forwarded :%s' % ( filename))
    
    rename(path.join(fp ,fn), path.join(fp, filename ))
    if path.exists(path.join(fp, filename)):
      newnames.append(path.join(fp, filename))
    logger.debug('file name generated and renamed :%s' % (len(filename)))
  return newnames


def get_dimension_map(resource): 
  """ returns the dimension map for a file, required for ocgis processing. 
  file must have a DRS conform filename (see: utils.drs_filename())
  
  :param resource: str input file path
  """
  from os.path import basename
  file_name = basename(resource)
  
  dim_map1 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0}}
  
  dim_map2 = {'X': {'variable': 'lon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'lat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0, 'bounds': 'time_bnds'}}
  
  dim_map3 = {'X': {'variable': 'rlon', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'rlat', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  dim_map4 = {'X': {'variable': 'x', 'dimension': 'x', 'pos': 2},
              'Y': {'variable': 'y', 'dimension': 'y', 'pos': 1},
              'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}
  
  
  dim_map5  = {'X': {'variable': 'Actual_longitude', 'dimension': 'x', 'pos': 2},
                 'Y': {'variable': ' Actual_latitude', 'dimension': 'y', 'pos': 1},
                 'T': {'variable': 'time', 'dimension': 'time', 'pos': 0 }}

  
  if 'CM5A-MR_WRF331F' in file_name: 
    dimension_map = dim_map1
  elif 'CNRM-CM5_CNRM-ALADIN53' in file_name: 
    dimension_map = dim_map1
  elif 'MPI-ESM-LR_REMO019' in file_name: 
    dimension_map = dim_map1
  elif 'CLMcom-CCLM4-8-17' in file_name:
    dimension_map = dim_map1
  elif '_EOBS_observation_' in file_name:
    dimension_map = dim_map5  
    
    
  #elif 'KNMI-RACMO22E' in file_name:   
    #dimension_map = dim_map1
  else:     
    dimension_map = None
  return dimension_map

def unroate_pole(resource, write_to_file=True): 
  from numpy import reshape, repeat
  from iris.analysis import cartography  as ct
  
  ds = Dataset(resource, mode='a')
  
  try:
    if 'rotated_latitude_longitude' in ds.variables:
      rp = ds.variables['rotated_latitude_longitude']
    elif 'rotated_pole' in ds.variables:   
      rp = ds.variables['rotated_pole']
    else: 
      logger.debug('rotated pole variable not found')
    pole_lat = rp.grid_north_pole_latitude
    pole_lon = rp.grid_north_pole_longitude
  except Exception as e: 
    logger.debug('failed to find rotated_pole coorinates: %s' % e)
  try:   
    if 'rlat' in ds.variables: 
      rlats = ds.variables['rlat']
      rlons = ds.variables['rlon']
      
    if 'x' in ds.variables:
      rlats = ds.variables['y']
      rlons = ds.variables['x'] 
  except Exception as e: 
    logger.debug('failed to read in rotated coordiates %s '% e)
    
  try:   
    rlons_i = reshape(rlons,(1,len(rlons)))
    rlats_i = reshape(rlats,(len(rlats),1))
    grid_rlats = repeat(rlats_i, (len(rlons)), axis=1)
    grid_rlons = repeat(rlons_i, (len(rlats)), axis=0)
  except Exception as e: 
    logger.debug('failed to repeat coordinates %s'% e)
  
  lons, lats = ct.unrotate_pole(grid_rlons, grid_rlats, pole_lon, pole_lat)
  
  if write_to_file == True: 
    lat = ds.createVariable('lat','f8',('rlat','rlon'))
    lon = ds.createVariable('lon','f8',('rlat','rlon'))    
    
    lon.standard_name = "longitude" ;
    lon.long_name = "longitude coordinate" ;
    lon.units = 'degrees_east'
    lat.standard_name = "latitude" ;
    lat.long_name = "latitude coordinate" ;
    lat.units = 'degrees_north'
    
    lat[:] = lats
    lon[:] = lons
  
  ds.close()
  return lats, lons

class FreeMemory(object):
  """
  Non-cross platform way to get free memory on Linux. Note that this code 
  uses the key word as, which is conditionally Python 2.5 compatible!
  If for some reason you still have Python 2.5 on your system add in the head
  of your code, before all imports:
  from __future__ import with_statement
  """

  def __init__(self, unit='kB'):

      with open('/proc/meminfo', 'r') as mem:
          lines = mem.readlines()

      self._tot = int(lines[0].split()[1])
      self._free = int(lines[1].split()[1])
      self._buff = int(lines[2].split()[1])
      self._cached = int(lines[3].split()[1])
      self._shared = int(lines[20].split()[1])
      self._swapt = int(lines[14].split()[1])
      self._swapf = int(lines[15].split()[1])
      self._swapu = self._swapt - self._swapf

      self.unit = unit
      self._convert = self._faktor()

  def _faktor(self):
      """determine the convertion factor"""
      if self.unit == 'kB':
          return 1
      if self.unit == 'k':
          return 1024.0
      if self.unit == 'MB':
          return 1/1024.0
      if self.unit == 'GB':
          return 1/1024.0/1024.0
      if self.unit == '%':
          return 1.0/self._tot
      else:
          raise Exception("Unit not understood")

  @property
  def total(self):
      return self._convert * self._tot

  @property
  def used(self):
      return self._convert * (self._tot - self._free)

  @property
  def used_real(self):
      """memory used which is not cache or buffers"""
      return self._convert * (self._tot - self._free - self._buff - self._cached)

  @property
  def shared(self):
      return self._convert * (self._tot - self._free)

  @property
  def buffers(self):
      return self._convert * (self._buff)

  @property
  def cached(self):
      return self._convert * self._cached

  @property
  def user_free(self):
      """This is the free memory available for the user"""
      return self._convert *(self._free + self._buff + self._cached)

  @property
  def swap(self):
      return self._convert * self._swapt

  @property
  def swap_free(self):
      return self._convert * self._swapf

  @property
  def swap_used(self):
      return self._convert * self._swapu 
