from flyingpigeon import utils

import logging
logger = logging.getLogger(__name__)

def get_pca(resource):
  """
  calculation of principal components

  :param resource: netCDF file containing pressure values for a defined region and selected timesteps
  :returns data, pca: normalised data , sklean objct
  """
  from netCDF4 import Dataset, num2date
  from flyingpigeon.utils import get_variable
  from numpy import mean

  var = get_variable(resource)
  ds = Dataset(resource)
  vals = ds.variables[var]

  lat = ds.variables['lat']
  lon = ds.variables['lon']
  #time = ds.variables['time']
  
  # make array of seasons:
  # convert netCDF timesteps to datetime
  #timestamps = num2date(time[:], time.units, time.calendar)
  #season = [get_season(s) for s in timestamps]
  
  from sklearn.decomposition import PCA
  import numpy as np
  
  # reshape
  #data = np.array(vals)
  #m = mean(vals)
  #mdata = vals - m 
  adata = vals[:].reshape(vals[:].shape[0], (vals[:].shape[1] * vals[:].shape[2]) )
  pca = PCA(n_components=50).fit_transform(adata)
  return pca #, season

def calc_tSNE(pca):
  """
  perform a cluster analysis with tSNE method
  
  """
  from sklearn.manifold import TSNE
  #X_all, y_all = get_pca(resource)
  data = TSNE(n_components=2, perplexity=40, verbose=2).fit_transform(pca)
  
  return data

def calc_kMEAN(pca):
  """
  perform a cluster analysis with kMean method
  :param pca: principal components
  :return centroids, distance, regime: centroids of all timesepps, the distances to the cluster center weather regime of timestep
  """
  from sklearn import cluster
  kmeans = cluster.KMeans(n_clusters=4)
  #cluster.KMeans(n_clusters=4, init='k-means++', n_init=10, max_iter=300, tol=0.0001, precompute_distances='auto', verbose=0, random_state=None, copy_x=True, n_jobs=1)
  
  centroids = kmeans.fit(pca)
  distance = kmeans.fit_transform(pca)
  regime = distance.argsort()[:,3]
  
  return centroids, distance, regime

def get_NCEP( start=1948, end=None ):
  """
  fetching the NCEP slp data to local file system
  :param start: int for start year to fetch source data
  :param end: int for end year to fetch source data (if None, current year will be the end)
  :return list: list of path/files.nc 
  """
  try:
    from datetime import datetime as dt

    if end == None: 
      end = dt.now().year
    ncep_data = []

    for year in range(start, end + 1):
      try:
        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.%s.nc' % year
        ncep_data.append(utils.download(url, cache=True))
      except:
        msg = "wget failed on {0}.".format(url)
        logger.exception(msg)
        raise Exception(msg)
    logger.info('NCEP data fetched for %s files' % len(ncep_data))  
  except:
    msg = "get_NCEP module failed"
    logger.exception(msg)
    raise Exception(msg)
  return ncep_data

def weight_by_lat(resource):
  """
  weight values according to their latitude

  :param resource: netCDF file containing sea searface pressure
  :returns : netCDF file with weighted values
  """

  from netCDF4 import Dataset
  from numpy import meshgrid, sqrt, cos, pi, broadcast_arrays

  variable = utils.get_variable(resource)

  ds = Dataset(resource, mode='a')
  lat = ds.variables['lat']
  lon = ds.variables['lon']
  lons, lats = meshgrid(lon, lat)

  vals = ds.variables[variable]

  weights = 1/sqrt(cos(lats[:]*pi/180))
  weights3D = broadcast_arrays(vals,weights)[1]

  vals_weighted = vals[:] * weights3D
  vals[:,:,:] = vals_weighted[:,:,:]
  
  ds.close()

  return resource

def score_pattern(pattern1, pattern2):
  """
  compares two 2D fields in terms of similarity

  :param pattern1: observation pattern
  :param pattern2: model pattern
  :return score: number 0-1 (0 low, 1 high correlation)
  """
  from sklearn import linear_model
  from numpy import mean

  if len(pattern1.shape) == 3:
    pattern1 = mean(pattern1, axis=0)
  if len(pattern2.shape) == 3:
    pattern2 = mean(pattern2, axis=0)

  if pattern1.shape != pattern2.shape:
    logger.error('score_pattern failed: pattern are in differnet shape')
  
  regr = linear_model.LinearRegression()
  regr.fit(pattern1, pattern2)
  score = regr.score(pattern1, pattern2)
  return score

def subset(resource=[], bbox="-80,50,22.5,70",  time_region=None, time_range=None, variable=None, regrid_destination=None):
  """
  extracts the time regions (e.g. '12,1,2') and bounding box from a dataset

  :param resource: List of files belonging to one dataset
  :param bbox: geographical region lat/lon
  :param time_region: month to be picked from the data
  :param time_range: defines the start and end time to be taken into accont (time_range=[datetime(start), datetime(end)])
  :returns netCDF: file containing containing lat weighted and normalize (by anual cycle) data
  """
  from flyingpigeon.ocgis_module import call
  from tempfile import mkstemp
  import uuid

  if type(resource) == str: 
    resource = list([resource])

  if variable == None:
    variable = utils.get_variable(resource[0])

  if not time_region == None:
    month = map(int, time_region.split(','))
    time_region = {'month':month}

  nc_grouped = call(resource=resource,  
    conform_units_to='hPa',
    #geom= [float(n) for n in bbox.split(',')], # not possible due to wrapping difficulties in ocgis
    #prefix=str(uuid.uuid1()),
    regrid_destination=regrid_destination,
    regrid_options='bil',
    time_region=time_region,
    time_range=time_range,
    variable=variable)
  
  from cdo import Cdo
  cdo = Cdo()

  ip, nc_subset = mkstemp(dir='.',suffix='.nc')
  nc_subset  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output=nc_subset)
  logger.info('subset done: %s ' % nc_subset)
  
  # nc_subset = nc_grouped
  nc_weighted = weight_by_lat(nc_subset)
  
  ip , nc_anual_cycle = mkstemp(dir='.',suffix='.nc')
  ip , nc_normalized = mkstemp(dir='.',suffix='.nc')

  nc_anual_cycle = cdo.ydaymean(input= nc_weighted , output=nc_anual_cycle) 
  nc_normalized = cdo.sub(input=[nc_weighted, nc_anual_cycle], output= nc_normalized )
  logger.info('weightning and normalisation done: %s ' % nc_subset)
  
  return nc_normalized


def subset_model(resource=[], bbox="-80,50,22.5,70",  time_region=None, variable=None, regrid_destination=None):
  """
  extracts the time regions (e.g. '12,1,2') and bounding box from a dataset
  
  Temporary module will be deleted soon

  :param resource: List of files belonging to one dataset
  :param bbox: geographical region lat/lon
  :param time_region: month to be picked from the data
  :returns netCDF: file containing containing lat weighted and normalize (by anual cycle) data
  """
  from flyingpigeon.ocgis_module import call
  from tempfile import mkstemp
  import uuid
  from shutil import rmtree

  if type(resource) == str: 
    resource = list([resource])

  if variable == None:
    variable = utils.get_variable(resource[0])

  if not time_region == None:
    month = map(int, time_region.split(','))
    time_region = {'month':month}

  from cdo import Cdo
  cdo = Cdo()

  ip, nc_grouped = mkstemp(dir='.',suffix='.nc')

  nc_grouped = cdo.cat(input=resource, output=nc_grouped)
  logger.info('concatination done: %s ' % nc_grouped)

  ip, nc_remap = mkstemp(dir='.',suffix='.nc')
  nc_remap  = cdo.remapbil('%s' % regrid_destination, input=nc_grouped, output=nc_remap)
  logger.info('remapbil done: %s ' % nc_remap)
  
  rmtree(nc_grouped, ignore_errors=True, onerror=None)
  logger.info('concatination removed')
  
  nc_unitconvert = call(resource=nc_remap,  
    conform_units_to='hPa',
    #geom= [float(n) for n in bbox.split(',')], # not possible due to wrapping difficulties in ocgis
    #prefix=str(uuid.uuid1()),
    #regrid_destination=regrid_destination,
    #regrid_options='bil',
    time_region=time_region,
    variable=variable)
  
  # nc_subset = nc_grouped
  nc_weighted = weight_by_lat(nc_unitconvert)
  
  ip , nc_anual_cycle = mkstemp(dir='.',suffix='.nc')
  ip , nc_normalized = mkstemp(dir='.',suffix='.nc')

  nc_anual_cycle = cdo.ydaymean(input= nc_weighted , output=nc_anual_cycle) 
  nc_normalized = cdo.sub(input=[nc_weighted, nc_anual_cycle], output= nc_normalized )
  logger.info('weightning and normalisation done: %s ' % nc_normalized)
  
  return nc_normalized
