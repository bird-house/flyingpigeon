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
  print 'variable name: %s' % var
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
  :return distance, centroid: distances and appropriate nearest centroid (weather regime of timestep)
  """
  
  from sklearn import cluster
  import numpy as np
  from tempfile import mkstemp

  kmeans = cluster.KMeans(n_clusters=4)
  #cluster.KMeans(n_clusters=4, init='k-means++', n_init=10, max_iter=300, tol=0.0001, precompute_distances='auto', verbose=0, random_state=None, copy_x=True, n_jobs=1)
  
  centroids = kmeans.fit(pca)
  
  distance = kmeans.fit_transform(pca)
  regime = distance.argsort()[:,3]
  
  return centroids, distance, regime

def get_NCEP():
  """
  fetching the NCEP slp data to local file system
  :return list: list of path/files.nc 
  """
  try:
    from datetime import datetime as dt

    cur_year = dt.now().year
    ncep_data = []

    for year in range(1948, cur_year + 1):
      try:
        url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.%s.nc' % year
        ncep_data.append(utils.download(url, cache=True))
      except:
        msg = "wget failed on {0}.".format(url)
        logger.exception(msg)
        raise Exception(msg)
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



def subset(resource=[], bbox="-80,50,22.5,70",  time_region=None, variable=None):
  """
  extracts the time regions (e.g. '12,1,2') and bounding box from a dataset

  :param resource: List of files belonging to one dataset
  :param bbox: geographical region lat/lon
  :param time_region: month to be picked from the data
  :returns netCDF: file containing containing lat weighted and normalize (by anual cycle) data
  """
  
  from flyingpigeon.ocgis_module import call

  if type(resource) == str: 
    resource = list([resource])

  if variable == None:
    variable = utils.get_variable(resource[0])

  if not time_region == None:
    month = map(int, time_region.split(','))
    time_region = {'month':month}

  nc_grouped = call(resource=resource,  conform_units_to='hPa', variable=variable, time_region=time_region, prefix='nc_grouped')

  # conform_units_to='hPa', 
  
  from cdo import Cdo
  cdo = Cdo()

  nc_subset  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output='nc_subset.nc')
  logger.info('subset done: %s ' % nc_subset)

  nc_weighted = weight_by_lat(nc_subset)

  nc_anual_cycle = cdo.ydaymean(input= nc_weighted , output='nc_anual_cycle.nc') #call(resource=nc_subset, calc=calc, calc_grouping=calc_grouping, variable=variable)
  nc_normalized = cdo.sub(input=[nc_weighted, nc_anual_cycle], output= 'nc_normalized.nc')

  return nc_normalized


def get_NCEPmatrix(dictionary):
  """
  Compares the model weather regimes with the NCEP weather regimes
  :param dictionary: dictionary of weather regimes
  :returns matix: modelmatrix
  """
  from scipy.interpolate import griddata
  
  ncep = dictionary['NCEP']
  dictionary.pop('NCEP')
  domain = ncep['regime 1'].shape
  
  for key in dictionary.keys(): 
    for regime in dictionary[key].keys():
      print regime
      
  
  return domain #ncep, models # modelmatrix