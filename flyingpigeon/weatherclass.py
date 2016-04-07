import wget

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
  m = mean(vals)
  mdata = vals - m 
  adata = mdata.reshape(vals[:].shape[0], (vals[:].shape[1] * vals[:].shape[2]) )
  pca = PCA(n_components=50).fit_transform(adata)
  return mdata , pca #, season


def calc_tSNE(pca):
  """
  perform a cluster analysis 
  
  """
  from sklearn.manifold import TSNE
  #X_all, y_all = get_pca(resource)
  data = TSNE(n_components=2, perplexity=40, verbose=2).fit_transform(pca)
  
  return data

def calc_kMEAN(pca):
  
  from sklearn import cluster
  import numpy as np
  from tempfile import mkstemp

  kmeans = cluster.KMeans(n_clusters=4)
  #cluster.KMeans(n_clusters=4, init='k-means++', n_init=10, max_iter=300, tol=0.0001, precompute_distances='auto', verbose=0, random_state=None, copy_x=True, n_jobs=1)
  
  kmeans.fit(pca)
  return kmeans

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
        ncep_data.append(wget.download(url, bar=None))
      except:
        msg = "wget failed on {0}.".format(url)
        logger.exception(msg)
        raise Exception(msg)
  except:
    msg = "get_NCEP module failed"
    logger.exception(msg)
    raise Exception(msg)
  return ncep_data

def subset(resource=[], bbox="-80,50,22.5,70",  time_region=None, variable=None):
  """
  extracts the time regions (e.g. '12,1,2') and bounding box from a dataset
  :param resource: List of files belonging to one dataset
  :param bbox: geographical region lat/lon
  :param time_region: month to be picked from the data
  :returns str: path to output file
  """
  
  from flyingpigeon.ocgis_module import call

  month = map(int, time_region.split(','))

  nc_grouped = call(resource=resource, variable=variable, time_region={'month':month}, prefix='grouped')

  from cdo import Cdo
  cdo = Cdo()

  data  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output='subset.nc')
  logger.info('subset done: %s ' % data)  

  return data
