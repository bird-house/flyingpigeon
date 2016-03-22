def get_pca(resource):
  """
  calculation of principal components

  :param resource: netCDF file containing pressure values for a defined region and selected timesteps
  :return pca: sklean objct
  """
  from netCDF4 import Dataset, num2date
  from flyingpigeon.utils import get_variable

  var = get_variable(resource)
  print 'variable name: %s' % var
  ds = Dataset(resource)
  psl = ds.variables[var]
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
  data = np.array(psl)
  adata = data.reshape(psl[:].shape[0], (psl[:].shape[1] * psl[:].shape[2]) )
  pca = PCA(n_components=50).fit_transform(adata)
  return pca #, season


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