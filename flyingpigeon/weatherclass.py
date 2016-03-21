def get_season(date):
    """
    convert month to season Nr.
    """
    m = date.month 

    if ((m >= 2) and (m <= 4)):
        s = 1  # spring
    elif ((m >= 5) and (m <= 7)):
        s = 2  # summer
    elif ((m >= 8) and (m <= 10)):
        s = 3  # fall
    elif ((m >= 11) or (m == 1)):
        s = 4  # winter
    else:
        raise IndexError("Invalid date")
    return s
  
def plot_mnist(X, y, X_embedded, name, min_dist=10.0):
  from matplotlib import pyplot as plt
  fig = plt.figure(figsize=(10, 10))
  ax = plt.axes(frameon=True)
  plt.subplots_adjust(left=0.0, bottom=0.0, right=1.0, top=0.9,
                  wspace=0.0, hspace=0.0)
  plt.scatter(X_embedded[:, 0], X_embedded[:, 1],
          c=y, marker="x")
  plt.title('filename')
  image = 'cluster.png'
  
  plt.savefig(image)
  return image 
    
def get_pca(resource):
  """
  calculation of p components

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
  time = ds.variables['time']
  
  # make array of seasons:
  # convert netCDF timesteps to datetime
  timestamps = num2date(time[:], time.units, time.calendar)
  season = [get_season(s) for s in timestamps]
  
  from matplotlib import pyplot as plt
  from cartopy import config
  from cartopy.util import add_cyclic_point
  import cartopy.crs as ccrs
  #from numpy import meshgrid
  
  from sklearn.decomposition import PCA
  import numpy as np
  
  # reshape
  data = np.array(psl)
  adata = data.reshape(psl[:].shape[0], (psl[:].shape[1] * psl[:].shape[2]) )
  pca = PCA(n_components=50).fit_transform(adata)
  
  return pca, season


def tSNE(resource):
  from sklearn.manifold import TSNE
  X_all, y_all = get_pca(resource)

  X_all_embedded = TSNE(n_components=2, perplexity=40, verbose=2).fit_transform(X_all)
  
  image = plot_mnist(X_all, y_all, X_all_embedded, "t-SNE", min_dist=20.0)
  
  return image
