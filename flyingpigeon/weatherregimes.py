from flyingpigeon import utils
from tempfile import mkstemp

import logging
logger = logging.getLogger(__name__)

def get_pca(resource):
  """
  calculation of principal components (PCA)
  
  :param resource: input netCDF file 
  :returns data, pca: normalised data, sklean objct
  """
  from netCDF4 import Dataset, num2date
  from flyingpigeon.utils import get_variable
  from numpy import mean
  import numpy as np
  
  var = get_variable(resource)
  ds = Dataset(resource)
  vals = np.squeeze(ds.variables[var])

  lat = ds.variables['lat']
  lon = ds.variables['lon']
  #time = ds.variables['time']
  
  # make array of seasons:
  # convert netCDF timesteps to datetime
  #timestamps = num2date(time[:], time.units, time.calendar)
  #season = [get_season(s) for s in timestamps]
  
  from sklearn.decomposition import PCA
 
  # reshape
  #data = np.array(vals)
  #m = mean(vals)
  #mdata = vals - m 
  adata = vals.reshape(vals.shape[0], (vals.shape[1] * vals.shape[2]) )
  pca = PCA(n_components=50).fit_transform(adata)
  return pca #, season

# def calc_tSNE(pca):
#   """
#   perform a cluster analysis with tSNE method
  
#   """
#   from sklearn.manifold import TSNE
#   #X_all, y_all = get_pca(resource)
#   data = TSNE(n_components=2, perplexity=40, verbose=2).fit_transform(pca)
#   return data

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


def smooth(y, window_size, order, deriv=0, rate=1):
    """Smooth (and optionally differentiate) data with a Savitzky-Golay filter.
    The Savitzky-Golay filter removes high frequency noise from data.
     It has the advantage of preserving the original shape and
     features of the signal better than other types of filtering
    approaches, such as moving averages techniques.
    Parameters
    ----------
    y : array_like, shape (N,)
    the values of the time history of the signal.
    window_size : int
    the length of the window. Must be an odd integer number.
    order : int
           the order of the polynomial used in the filtering.
         Must be less then `window_size` - 1.
     deriv: int
         the order of the derivative to compute (default = 0 means only smoothing)
     Returns
     -------
     ys : ndarray, shape (N)
         the smoothed signal (or it's n-th derivative).
     Notes
     -----
     The Savitzky-Golay is a type of low-pass filter, particularly
     suited for smoothing noisy data. The main idea behind this
     approach is to make for each point a least-square fit with a
     polynomial of high order over a odd-sized window centered at
     the point.
     [1] A. Savitzky, M. J. E. Golay, Smoothing and Differentiation of
     Data by Simplified Least Squares Procedures. Analytical
     Chemistry, 1964, 36 (8), pp 1627-1639.
     [2] Numerical Recipes 3rd Edition: The Art of Scientific Computing
     W.H. Press, S.A. Teukolsky, W.T. Vetterling, B.P. Flannery
     Cambridge University Press ISBN-13: 9780521880688
     """

    import numpy as np
    from math import factorial
    
    try:
        window_size = np.abs(np.int(window_size))
        order = np.abs(np.int(order))
    except ValueError, msg:
         raise ValueError("window_size and order have to be of type int")
    if window_size % 2 != 1 or window_size < 1:
         raise TypeError("window_size size must be a positive odd number")
    if window_size < order + 2:
        raise TypeError("window_size is too small for the polynomials order")
    order_range = range(order+1)
    half_window = (window_size -1) // 2
    # precompute coefficients
    b = np.mat([[k**i for i in order_range] for k in range(-half_window, half_window+1)])
    m = np.linalg.pinv(b).A[deriv] * rate**deriv * factorial(deriv)
    # pad the signal at the extremes with
    # values taken from the signal itself
    firstvals = y[0] - np.abs( y[1:half_window+1][::-1] - y[0] )
    lastvals = y[-1] + np.abs(y[-half_window-1:-1][::-1] - y[-1])
    y = np.concatenate((firstvals, y, lastvals))
    return np.convolve( m[::-1], y, mode='valid')



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
    logger.debug('score_pattern failed: pattern are in differnet shape')
  
  regr = linear_model.LinearRegression()
  regr.fit(pattern1, pattern2)
  score = regr.score(pattern1, pattern2)
  return score


# def subset(resource=[], bbox="-80,50,22.5,70",  time_region=None, time_range=None,
#            variable=None, level=None, conform_units_to='hPa', 
#            regrid_destination=None, regrid_options=None,):
#   """
#   extracts the time regions (e.g. '12,1,2') and bounding box from a dataset

#   :param resource: List of files belonging to one dataset
#   :param bbox: geographical region lat/lon
#   :param time_region: month to be picked from the data
#   :param time_range: defines the start and end time to be taken into accont (time_range=[datetime(start), datetime(end)])
#   :returns netCDF: file containing containing lat weighted and normalize (by anual cycle) data
#   """
#   from flyingpigeon.ocgis_module import call
#   from tempfile import mkstemp
#   import uuid

#   if type(resource) == str: 
#     resource = list([resource])

#   if variable == None:
#     variable = utils.get_variable(resource)

#   if not time_region == None:
#     month = map(int, time_region.split(','))
#     time_region = {'month':month}
    
#     minx, maxx, miny, maxy  =  bbox.split(',')  # ( minx , miny , maxx , maxy )
#     geom = [ float(minx) , float(miny) , float(maxx) , float(maxy) ]

#   nc_grouped = call(resource=resource,  
#     conform_units_to=conform_units_to,
#     #geom= geom, # not possible due to wrapping difficulties in ocgis
#     #prefix=str(uuid.uuid1()),
#     regrid_destination=regrid_destination,
#     regrid_options='bil',
#     time_region=time_region,
#     time_range=time_range,
#     variable=variable)
  
#   from cdo import Cdo
#   cdo = Cdo()
#   ip, nc_subset = mkstemp(dir='.',suffix='.nc')
#   nc_subset  = cdo.sellonlatbox('%s' % bbox, input=nc_grouped, output=nc_subset)
#   logger.info('subset done: %s ' % nc_subset)
#   if level != None:
#     nc_level = get_level( nc_subset, level) 
#     nc_subset =  nc_level
  
#   # nc_subset = nc_grouped
#   nc_weighted = weight_by_lat(nc_subset)
  
#   ip , nc_anual_cycle = mkstemp(dir='.',suffix='.nc')
#   ip , nc_normalized = mkstemp(dir='.',suffix='.nc')

#   nc_anual_cycle = cdo.ydaymean(input= nc_weighted , output=nc_anual_cycle) 
#   nc_normalized = cdo.sub(input=[nc_weighted, nc_anual_cycle], output= nc_normalized )
#   logger.info('weightning and normalisation done: %s ' % nc_subset)
  
#   return nc_normalized

def get_anomalies(nc_file, frac=0.2, reference=None):
  
  from flyingpigeon.ocgis_module import call

  variable = utils.get_variable(nc_file)

  calc = [{'func': 'mean', 'name': variable}]
  calc_grouping = calc_grouping = ['day','year']
  nc_anual_cycle = call(nc_file, calc=calc, calc_grouping=calc_grouping, time_range=reference)
  
  # ip , nc_anual_cycle = mkstemp(dir='.',suffix='.nc')
  # ip , nc_period = mkstemp(dir='.',suffix='.nc')
  # nc_period = cdo.selyear('1970/1999', input= nc_file, output=nc_period)
  # nc_anual_cycle = cdo.ydaymean(input= nc_period , output=nc_anual_cycle) 
  ### spline for smoothing
  
  import statsmodels.api as sm
  from numpy import tile, empty, linspace
  from netCDF4 import Dataset
  from cdo import Cdo
  cdo = Cdo()
  
  try:
    # variable = utils.get_variable(nc_file)
    ds = Dataset(nc_anual_cycle, mode='a')
    vals = ds.variables[variable]
    vals_sm = empty(vals.shape)
    ts = vals.shape[0]
    x = linspace(1, ts*3 , num=ts*3 , endpoint=True)

    for lat in range(vals.shape[1]):
      for lon in range(vals.shape[2]):
        try:
          y = tile(vals[:,lat,lon], 3)
          ys = smooth(y, window_size=91, order=2, deriv=0, rate=1)[ts:ts*2]
          # ys = smooth(y,  window_size=91, order=2, deriv=0, rate=1)
          # ys = sm.nonparametric.lowess(y, x, frac=frac )[ts:ts*2,1]
          vals_sm[:,lat,lon] = ys
        except Exception as e:
          msg = 'failed for lat %s lon %s  %s ' % (lat,lon,e)
          logger.debug('failed for lat %s lon %s  %s ' % (lat,lon,e))
          raise Exception(msg)
      print 'done for %s - %s ' % (lat, lon)    
    vals[:,:,:] = vals_sm[:,:,:]
    ds.close()
    logger.info('smothing of anual cycle done')
  except Exception as e:
    msg = 'failed smothing of anual cycle %s ' % e
    logger.error(msg)
    raise Exception(msg)
  
  ip , nc_anomal = mkstemp(dir='.',suffix='.nc')
  nc_anomal = cdo.sub(input=[nc_file, nc_anual_cycle], output= nc_anomal )
  logger.info('anomalisation done: %s ' % nc_anomal)
  return nc_anomal


def get_ponderate(nc_file):
  from netCDF4 import Dataset
  from numpy import meshgrid, sqrt, cos, pi, broadcast_arrays

  variable = utils.get_variable(nc_file)
  ds = Dataset(nc_file, mode='a')
  
  lat = ds.variables['lat']
  lon = ds.variables['lon']

  lons, lats = meshgrid(lon, lat)
  vals = ds.variables[variable]
  ponderate = 1/sqrt(cos(lats[:]*pi/180))

  weights3D = broadcast_arrays(vals,ponderate)[1]
  vals_weighted = vals[:] * weights3D
  vals[:,:,:] = vals_weighted[:,:,:]  
  ds.close()
  return ponderate
  


# def subset_model(resource=[], bbox="-80,50,22.5,70",  time_region=None, variable=None, regrid_destination=None):
#   """
#   extracts the time regions (e.g. '12,1,2') and bounding box from a dataset
  
#   Temporary module will be deleted soon

#   :param resource: List of files belonging to one dataset
#   :param bbox: geographical region lat/lon
#   :param time_region: month to be picked from the data
#   :returns netCDF: file containing containing lat weighted and normalize (by anual cycle) data
#   """
#   from flyingpigeon.ocgis_module import call
  
#   import uuid
#   from shutil import rmtree

#   if type(resource) == str: 
#     resource = list([resource])

#   if variable == None:
#     variable = utils.get_variable(resource[0])

#   if not time_region == None:
#     month = map(int, time_region.split(','))
#     time_region = {'month':month}

#   from cdo import Cdo
#   cdo = Cdo()

#   ip, nc_grouped = mkstemp(dir='.',suffix='.nc')

#   nc_grouped = cdo.cat(input=resource, output=nc_grouped)
#   logger.info('concatination done: %s ' % nc_grouped)

#   ip, nc_remap = mkstemp(dir='.',suffix='.nc')
#   nc_remap  = cdo.remapbil('%s' % regrid_destination, input=nc_grouped, output=nc_remap)
#   logger.info('remapbil done: %s ' % nc_remap)
  
#   rmtree(nc_grouped, ignore_errors=True, onerror=None)
#   logger.info('concatination removed')
  
#   nc_unitconvert = call(resource=nc_remap,  
#     conform_units_to='hPa',
#     #geom= [float(n) for n in bbox.split(',')], # not possible due to wrapping difficulties in ocgis
#     #prefix=str(uuid.uuid1()),
#     #regrid_destination=regrid_destination,
#     #regrid_options='bil',
#     time_region=time_region,
#     variable=variable)
  
#   # nc_subset = nc_grouped
#   nc_weighted = weight_by_lat(nc_unitconvert)
  
#   ip , nc_anual_cycle = mkstemp(dir='.',suffix='.nc')
#   ip , nc_normalized = mkstemp(dir='.',suffix='.nc')

#   nc_anual_cycle = cdo.ydaymean(input= nc_weighted , output=nc_anual_cycle) 
#   nc_normalized = cdo.sub(input=[nc_weighted, nc_anual_cycle], output= nc_normalized )
#   logger.info('weightning and normalisation done: %s ' % nc_normalized)
  
#   return nc_normalized
