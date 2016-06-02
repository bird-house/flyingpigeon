import os
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date 
import numpy as np

import matplotlib
matplotlib.use('Agg')   # use this if no xserver is available
from matplotlib import pyplot as plt
from matplotlib.colors import Normalize

from cartopy import config
from cartopy.util import add_cyclic_point
import cartopy.crs as ccrs

from flyingpigeon import utils

import logging
logger = logging.getLogger(__name__)


class MidpointNormalize(Normalize):
  def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
    self.midpoint = midpoint
    Normalize.__init__(self, vmin, vmax, clip)

  def __call__(self, value, clip=None):
    x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
    return np.ma.masked_array(np.interp(value, x, y))


def spaghetti(resouces, variable=None, title=None, dir_out=None):
  """
  retunes a png file containing the appropriate spaghetti plot. 
  
  :param resouces: list of files containing the same variable 
  :param variable: variable to be visualised, if None (default) variable will be detected
  :param title: sting to be used as title
  :param dir_out: directory for output files
  """

  fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')

  logger.debug('Start visualisation spagetti plot')
  
  # === prepare invironment
  if type(resouces) == str: 
    resouces = list([resouces])    
  if variable == None:
    variable = utils.get_variable(resouces[0])
  if title == None:
    title = "Field mean of %s " % variable
  if dir_out == None: 
    dir_out = os.curdir

  try: 
    o1 , output_png = mkstemp(dir=dir_out, suffix='.png')
    
    for c , nc in enumerate(resouces):
      # get timestapms
      try: 
        d = utils.get_time(nc) # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
        
        dt = [datetime.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in d ]
        ds=Dataset(nc)
        data = np.squeeze(ds.variables[variable][:])
        if len(data.shape) == 3: 
          meanData = np.mean(data,axis=1)
          ts = np.mean(meanData,axis=1)
        else: 
          ts = data[:]
        plt.plot( dt,ts )
        #fig.line( dt,ts )
      except Exception as e:
        logger.debug('lineplot failed for %s: %s\n' % (nc, e))

      # plot into current figure
      # , legend= nc 
    
    #fig.legend()[0].orientation = "bottom_left"
    # fig.legend().orientation = "bottom_left"
    plt.title(title, fontsize=20)
    plt.grid()# .grid_line_alpha=0.3
    #lt.rcParams.update({'font.size': 22})
    #window_size = 30
    #window = np.ones(window_size)/float(window_size)
    fig.savefig(output_png)
    #bplt.hold('off')
    
    plt.close()
    
    logger.debug('timesseries spagetti plot done for %s with %s lines.'% (variable, c)) 
  except Exception as e:
    msg = 'matplotlib spagetti plot failed for %s' % variable
    logger.debug(msg)
    #raise Exception(msg) 
  return output_png 

def uncertainty(resouces , variable=None, ylim=None, title=None, dir_out=None): 
  """
  returnes a png file containing the appropriate uncertainty plot. 
  
  :param resouces: list of files containing the same variable 
  :param variable: variable to be visualised, if None (default) variable will be detected
  :param title: sting to be used as title
  :returns str: path/to/file.png
  """
  logger.debug('Start visualisation uncertainty plot')

  import pandas as pd
  import pandas as pd
  import numpy as np
  import netCDF4
  from os.path import basename
  
  # === prepare invironment
  if type(resouces) == str: 
    resouces = list([resouces])    
  if variable == None:
    variable = utils.get_variable(resouces[0])
  if title == None:
    title = "Field mean of %s " % variable
  if dir_out == None: 
    dir_out = '.'
  
  try:
    fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')
    o1 , output_png = mkstemp(dir=dir_out, suffix='.png')
    variable = utils.get_variable(resouces[0])
    df = pd.DataFrame()
    for f in resouces:
      ds = Dataset(f)
      data = np.squeeze(ds.variables[variable][:])
      if len(data.shape) == 3: 
        meanData = np.mean(data,axis=1)
        ts = np.mean(meanData,axis=1)
      else: 
        ts = data[:]

      times = ds.variables['time']
      jd = netCDF4.num2date(times[:],times.units)
      
      hs = pd.Series(ts, index=jd, name=basename(f))
      hd = hs.to_frame()
      df[basename(f)] = hs# 

    rollmean = df.rolling(window=30,center=True).mean()
    
    rmean = rollmean.median(axis=1, skipna=False)#  quantile([0.5], axis=1, numeric_only=False )
    q05 = rollmean.quantile([0.05], axis=1,)# numeric_only=False)
    q33 = rollmean.quantile([0.33], axis=1,)# numeric_only=False)
    q66 = rollmean.quantile([0.66], axis=1, )#numeric_only=False)
    q95 = rollmean.quantile([0.95], axis=1, )#numeric_only=False)
    
    plt.fill_between(rollmean.index.values,  np.squeeze(q05.values), np.squeeze( q95.values), alpha=0.5, color='grey')
    plt.fill_between(rollmean.index.values, np.squeeze( q33.values),np.squeeze( q66.values), alpha=0.5, color='grey')
    plt.plot(rollmean.index.values, np.squeeze(rmean.values), c='r', lw=3)
    
    plt.xlim(min(df.index.values), max(df.index.values))
    plt.ylim(ylim)
    plt.title(title, fontsize=20)
    plt.grid()# .grid_line_alpha=0.3
    #lt.rcParams.update({'font.size': 22})
    #window_size = 30
    #window = np.ones(window_size)/float(window_size)
    fig.savefig(output_png)
    #bplt.hold('off')
    
    plt.close()
    
    logger.debug('timesseries uncertainty plot done for %s'% variable) 
  except Exception as e:
    logger.exception('uncertainty plot failed for %s' % variable)
    raise  
  return output_png 

def map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, variable, cmap='seismic', title=None):
  """
  generating a graphic for the output of the ensembleRobustness process for a lat/long file. 

  :param signal: netCDF file containing the signal differnce over time
  :param highagreement: 
  :param lowagreement: 
  :param variable:
  :param cmap: default='seismic',
  :param title: default='Modelagreement of Signal'
  :returns str: path/to/file.png
  """

  #try:
    ##import matplotlib.pyplot as plt
    ##plt.switch_backend('agg')  # dont use x-server
    ##from cartopy import config
    ##from cartopy.util import add_cyclic_point
    ##import cartopy.crs as ccrs
    #logger.info('libraries loaded')
  #except Exception as e:
    #msg = 'failed to load libraries'  
    #logger.exception(msg)
    #raise Exception(msg)

  try: 
   # get the path of the file. It can be found in the repo data directory.
   
    ds_signal = Dataset(signal,mode='r')
    ds_lagree = Dataset(low_agreement_mask,mode='r')
    ds_hagree = Dataset(high_agreement_mask,mode='r')

    var_signal = np.squeeze(ds_signal.variables[variable])
    mask_l = np.squeeze(ds_lagree.variables[variable])
    mask_h = np.squeeze(ds_hagree.variables[variable])

    mask_l[mask_l==0]=np.nan
    mask_h[mask_h==0]=np.nan

    logger.info('data loaded')
    
    lons = np.squeeze(ds_signal.variables['lon'][:])
    lats = np.squeeze(ds_signal.variables['lat'][:])
          
    cyclic_var, cyclic_lons = add_cyclic_point(var_signal, coord=lons)
    mask_l, cyclic_lons = add_cyclic_point(mask_l, coord=lons)
    mask_h, cyclic_lons = add_cyclic_point(mask_h, coord=lons)

    lons = cyclic_lons.data
    var_signal = cyclic_var

    logger.info('lat lon loaded')
    
    minval = round(np.nanmin(var_signal))
    maxval = round(np.nanmax(var_signal)+.5)
 
    logger.info('prepared data for plotting')
  except Exception as e:
    msg = 'failed to get data for plotting %s' % e  
    logger.exception(msg)
    raise Exception(msg) 

  try:
    fig = plt.figure( facecolor='w', edgecolor='k') # figsize=(20,10), dpi=600,
    ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
    norm = MidpointNormalize(midpoint=0)

    cs = plt.contourf(lons, lats, var_signal, 60, norm=norm, transform=ccrs.PlateCarree(), cmap=cmap, interpolation='nearest')
    cl = plt.contourf(lons, lats, mask_l, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['//']) 
    ch = plt.contourf(lons, lats, mask_h, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['.'])

    # plt.clim(minval,maxval)
    ax.coastlines()
    ax.set_global()
    
    if title == None: 
      plt.title('%s with Agreement' % variable)
    else: 
      plt.title(title)
      
    plt.colorbar(cs) 

    plt.annotate('// = low model ensemble agreement', (0,0), (0, -10), xycoords='axes fraction', textcoords='offset points', va='top')
    plt.annotate('..  = high model ensemble agreement', (0,0), (0, -20), xycoords='axes fraction', textcoords='offset points', va='top')
    
    graphic = 'modelAgreement.png'
    fig.savefig(graphic)

    plt.close()
    
    logger.info('Plot created and figure saved')
  except Exception as e:
    msg = 'failed to plot graphic'  
    logger.exception(msg)
    raise Exception(msg)

  return graphic

#def plot_tSNE(data, title='custer', sub_title='method: principal components'):
  #"""
  #plot the output of weather classifiaction as a cluster
  #:param param: values for x y coordinate
  #:param title: string for title
  #"""
  #fig = plt.figure(figsize=(10, 10))
  #plt.scatter(data[:, 0], data[:, 1], marker=".")
  #plt.title(title)
  #plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction', textcoords='offset points', va='top')
  
  #ip, image = mkstemp(dir='.',suffix='.png')
  #plt.savefig(image)
  #plt.close()
  
  #return image 

def plot_kMEAN(kmeans, pca, title='kmean', sub_title='file='):
  from itertools import cycle  
  centroids = kmeans.cluster_centers_
  
  c = kmeans.predict(pca)
  x = pca[:, 0]
  y = pca[:, 1]
  
  fig = plt.figure(figsize=(10, 10))
  
  cx = centroids[:, 0]
  cy= centroids[:, 1]
  ct = plt.scatter(cx, cy,
            marker='.', s=100, linewidths=3,
            color='black', zorder=10)
  
  #n = ['1', '2','3','4']
  
  #for i, txt in enumerate(n):
    #plt.annotate(txt, (cx[i],cy[i]))
  
  colors = cycle(["r", "b", "g", "y"])
  
  for i in range(max(c)+1):
    plt.scatter(x[c==i],y[c==i],marker='.', s=30, lw=None, color=next(colors))
  
  plt.axvline(0)
  plt.axhline(0)
  plt.title(title)
  
  plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction', textcoords='offset points', va='top')
  
  ip, image = mkstemp(dir='.',suffix='.png')
  plt.savefig(image)
  plt.close()
  
  return image


def plot_pressuremap(data, lats=None, lons=None,
                    facecolor=None,  edgecolor=None, vmin=None, vmax=None,
                    title='Pressure Pattern', 
                    sub_title='plotted in birdhouse'):
  """
  plots pressure data
  :param data: 2D or 3D array of pressure data. if data == 3D a mean will be calculated
  :param lats: 1D or 2D array for latitude coordinates (geographcal map will be plotted if lats / lons are provided)
  :param lons: 1D or 2D array for longitude coordinates (geographcal map will be plotted if lats / lons are provided)
  :param title: string for title
  :param sub_title: string for sub_title
  """
  from numpy import squeeze, mean, meshgrid
  norm = MidpointNormalize(midpoint=0, vmin=vmin, vmax=vmax)
  d = squeeze(data)

  if len(d.shape)==3:
    d = mean(d, axis=0)
  if len(d.shape)!=2:
    logger.error('data are not in shape for map display')

  # fig = plt.figure( )
  # fig.patch.set_facecolor(facecolor)
  
  if not (lats == None or lons == None):  
    if len(lats.shape) == 1: 
      lons, lats = meshgrid( lons, lats)
    central_longitude = int(mean(lons))
    
    #AlbersEqualArea(central_longitude=0.0, central_latitude=0.0, false_easting=0.0, false_northing=0.0, standard_parallels=(20.0, 50.0), globe=None)
    
    ax = plt.axes(projection=ccrs.AlbersEqualArea(central_longitude=central_longitude), axisbg=facecolor) #,Robinson(central_longitude=central_longitude))
    ax.gridlines() 
    ax.coastlines()
    
    cf = plt.contourf(lons, lats, d, 60, transform=ccrs.PlateCarree(), norm=norm, cmap='jet', interpolation=None) #'nearest'
    co = plt.contour(lons, lats, d, transform=ccrs.PlateCarree(), lw=2, color='black')
  else:
    cf = plt.contourf(d, norm=norm, cmap='jet')
    co = plt.contour(d, lw=2, c='black')
    
  plt.colorbar(cf, shrink=0.5,)
  # clb = plt.colorbar( ticks=clevs)
  plt.clabel(co, inline=1) # fontsize=10
  plt.title(title)
  plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction',
               textcoords='offset points', va='top')

  ip, image = mkstemp(dir='.',suffix='.png')
  plt.savefig(image)
  plt.close()
  return image


def concat_images(images, orientation='v'): 
  """ 
  concatination of images.
  :param images: list of images
  :param orientation: vertical ('v' default) or horizontal ('h') concatination
  :return string: path to image  
  """
  from PIL import Image
  import sys

  open_images = map(Image.open, images)
  w = max(i.size[0] for i in open_images)
  h = max(i.size[1] for i in open_images)
  nr = len(open_images)
  
  if orientation == 'v': 
    result = Image.new("RGB", (w, h * nr))
    #p = nr # h / len(images) 
    for i in range(len(open_images)):
      oi = open_images[i] 
      
      cw = oi.size[0]
      ch = oi.size[1]
      cp = h * i
      box = [0,cp,cw,ch+cp]
      
      result.paste(oi, box=box)

  if orientation == 'h': 
    result = Image.new("RGB", (w * nr , h ))
    #p = nr # h / len(images) 
    for i in range(len(open_images)):
      oi = open_images[i] 
      
      cw = oi.size[0]
      ch = oi.size[1]
      cp = w * i
      box = [cp,0,cw+cp,ch]
      result.paste(oi, box=box)
  
  ip, image = mkstemp(dir='.',suffix='.png')  
  result.save(image)

  return image


def map_gbifoccurrences(latlon):
  
  import matplotlib.pyplot as plt
  from cartopy import config
  from cartopy.util import add_cyclic_point
  import cartopy.crs as ccrs
  
  tree_presents = 'tree_presents.png'
  fig = plt.figure(figsize=(20,10), facecolor='w', edgecolor='k')
  ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
  ax.coastlines()
  ax.set_global()
  cs = plt.scatter(latlon[:,1], latlon[:,0], transform=ccrs.PlateCarree())
  fig.savefig(tree_presents)
  plt.close()
 
  return tree_presents
