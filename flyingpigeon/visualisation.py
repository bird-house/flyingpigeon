import os
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date 
import numpy as np

from flyingpigeon import utils

import logging
logger = logging.getLogger(__name__)

from matplotlib.colors import Normalize
class MidpointNormalize(Normalize):
  def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
    self.midpoint = midpoint
    Normalize.__init__(self, vmin, vmax, clip)

  def __call__(self, value, clip=None):
    x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
    return np.ma.masked_array(np.interp(value, x, y))


def spaghetti(resouces, variable=None, title=None, dir_out=None):
  """
  retunes an html file containing the appropriate spaghetti plot. 
  
  :param resouces: list of files containing the same variable 
  :param variable: variable to be visualised, if None (default) variable will be detected
  :param title: sting to be used as title
  :param dir_out: directory for output files
  """

  import matplotlib.pyplot as plt

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

  # === prepare bokeh
  try: 
    o1 , output_png = mkstemp(dir=dir_out, suffix='.png')
    
    #output_file(output_html)
    #bplt.save()
    
    #fig = figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
    #logger.debug('output_file.html created')
    #output_file(output_html, title=variable, autosave=True,)
    #bplt.hold()
    
    for c , nc in enumerate(resouces):
      # get timestapms
      try: 
        d =  utils.get_time(nc) # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
        
        dt = [datetime.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in d ]
        ds=Dataset(nc)
        data = np.squeeze(ds.variables[variable][:])
        if len(data.shape) == 3: 
          meanData = np.mean(data,axis=1)
          ts = np.mean(meanData,axis=1)
        else: 
          ts = data
        plt.plot( dt,ts )
        #fig.line( dt,ts )
      except Exception as e:
        logger.exception('bokeh lineplot failed for %s: %s\n' % (nc, e))

      # plot into current figure
      # , legend= nc 
    
    #fig.legend()[0].orientation = "bottom_left"
    # fig.legend().orientation = "bottom_left"
    plt.title(title)
    plt.grid()# .grid_line_alpha=0.3

    #window_size = 30
    #window = np.ones(window_size)/float(window_size)
    fig.savefig(output_png)
    #bplt.hold('off')
    
    plt.close()
    
    logger.debug('timesseries spagetti plot done for %s with %s lines.'% (variable, c)) 
  except Exception as e:
    logger.exception('matplotlib spagetti plot failed for %s : %s\n' % (variable , e))
    raise  
  return output_png 

def uncertainty(resouces , variable=None, title=None, dir_out=None): 
  """
  retunes an html file containing the appropriate uncertainty plot. 
  
  :param resouces: list of files containing the same variable 
  :param variable: variable to be visualised, if None (default) variable will be detected
  :param title: sting to be used as title
  """
  logger.debug('Start visualisation uncertainty plot')

  from bokeh.plotting import figure, output_file, save

  import cdo 
  cdo = cdo.Cdo()
  
  # === prepare invironment
  if type(resouces) == str: 
    resouces = list([resouces])    
  if variable == None:
    variable = utils.get_variable(resouces[0])
  if title == None:
    title = "Field mean of %s " % variable
  if dir_out == None: 
    dir_out = '.'

  # === prepare bokeh
  try: 
    o1 , output_html = mkstemp(dir=dir_out, suffix='.html')
  
    fig = figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
    
    output_file(output_html, title=variable, autosave=True,)
  
    # === get the datetime
    dates = set()
    for nc in resouces:

        logger.debug('looping files : %s ' % (nc))
        # get timestapms
        rawDate = cdo.showdate(input=[nc]) # ds.variables['time'][:]
        strDate = rawDate[0].split('  ')
        logger.debug('len strDate : %s ' % (len(strDate)))
        dates =  dates.union(strDate) #dates.union( utils.get_time(nc))

    #self.show_status('dates : %s ' % len(dates), 62)
    ldates = list(dates)
    ldates.sort()
    ddates = dict( (ldates[i], i) for i in range(0,len(ldates)))

    # initialise matirx

    ma = np.empty([len(ddates), len(resouces)])*np.nan
    #self.show_status('ddates : %s ' % ddates, 62)

    # fill matrix
    for y in range(0,len(resouces)) : 
        rawDate = cdo.showdate(input=[resouces[y]]) # ds.variables['time'][:]
        strDate = rawDate[0].split('  ')

        ds=Dataset(resouces[y])
        data = np.squeeze(ds.variables[variable][:])
        if len(data.shape) == 3: 
          meanData = np.mean(data,axis=1)
          ts = np.mean(meanData,axis=1)
        else: 
          ts = data
        logger.debug('ts array  : %s ' % (len(ts)) )
        
        for t in range(0, len(strDate)) : 
            x = ddates.get(strDate[t],0)
            ma[x,y] = ts[t]
        
    # get datetimes
    dt = [datetime.strptime(elem, '%Y-%m-%d') for elem in ldates]
    mdat = np.ma.masked_array(ma ,np.isnan(ma))

    #self.show_status('matrix masked %s ' % mdat , 80)
    #logger.debug('matrix %s ', mdat.shape) 

    ma_mean = np.mean(mdat,axis=1)
    logger.debug('mean  %s '%  len(ma_mean))
    ma_min = np.min(mdat,axis=1)
    ma_max = np.max(mdat,axis=1)
    #ma_sub = np.subtract(ma_max, ma_min)
    #ma_per75 = np.percentile(mdat,75, axis=0)
    #ma_per25 = np.percentile(mdat,25, axis=0)
    logger.debug('ma Vaules %s' % len(mdat.data))

    #line(dt, ma_min , color='grey' ,line_width=1)
    #line(dt, ma_max , color='grey' , line_width=2 )
    fig.line(dt, ma_mean , color='red', line_width=1)

    x = []
    y = []
    x = np.append(dt,dt[::-1])
    y = np.append(ma_min, ma_max[::-1])

    fig.patch(x,y, color='grey', alpha=0.8, line_color=None)
    
    fig.title = "Mean and Uncertainty of  %s " % variable
    fig.grid
    
    
    save(fig)


    #hold('off')
  
    logger.debug('timesseries uncertainty plot done for %s'% variable) 
  except Exception as e:
    logger.exception('bokeh uncertainty plot failed for %s' % variable)
    raise  
  return output_html  

def map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, variable, cmap='seismic', title=None):
  """
  generating a graphic for the output of the ensembleRobustness process for a lat/long file. 

  :param signal: netCDF file containing the signal differnce over time
  :param highagreement: 
  :param lowagreement: 
  :param variable:
  :param cmap: default='seismic',
  :param title: default='Modelagreement of Signal'
  """

  try: 
    import matplotlib.pyplot as plt
    from cartopy import config
    from cartopy.util import add_cyclic_point
    import cartopy.crs as ccrs
    logger.info('libraries loaded')
  except Exception as e: 
    logger.error('failed to load libraries: %s' % e)

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
    
    # if 'rlon' in ds_signal.variables:
    #   lons = np.squeeze(ds_signal.variables['rlon'][:])
    #   lats = np.squeeze(ds_signal.variables['rlat'][:])
    #   logger.info('rlat rlon loaded')

    # elif 'lon' in ds_signal.variables:
    lons = np.squeeze(ds_signal.variables['lon'][:])
    lats = np.squeeze(ds_signal.variables['lat'][:])
    # var_signal, lons = util.add_cyclic_point(var_signal, coord=lons, axis=-1)
          
    cyclic_var, cyclic_lons = add_cyclic_point(var_signal, coord=lons)
    mask_l, cyclic_lons = add_cyclic_point(mask_l, coord=lons)
    mask_h, cyclic_lons = add_cyclic_point(mask_h, coord=lons)

    lons = cyclic_lons.data
    var_signal = cyclic_var

    logger.info('lat lon loaded')
    # else: 
    #   logger.error('variables for lat and lon not found') 

    minval = round(np.nanmin(var_signal))
    maxval = round(np.nanmax(var_signal)+.5)
 
    logger.info('prepared data for plotting')
  except Exception as e: 
    logger.error('failed to get data for plotting: %s' % e) 

  try:
    fig = plt.figure(figsize=(20,10), dpi=600, facecolor='w', edgecolor='k')
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
    logger.error(' failed to plot graphic: %s ' % e)

  return graphic