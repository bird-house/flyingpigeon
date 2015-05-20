""" processes for visualisation """
from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)


def spaghetti(resouces , variable=None, title=None, dir_out=None):
  """
  retunes an html file containing the appropriate spaghetti plot. 
  
  :param resouces: list of files containing the same variable 
  :param variable: variable to be visualised, if None (default) variable will be detected
  :param title: sting to be used as title
  """
  from flyingpigeon import utils
  from tempfile import mkstemp
  from netCDF4 import Dataset
  from datetime import datetime, date
  import numpy as np
 

  from bokeh.plotting import figure, output_file, vplot
  
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
  o1 , output_html = mkstemp(dir=dir_out, suffix='.html')
  output_file(output_html)
  #bplt.save()
  
  fig = figure(x_axis_type = "datetime", tools="pan,wheel_zoom,box_zoom,reset,previewsave")
  logger.debug('output_file created')
  
  #bplt.hold()
  for nc in resouces:
      # get timestapms
      try: 
        dt =  utils.get_time(nc) # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
        ds=Dataset(nc)
        data = np.squeeze(ds.variables[variable][:])
        meanData = np.mean(data,axis=1)
        ts = np.mean(meanData,axis=1)
        fig.line( dt,ts )
      except Exception as e:
        logger.exception('bokeh lineplot failed for %s: %s\n' % (nc, e))

      # plot into current figure
       # , legend= nc 
  
  logger.debug('timesseries lineplot done.') 
  #fig.legend()[0].orientation = "bottom_left"
  # fig.legend().orientation = "bottom_left"
  fig.title = title
  fig.grid# .grid_line_alpha=0.3

  window_size = 30
  window = np.ones(window_size)/float(window_size)
  bplt.save(fig, filename=output_html)
  #bplt.hold('off')
  
  logger.debug('plott attributes set.')
  return output_html  

  
  
  
  
