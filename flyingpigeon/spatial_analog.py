import logging
logger = logging.getLogger(__name__)


def get_gam(ncs_indices, coordinate):
  
  from netCDF4 import Dataset
  from os.path import basename
  from shapely.geometry import Point
  from numpy import squeeze, ravel, isnan, nan, array, reshape
  
  from flyingpigeon.utils import get_variable, get_values, unrotate_pole
  from flyingpigeon.ocgis_module import call 

  try:
    from rpy2.robjects.packages import importr
    import rpy2.robjects as ro
    import rpy2.robjects.numpy2ri
    
    rpy2.robjects.numpy2ri.activate()    
    base = importr("base")
    stats = importr("stats")
    mgcv = importr("mgcv")
    logger.info('rpy2 modules imported')
  except Exception as e: 
    msg = 'failed to import rpy2 modules %s' % e
    logger.debug(msg)
    raise Exception(msg)

  for i, ncs in enumerate(ncs_indices):
    # ocgis need unrotated coordinates to extract points
    # unrotate_pole writes lats lons into the file. 
    # ACHTUNG: will fail if the data is stored on a file system with no write permissions 
    try: 
      lats, lons = unrotate_pole(ncs, write_to_file=True)
      point = Point(float(coordinate[0]), float(coordinate[1]))
      # get the values
      variable = get_variable(ncs)
      agg = basename(ncs).split('_')[-2]
      indice = '%s_%s' % (variable, agg)
      timeseries = call(resource=ncs, geom=point, select_nearest=True)
      ts = Dataset(timeseries)
      vals = squeeze(ts.variables[variable][:])
      from numpy import min, max, mean, append, zeros, ones
      dif = max(vals) - min(vals)
      a = append(vals - dif ,vals)
      vals = append(a, vals+dif)
      
      if i == 0 :
        from numpy import zeros, ones
        a = append (zeros(len(vals)) , ones(len(vals)) )
        PA = append(a , zeros(len(vals)))
        data = {'PA': ro.FloatVector(PA)}
        data[str(indice)] = ro.FloatVector(vals)
        form = 'PA ~ '
        form = form + 's(%s, k=3)' % indice 
      else: 
        form = form + ' + s(%s, k=3)' % indice
        data[str(indice)] = ro.FloatVector(vals)

    except Exception as e: 
      msg = 'Failed to prepare data %s' % e
      logger.debug(msg)

  try: 
      
    logger.info(data)  
    dataf = ro.DataFrame(data)
    eq = ro.Formula(str(form))
    gam_model = mgcv.gam(base.eval(eq), data=dataf, family=stats.binomial(), scale=-1, na_action=stats.na_exclude) # 
    logger.info('GAM model trained')
  except Exception as e: 
    msg = 'Failed to generate GAM model %s' % e
    logger.debug(msg)
  
  # ### ###########################
  # # plot response curves
  # ### ###########################
  try: 
    from flyingpigeon.visualisation import concat_images
    from tempfile import mkstemp
    grdevices = importr('grDevices')
    graphicDev = importr('Cairo')
    infos = []    
    for i in range(1,len(ncs_indices)+1):
      
      ip, info =  mkstemp(dir='.',suffix='.png')
      #grdevices.png(filename=info)
      #graphicDev.CairoPDF(info, width = 7, height = 7, pointsize = 12)
      graphicDev.CairoPNG(info, width = 640 , height = 480, pointsize = 12) # 640, 480) #,  pointsize = 12  width = 30, height = 30,
      print 'file opened!'
      
      infos.append(info)
      #grdevices.png(filename=info)
            
      ylim = ro.IntVector([-6,6])
      trans = ro.r('function(x){exp(x)/(1+exp(x))}')
      mgcv.plot_gam(gam_model, trans=trans, shade='T',
                    col='black',select=i,ylab='Predicted Probability',rug=False ,
                    cex_lab = 1.4, cex_axis = 1.4, ) #
      print 'gam plotted ;-)'
      #ylim=ylim,  ,
      grdevices.dev_off()
      #graphicDev.dev_off()
      #graphicDev.Cairo_onSave( dev_cur(), onSave=True )
      
    print(' %s plots generated ' % len(infos))
    infos_concat = concat_images(infos, orientation='h')
  except Exception as e: 
    msg = 'Failed to plot statistical graphic %s' % e
    logger.debug(msg)
    raise Exception(msg)
    
  return  gam_model, infos_concat 
