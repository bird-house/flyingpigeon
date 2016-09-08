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
          
    data[str(indice)] = ro.FloatVector(vals)
    
    if i == 0 :
      from numpy import zeros, ones
      a = append (zeros(len(vals)) , ones(len(vals)) )
      PA = append(a , zeros(len(vals)))
      data = {'PA': ro.FloatVector(PA)}
      form = 'PA ~ '
      form = form + 's(%s, k=3)' % indice 
    else: 
      form = form + ' + s(%s, k=3)' % indice

  dataf = ro.DataFrame(data)
  eq = ro.Formula(str(form))
  gam_model = mgcv.gam(base.eval(eq), data=dataf, family=stats.binomial(), scale=-1, na_action=stats.na_exclude) # 
  grdevices = importr('grDevices')
  
  # ### ###########################
  # # plot response curves
  # ### ###########################

  # from flyingpigeon.visualisation import concat_images
  # from tempfile import mkstemp
  # infos = []

  # for i in range(1,len(ncs_reference)+1):
  #   #ip, info =  mkstemp(dir='.',suffix='.pdf')
  #   ip, info =  mkstemp(dir='.',suffix='.png')
  #   infos.append(info)
  #   grdevices.png(filename=info)
  #   #grdevices.pdf(filename=info)
  #   #ylim = ro.IntVector([-6,6])
  #   trans = ro.r('function(x){exp(x)/(1+exp(x))}')
  #   mgcv.plot_gam(gam_model, trans=trans, shade='T', col='black',select=i,ylab='Predicted Probability',rug=False , cex_lab = 1.4, cex_axis = 1.4, ) #
  #   #ylim=ylim,  ,
  #   grdevices.dev_off()
    
  # infos_concat = concat_images(infos, orientation='h')
  # predict_gam = mgcv.predict_gam(gam_model, type="response", progress="text", na_action=stats.na_exclude) #, 
  # prediction = array(predict_gam).reshape(domain)
    
  return timeseries, vals, gam_model 
