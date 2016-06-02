import logging
logger = logging.getLogger(__name__)


_SDMINDICES_ = [
  'CDD_AMJJAS',
  'CFD_ONDJFM',
  'FD_ONDJFM','FD_April', 'FD_May', 'FD_June',
  'GD4_yr',
  'ID_yr',
  'PRCPTOT_yr','PRCPTOT_ONDJFM', 'PRCPTOT_AMJJAS', 'PRCPTOT_JJA', 'PRCPTOT_MAM', 'PRCPTOT_JJA',
  'RR1_yr',
  'SU_yr',
  'TG_yr', 'TG_AMJJAS', 'TG_ONDJFM','TG_JJA', 
  'TNn_yr','TNn_AMJJAS','TG_ONDJFM','TNn_Jan', 
  ]

def get_csv(zip_file_url):
  
  import requests, zipfile, StringIO
  r = requests.get(zip_file_url, stream=True)
  z = zipfile.ZipFile(StringIO.StringIO(r.content))
  z.extractall()  
  csv = z.namelist()[0]
  return csv

def gbif_serach(taxon_name): 
  from pygbif import species, occurrences
  from numpy import nan, empty
  #TName = "Fagus sylvatica"
  try:
    key = species.name_backbone(name=taxon_name, rank="species")["usageKey"]
    n = occurrences.count(taxonKey=key, isGeoreferenced=True)

    if n > 300:
        max = 300
    else:
        max = n
        
    results = occurrences.search(taxonKey=key, limit=max)
    logger.info('(', key, ')', '-', format(n, ','), " ocurrence(s)")

    latlon = empty([max,2], dtype=float, order='C')

    for i, x in enumerate(results["results"]):
      try:
        Latitude = (x['decimalLatitude'])
        #if Latitude == 0.0:
          #Latitude = nan
      except:
        Latitude = nan
      try:
        Longitude = (x['decimalLongitude'])
        #if Longitude == 0.0:
          #Longitude = nan
      except:
        Longitude = nan
      
      latlon[i][0] = Latitude  
      latlon[i][1] = Longitude
  
    nz = (latlon == 0).sum(1)
    ll = latlon[nz == 0, :]
    logger.info('read in PA coordinates for %s rows ' % len(ll[:,0]))          
  except Exception as e: 
    logger.exception('failed search GBIF data %s' % (e))
  return ll


def get_latlon( csv_file ):
  import csv 
  from collections import defaultdict
  from numpy import empty
  columns = defaultdict(list)
  
  with open(csv_file, 'rb') as f:
    reader = csv.DictReader(f, delimiter='\t')
    for row in reader:
        for (k,v) in row.items():
            columns[k].append(v)
            
  l = len(columns['decimallongitude'])           
  
  latlon = empty([l,2], dtype=float, order='C')
  
  c = 0
  for i in range(0,l):
    try:
      latlon[i][0] = float(columns['decimallatitude'][i])
      latlon[i][1] = float(columns['decimallongitude'][i])
    except Exception as e: 
      c = c +1 
  logger.info('failed to read in PA coordinates for %s rows ' % c)
  
  nz = (latlon == 0).sum(1)
  ll = latlon[nz == 0, :]    
  
  logger.info('read in PA coordinates for %s rows ' % len(ll[:,0]))
  
  return ll

def get_PAmask(coordinates=[], domain='EUR-11'):
  """
  generates a matrix with 1/0 values over land areas. (nan for water regions)
  
  :param coordinates: 2D array with lat lon coordinates representing tree observation
  :param domain: region (default='EUR-11') 
  """
  from scipy import spatial
  import numpy as np
  from netCDF4 import Dataset
  from flyingpigeon import config
  DIR_MASKS = config.masks_dir()
  
  if domain=='EUR-11': 
    nc = DIR_MASKS + '/sftlf_EUR-11_ECMWF-ERAINT_evaluation_r1i1p1_KNMI-RACMO22E_v1_fx.nc'
  else: 
    logger.debug('domain not found')
  
  ds = Dataset(nc, mode='r')
  lats = ds.variables['lat']
  lons = ds.variables['lon']
  sftlf = np.array(ds.variables['sftlf'])
  
  domain = lats.shape
  
  lats1D = np.array(lats).ravel()
  lons1D = np.array(lons).ravel()
  tree = spatial.KDTree(zip(lats1D,lons1D))
  l, i = tree.query(coordinates)
  
  PA = np.zeros(len(lats1D)) 
  PA[i] = 1
  PAmask = PA.reshape(domain)
  
  PAmask[sftlf<=50] = np.nan
  return PAmask

def get_indices(resources, indices):
  from flyingpigeon.utils import sort_by_filename, calc_grouping, drs_filename
  from flyingpigeon.ocgis_module import call
  from flyingpigeon.indices import indice_variable, calc_indice_single

  #names = [drs_filename(nc, skip_timestamp=False, skip_format=False, 
  #               variable=None, rename_file=True, add_file_path=True) for nc in resources]
  
  ncs = sort_by_filename(resources, historical_concatination=True)
  ncs_indices = []
  logger.info('resources sorted found %s datasets' % len(ncs.keys()) ) 
  for key in ncs.keys():
    for indice in indices:
      try: 
        name , month = indice.split('_')
        variable=key.split('_')[0]
        # print name, month , variable 
        if variable == indice_variable(name):
          logger.info('calculating indice %s ' % indice)
          prefix=key.replace(variable, name).replace('_day_','_%s_' % month)
          nc = calc_indice_single(resource=ncs[key], variable=variable, prefix=prefix, indices=name,  groupings=month, memory_limit=500)
          
          #grouping = calc_grouping(month)
          #calc = [{'func' : 'icclim_' + name, 'name' : name}] 
          #nc = call(resource=ncs[key], variable=variable, calc=calc, calc_grouping=grouping, prefix=prefix , memory_limit=500) #memory_limit=500
          
          ncs_indices.append(nc[0])
          logger.info('Successful calculated indice %s %s' % (key, indice))
      except Exception as e: 
        logger.exception('failed to calculate indice %s %s' % (key, indice))    
  return ncs_indices

def sort_indices(ncs_indices):
  """
  groups the defining growing conditions indices per dataset
  :param ncs_indices: list of climate indices
  :return dictionary: {'dataset' = [filepathlist]}
  """
  from os.path import basename 
  indices_dic = {}

  for indice in ncs_indices: 
    f = basename(indice).strip('.nc')
    name = '_'.join(f.split('_')[1:-2])
    indices_dic[name] = []
      
  for key in indices_dic.keys():
    for path in ncs_indices: 
      if key in path:
        indices_dic[key].append(path)
 
  return indices_dic

def get_reference(ncs_indices, period='all'):
  """
  calculates the netCDF files containing the mean climatology for statistical GAM training
  :param ncs_indices: list of climate indices defining the growing conditions of tree species
  :param refperiod: time period for statistic training 
  :return present: present conditions
  """
  from datetime import datetime as dt
  from flyingpigeon.ocgis_module import call
  from flyingpigeon.utils import get_variable
  from os.path import basename
  
  if not period == 'all':
    s, e = period.split('-')
    start = dt.strptime(s+'-01-01', '%Y-%m-%d')
    end = dt.strptime(e+'-12-31', '%Y-%m-%d')
    time_range=[start, end]
  else:
    time_range=None
    
  ref_indices = []
  for nc_indice in ncs_indices: 
    variable = get_variable(nc_indice)
    f = basename(nc_indice).strip('.nc')
    prefix = '%s_ref-%s' % ('_'.join(f.split('_')[0:-1]), period) 
    
    ref_indices.append(call(resource=nc_indice, variable=variable,prefix=prefix, calc=[{'func':'mean','name': variable}],calc_grouping=['all'],time_range=time_range))
  
  return ref_indices


def get_gam(ncs_reference, PAmask):
  
  from netCDF4 import Dataset
  from os.path import basename
  from numpy import squeeze, ravel, isnan, nan, array, reshape
  
  from flyingpigeon.utils import get_variable
  

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
  
  try: 
    data = {'PA': ro.FloatVector(ravel(PAmask))}
    domain = PAmask.shape
    logger.info('mask data converted to R float vector')
  except Exception as e: 
    msg = 'failed to convert mask to R vector'
  
  form = 'PA ~ '
  ncs_reference.sort()
  
  try:
    for i , nc in enumerate(ncs_reference):
      var = get_variable(nc)
      agg = basename(nc).split('_')[-2]
      ds = Dataset(nc)
      vals = squeeze(ds.variables[var])
      vals[vals > 1000 ] = 0 
      vals[isnan(PAmask)] = nan 
      indice = '%s_%s' % (var, agg)
      data[str(indice)] = ro.FloatVector(ravel(vals))
      if i == 0:
        form = form + 's(%s, k=3)' % indice 
      else: 
        form = form + ' + s(%s, k=3)' % indice
  except Exception as e:
    logger.debug('form string generation for gam failed')
  
  dataf = ro.DataFrame(data)
  eq = ro.Formula(str(form))
  
  gam_model = mgcv.gam(base.eval(eq), data=dataf, family=stats.binomial(), scale=-1, na_action=stats.na_exclude) # 
  
  grdevices = importr('grDevices')
  
  ### ###########################
  # plot response curves
  ### ###########################

  from flyingpigeon.visualisation import concat_images
  from tempfile import mkstemp
  infos = []

  for i in range(1,len(ncs_reference)+1):
    #ip, info =  mkstemp(dir='.',suffix='.pdf')
    ip, info =  mkstemp(dir='.',suffix='.png')
    infos.append(info)
    grdevices.png(filename=info)
    #grdevices.pdf(filename=info)
    #ylim = ro.IntVector([-6,6])
    trans = ro.r('function(x){exp(x)/(1+exp(x))}')
    mgcv.plot_gam(gam_model, trans=trans, shade='T', col='black',select=i,ylab='Predicted Probability',rug=False , cex_lab = 1.4, cex_axis = 1.4, ) #
    #ylim=ylim,  ,
    grdevices.dev_off()
    
  infos_concat = concat_images(infos, orientation='h')
  predict_gam = mgcv.predict_gam(gam_model, type="response", progress="text", na_action=stats.na_exclude) #, 
  prediction = array(predict_gam).reshape(domain)
    
  return gam_model, prediction, infos_concat


def get_prediction(gam_model, ncs_indices ): #mask=None
  """
  predict the probabillity based on the gam_model and the given climate indice datasets
  
  :param gam_model: fitted gam (output from sdm.get_gam)
  :pram nsc_indices: list of netCDF files containing climate indices of one dataset
  :param mask: 2D array of True/False to exclude areas (e.g ocean) for prediction
  """
  from netCDF4 import Dataset
  from os.path import basename
  from numpy import squeeze, ravel, array, reshape#, zeros, broadcast_arrays, nan
  
  from flyingpigeon.utils import get_variable
  
  from rpy2.robjects.packages import importr
  import rpy2.robjects as ro

  import rpy2.robjects.numpy2ri
  rpy2.robjects.numpy2ri.activate()
  mgcv = importr("mgcv")
  stats = importr("stats")

  ncs_indices.sort()
  
  data = {}
  
  for i , nc in enumerate(ncs_indices):
    var = get_variable(nc)
    agg = basename(nc).split('_')[-2]
    ds = Dataset(nc)
    vals = squeeze(ds.variables[var])
    if i == 0:
      dims = vals.shape
    #if mask != None: 
      #mask = broadcast_arrays(vals, mask)[1]
      #vals[mask==False] = nan
    indice = '%s_%s' % (var, agg)
    data[str(indice)] = ro.FloatVector(ravel(vals))
  
  dataf = ro.DataFrame(data)
  predict_gam = mgcv.predict_gam(gam_model, newdata=dataf, type="response", progress="text", newdata_guaranteed = True) #, na_action=`na.pass`
  
  prediction = array(predict_gam).reshape(dims)
  
  return prediction


def write_to_file(nc_indice, data):
  from netCDF4 import Dataset
  from shutil import copy
  from os.path import split, join
  from flyingpigeon.utils import get_variable
  from flyingpigeon.metadata import get_frequency 

  #path, nc_indice = split(indice_file)

  var = get_variable(nc_indice)
  fq = get_frequency(nc_indice)
  agg = nc_indice.split('_')[-2]

  nc = nc_indice.replace(var,'tree').replace(agg,fq)
  copy(nc_indice, nc)
  
  ds = Dataset(nc, mode= 'a')
  vals = ds.variables[var]
  
  ds.renameVariable(var,'tree') 
  
  vals[:,:,:] = data[:,:,:]
  vals.long_name = 'Favourabilliy for tree species'
  vals.standard_name = 'tree'
  vals.units = '0-1'
  ds.close()                  
  return nc                
