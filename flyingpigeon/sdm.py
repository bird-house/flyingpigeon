import logging
logger = logging.getLogger(__name__)


def get_csv(zip_file_url):
  
  import requests, zipfile, StringIO
  r = requests.get(zip_file_url, stream=True)
  z = zipfile.ZipFile(StringIO.StringIO(r.content))
  z.extractall()  
  csv = z.namelist()[0]
 
  return csv

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

def get_PApoints(coordinates, domain='EUR-11'):
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
    logger.error('domain not found')
  
  ds = Dataset(nc, mode='r')
  lats = ds.variables['lat']
  lons = ds.variables['lon']
  sftlf = np.array(ds.variables['sftlf'])
  
  domain = lats.shape
  
  lats1D = np.array(lats).ravel()
  lons1D = np.array(lons).ravel()
  tree = spatial.KDTree(zip(lats1D,lons1D))
  l, i = tree.query(points)
  
  PA = np.zeros(len(lats1D)) 
  PA[i] = 1
  PAmask = PA.reshape(domain)
  
  PAmask[sftlf<=50] = np.nan
  return PAmask

def get_indices(resources, indices):
  from flyingpigeon.utils import sort_by_filename, calc_grouping
  from flyingpigeon.ocgis_module import call
  from flyingpigeon.indices import indice_variable
  
  ncs = sort_by_filename(resources, historical_concatination=True)
  ncs_indices = []
  logger.info('resources sorted found %s datasets' % len(ncs.keys()) ) 
  for key in ncs.keys():
    for indice in indices:
      try: 
        name , month = indice.split('_')
        variable=key.split('_')[0]
        #print name 
        if variable == indice_variable(name):
          
          logger.info('calculating indice %s ' % indice)
          grouping = calc_grouping(month)
          calc = [{'func' : 'icclim_' + name, 'name' : name}]
          prefix=key.replace(variable, name).replace('_day_','_%s_' % month)
           
          nc = call(resource=ncs[key], variable=variable, calc=calc, calc_grouping=grouping, prefix=prefix, memory_limit=500 )
          ncs_indices.append(nc)
          logger.info('Successful calculated indice %s %s' % (key, indice))
      except Exception as e: 
        print e# logger.error('failed to calculate indice %s %s %s ' % (key, indice, e))    
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

def get_reference(ncs_indices, refperiod='1998-2000'):
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
  
  if not refperiod == 'all':
    s, e = refperiod.split('-')
    start = dt.strptime(s+'-01-01', '%Y-%m-%d')
    end = dt.strptime(e+'-12-31', '%Y-%m-%d')
    time_range=[start, end]
  else:
    time_range=None
    
  ref_indices = []
  for nc_indice in ncs_indices: 
    variable = get_variable(nc_indice)
    f = basename(nc_indice).strip('.nc')
    prefix = '%s_ref-%s' % ('_'.join(f.split('_')[0:-1]), refperiod) 
    
    ref_indices.append(call(resource=nc_indice, variable=variable,prefix=prefix, calc=[{'func':'mean','name': variable}],calc_grouping=['all'],time_range=time_range))
  
  return ncs_reference


def get_gam(ncs_reference, PAmask):
  
  from netCDF4 import Dataset
  from os.path import basename
  from numpy import squeeze, ravel, isnan, nan
  from flyingpigeon.utils import get_variable
  
  from rpy2.robjects.packages import importr
  import rpy2.robjects as ro

  import rpy2.robjects.numpy2ri
  rpy2.robjects.numpy2ri.activate()
  mgcv = importr("mgcv")
  base = importr("base")
  stats = importr("stats")
  
  data = {'PA': ro.FloatVector(ravel(PAmask))}
  
  form = 'PA ~ '
  
  for i , nc in enumerate(ncs_reference):
    var = get_variable(nc)
    agg = basename(nc).split('_')[-2]
    ds = Dataset(nc)
    vals = squeeze(ds.variables[var])
    vals[isnan(PAmask)] = nan 
    indice = '%s_%s' % (var, agg)
    data[indice] = ro.FloatVector(ravel(vals))
    if i == 0:
      form = form + 's(%s, k=3)' % indice 
    else: 
      form = form + ' + s(%s, k=3)' % indice
  
  dataf = ro.DataFrame(data)
  eq = ro.Formula(str(form))
  
  gam_model = mgcv.gam(base.eval(eq), data=dataf, family=stats.binomial(), scale=-1)
  
  return gam_model

