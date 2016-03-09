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


def get_refindices(resources, indices, period):
  
  from flyingpigeon.utils import sort_by_filename 
  from flyingpigeon.ocgis_module import call
  
  ncs = sort_by_filename(resources, historical_concatination=True)
  nc_indices = []
  
  for key in ncs.keys():
    for indice in indices:
      try: 
        icclim, month = indice.split('_') 
        calc = [{'func' : 'icclim_' + icclim, 'name' : indice}]
        variable='tas'
        nc = call(resource=ncs[key], variable=variable, calc=calc, calc_grouping=['all'], prefix=key.replace(variable, indice), time_region=None, time_range=None, output_format='nc')
        nc_indices = nc # nc_indices.append(nc)
        logger.info('Successful calculated indice %s %s' % (key, indice))
      except Exception as e: 
        logger.error('failed to calculate indice %s %s %s ' % (key, indice, e))
        
  return nc_indices