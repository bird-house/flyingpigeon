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
      latlon[i][0] = float(columns['decimallongitude'][i])
      latlon[i][1] = float(columns['decimallatitude'][i])
    except Exception as e: 
      c = c +1 
  logger.info('failed to read in PA coordinates for %s rows ' % c)
  
  nz = (latlon == 0).sum(1)
  ll = latlon[nz == 0, :]    
  
  logger.info('read in PA coordinates for %s rows ' % len(ll[:,0]))
  
  return ll 