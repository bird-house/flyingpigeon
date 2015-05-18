import ocgis
from os import environ , path

from flyingpigeon import segetalflora as sf 

HOME = environ['HOME']
CACHE = '.conda/envs/birdhouse/var/cache/pywps'
nc_in = 'tas_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_sem_200101-200510.nc'

nc_path = path.join(HOME,CACHE,nc_in)

sf.get_segetalflora(nc_path)

print config.cache_path()