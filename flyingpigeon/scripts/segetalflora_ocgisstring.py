from os import environ , path
from flyingpigeon import segetalflora as sf 

HOME = environ['HOME']
CACHE = '.conda/envs/birdhouse/var/cache/pywps'
nc_in = 'tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc'

nc_path = path.join(HOME,CACHE,nc_in)

d1, d2, d3, = sf.get_segetalflora(nc_path, dir_tas='dir_tas' , dir_segetalflora='dir_segetalflora', dir_fieldmean='dir_fieldmean')

print '%s %s %s ' % (d1, d2, d3 )