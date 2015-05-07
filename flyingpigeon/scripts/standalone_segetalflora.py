# calculation of number of segetal flora species

from os import environ, mkdir
from os.path import join
import tempfile
# import shutil

from cdo import *
cdo = Cdo()

# birdhouse WPS must be running (make start # in the toplevel of one of the birds)
# export PYTHONPATH=$HOME/birdhouse/flyingpigeon/flyingpigeon/
from flyingpigeon import segetalflora as sg
from flyingpigeon import clipping
     
climate_type = ['1','2','3','4','5','6','7','all']
culture_type = ['fallow', 'extensiv', 'intensiv', 'all']
HOME = environ['HOME']
nc  = join(HOME , '.conda/envs/birdhouse/var/cache/pywps/tas_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20210101-20251231.nc')
calc = [{'func':'mean','name':'tas'}]
calc_grouping = ['year']

# create temp dir
mkdir(os.path.curdir+'/dir_tas/')
mkdir(os.path.curdir+'/dir_polygons/')
mkdir(os.path.curdir+'/dir_timeseries/')
mkdir(join(dir_polygons,'EUR/')
dir_tas = (os.path.curdir+'/dir_tas/')
dir_polygons = (os.path.curdir+'/dir_polygons/')
dir_timeseries = (os.path.curdir+'/dir_timeseries/')

# prepare input files, Europe clipping and concatination 
europe_yearsum = clipping.clip_continent(urls=nc, calc=calc,calc_grouping= calc_grouping,
                                         prefix='tas_Europe_year', continent='Europe', dir_output=dir_tas)


cdo.expr(eq , input=europe_yearsum, output=join(dir_polygons,'EUR/', out))

EUR_seglo = clipping.clip_counties_EUR(urls=out, prefix='tas_FRA_year', dir_output = dir_polygons, country='FRA')

fldmean = timeseries.fldmean(EUR_seglo, dir_output = dir_timeseries)

