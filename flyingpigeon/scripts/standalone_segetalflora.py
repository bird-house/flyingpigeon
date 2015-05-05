# calculation of number of segetal flora species

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

nc  = '/homel/nhempel/anaconda/var/cache/pywps/tas_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20210101-20251231.nc'
out = '/homel/nhempel/data/cdotest.nc'

calc = [{'func':'mean','name':'tas'}]
calc_grouping = ['year']

# create temp dir
tmp_dir = tempfile.mkdtemp()
# prepare input files, Europe clipping and concatination 
europe_yearsum = clipping.clip_continent(urls=nc, calc=calc,calc_grouping= calc_grouping, 
                                         prefix='tas_Europe_year', continent='Europe',  dir_output=tmp_dir)

p1, tmp1 = tempfile.mkstemp( suffix='.nc')
cdo.yearmean (input = nc , output = tmp1)

nc_eur = clipping( '%s' % (tmp1) , tmp_dir)
cdo.expr(eq , input=tmp1, output=out)

os.close( p1 )
shutil.rmtree(tmp_dir)
