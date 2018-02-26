import request

from ocgis import RequestDataset, OcgOperations, env
from ocgis.util.large_array import compute

from datetime import datetime as dt
import uuid

# years = range(1948,2018)
years = range(2007, 2017)

ncs = []
for year in years:
    url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.%s.nc' % (year)
    ncs.extend(requests.get(url, stream=True, verify=False))


### test 1 -- just subset
time_range = [dt.strptime('20100315', '%Y%m%d'), dt.strptime('20111210', '%Y%m%d')]
bbox = [-80, 20, 20, 70]
rd = RequestDataset(ncs)
ops = OcgOperations(rd,
                    time_range=time_range,
                    geom=bbox,
                    output_format='nc',
                    prefix='ocgis_freememory_test1',
                    add_auxiliary_files=False)

# ############################
# check free memory available somehow

# free_memory = ...

# ###########################
# check required memory space
# requ_memory

# copmpare
# if requ_memory < free_memory/2:

shnip = dt.now()

geom = ops.execute()

shnap = dt.now()

print 'operation performed with execute in %s sec' % (shnap - shnip).total_seconds()
print geom

########################
# calculate tile dimension:

# here comes a nice tile calculation:
# tile_dimension = ...
tile_dimension = 5  # tile_dimension

shnip = dt.now()
geom = compute(ops, tile_dimension=tile_dimension, verbose=True)
shnap = dt.now()

print 'operation performed with compute in  %s sec' % (shnip - shnap).total_seconds()
print geom
