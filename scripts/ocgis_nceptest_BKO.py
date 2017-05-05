import os

import ocgis


# WD = '/media/benkoziol/Extra Drive 1/data/bekozi-work/i41-slp-mfdataset'
WD = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface'


fns = []
for fn in filter(lambda x: x.endswith('.nc'), os.listdir(WD)):
    fns.append(fn)
fns.sort()

print fns

from flyingpigeon.ocgis_module import call
paths = []
for fn in fns:
    paths.append(os.path.join(WD, fn))

ncs = ['/home/nils/birdhouse/flyingpigeon/scripts/4eb52546-29e2-11e7-825a-9cb6d0d3acd7.nc',
       '/home/nils/birdhouse/flyingpigeon/scripts/701522b8-29e2-11e7-825a-9cb6d0d3acd7.nc']

rd = ocgis.RequestDataset(ncs)
field = rd.get()

slp = field.variables['slp']
print slp.value.mean()
