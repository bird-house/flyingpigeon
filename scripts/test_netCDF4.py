from os import listdir
from os import path

from netCDF4 import MFDataset
from netCDF4 import Dataset

# p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/MPI-CSC/MPI-M-MPI-ESM-LR/historical/r1i1p1/MPI-CSC-REMO2009/v1/day/tas/v20160412/'
p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/CLMcom/CNRM-CERFACS-CNRM-CM5/historical/r1i1p1/CLMcom-CCLM4-8-17/v1/day/tas/v20140401/'

resource = [path.join(p, nc) for nc in listdir(p)]
resource.sort()

print len(resource)

for nc in resource:
    ds = Dataset(nc)
    print '*** success'
    ds.close()

mfd = MFDataset(resource)

print resource

print 'mfd sucess as well'
