
from os import listdir
from os import path

from ocgis import RequestDataset, OcgOperations

p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/MPI-CSC/MPI-M-MPI-ESM-LR/historical/r1i1p1/MPI-CSC-REMO2009/v1/day/tas/v20160412/'

ncs = [path.join(p, nc) for nc in listdir(p)]
ncs.sort()
rd = RequestDataset(ncs[0])
indice = 'TG'
geom = OcgOperations(rd,
                     calc=[{'func': 'icclim_' + indice, 'name': indice}],
                     calc_grouping=['month'],
                     prefix='single_file',
                     output_format='nc').execute()
print geom

rd = RequestDataset(ncs)
indice = 'TG'
geom = OcgOperations(rd,
                     calc=[{'func': 'icclim_' + indice, 'name': indice}],
                     calc_grouping=['month'],
                     prefix='multi_file',
                     output_format='nc').execute()
print geom
