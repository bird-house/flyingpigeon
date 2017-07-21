
from os import listdir
from os import path

from flyingpigeon import sdm


p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/MPI-CSC/MPI-M-MPI-ESM-LR/historical/r1i1p1/MPI-CSC-REMO2009/v1/day/tas/v20160412/'

ncs = [path.join(p, nc) for nc in listdir(p)]
ncs.sort()

indices = sdm._SDMINDICES_

in_nc = sdm.get_indices(ncs, indices=['TG_AMJJAS'])

print fp_indice
