#!/home/nils/.conda/envs/birdhouse/bin/python 

import os
from flyingpigeon import segetalflora as sf

from flyingpigeon import timeseries as ts

DIR_DATA = '/media/nils/Iomega HDD/data/EUR-11/tas'
DIR_OUT = '/media/nils/Iomega HDD/segetalflora'
dir_tas=DIR_OUT+'/tas'
dir_segetalflora=DIR_OUT+'/segetalflora'

ncs = [os.path.join(DIR_DATA,nc) for nc in os.listdir(DIR_DATA) if '.nc' in nc]
print len (ncs)

tas_yearmean = ts.get_yearmean(ncs, variable='tas', dir_output=dir_tas)


print tas_yearmean
