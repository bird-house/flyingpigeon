from flyingpigeon import sdm
from datetime import datetime as dt
from os.path import join
from os import getenv

from flyingpigeon.visualisation import map_gbifoccurrences
from flyingpigeon.utils import archiveextract
from flyingpigeon.visualisation import map_PAmask

tic = dt.now()

indices = '/home/nils/data/sdm/tmpt3aIHH.tar'

gbif = '/home/nils/data/sdm/acacia_albida.csv'

gbif_url = 'https://bovec.dkrz.de/download/wpsoutputs/flyingpigeon/392f1c34-b4d1-11e7-a589-109836a7cf3a/tmp95yvix.csv'
latlon = sdm.latlon_gbifcsv(csv)
latlon = sdm.latlon_gbifcsv(gbif)
latlon

occurence_map = map_gbifoccurrences(latlon)
occurence_map
ncs = archiveextract(indices)
ncs
history
indices_dic = sdm.sort_indices(ncs)
indices_dic
PAmask = sdm.get_PAmask(coordinates=latlon, nc=ncs[0])
PAmask
PAmask_pngs.extend([map_PAmask(PAmask)])
map_PAmask(PAmask)
map_PAmask(PAmask)
ncs_reference = sdm.get_reference(ncs_indices=ncs)
ncs_reference
gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask, modelname='TG_AFR-44_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1')
prediction = sdm.get_prediction(gam_model, ncs)
