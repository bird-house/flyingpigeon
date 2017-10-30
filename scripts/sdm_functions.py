from flyingpigeon import sdm
from datetime import datetime as dt
from os.path import join
from os import getenv

from flyingpigeon.visualisation import map_gbifoccurrences
from flyingpigeon.utils import archiveextract
from flyingpigeon.visualisation import map_PAmask


def get_prediction(gam_model, ncs_indices):  # mask=None
    """
    predict the probabillity based on the gam_model and the given climate index datasets

    :param gam_model: fitted gam (output from sdm.get_gam)
    :pram nsc_indices: list of netCDF files containing climate indices of one dataset
    :param mask: 2D array of True/False to exclude areas (e.g ocean) for prediction

    :return array: 3D array with prediction values
    """
    from netCDF4 import Dataset
    from os.path import basename
    from numpy import squeeze, ravel, array, reshape  # , zeros, broadcast_arrays, nan
    from flyingpigeon.utils import get_variable
    from rpy2.robjects.packages import importr
    import rpy2.robjects as ro

    import rpy2.robjects.numpy2ri
    rpy2.robjects.numpy2ri.activate()
    mgcv = importr("mgcv")
    stats = importr("stats")

    ncs_indices.sort()

    data = {}

    for i, nc in enumerate(ncs_indices):
        var = get_variable(nc)
        agg = basename(nc).split('_')[-2]
        ds = Dataset(nc)
        vals = squeeze(ds.variables[var])
        if i == 0:
            dims = vals.shape
        # if mask != None:
            # mask = broadcast_arrays(vals, mask)[1]
            # vals[mask==False] = nan
        indice = '%s_%s' % (var, agg)
        data[str(indice)] = ro.FloatVector(ravel(vals))

    dataf = ro.DataFrame(data)
    predict_gam = mgcv.predict_gam(gam_model, newdata=dataf,
                                   type="response", progress="text",
                                   newdata_guaranteed=True, na_action=stats.na_pass)
    prediction = array(predict_gam).reshape(dims)
    return prediction



tic = dt.now()

indices = '/home/nils/data/sdm/tmpTzDdv7.tar'

gbif = '/home/nils/data/sdm/acacia_albida.csv'

gbif_url = 'https://bovec.dkrz.de/download/wpsoutputs/flyingpigeon/392f1c34-b4d1-11e7-a589-109836a7cf3a/tmp95yvix.csv'
latlon = sdm.latlon_gbifcsv(gbif)
latlon

occurence_map = map_gbifoccurrences(latlon)
occurence_map
ncs = archiveextract(indices)
ncs
indices_dic = sdm.sort_indices(ncs)
indices_dic
PAmask = sdm.get_PAmask(coordinates=latlon, nc=ncs[0])
PAmask

# PAmask_pngs.extend([map_PAmask(PAmask)])

map_PAmask(PAmask)
ncs_reference = sdm.get_reference(ncs_indices=ncs)
ncs_reference
gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask, modelname='TG_AFR-44_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1')
prediction = get_prediction(gam_model, ncs)
