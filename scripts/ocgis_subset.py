from os import path, listdir
import ocgis

from flyingpigeon import subset
from flyingpigeon import utils
from flyingpigeon.ocgis_module import call


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






p = "/home/nils/data/AFR-44/tas/"
ncs = [path.join(p, nc) for nc in listdir(p)]
ncd = utils.sort_by_filename(ncs)
geom = subset.get_geom('CMR')
ugid = subset.get_ugid('CMR', geom=geom)

# from ocgis import RequestDataset, OcgOperations

keys = ncd.keys()
print len(keys)

ocgis.env.OVERWRITE = True

dmap = ocgis.DimensionMap()
dmap.set_variable('x', 'lon', dimension='rlon')
dmap.set_variable('y', 'lat', dimension='rlat')
dmap.set_variable('time', 'time', dimension='time')

#
# print dmap
# rd = ocgis.RequestDataset(ncd[keys[0]][0], crs=ocgis.crs.Spherical(), )
# geos = ocgis.OcgOperations(rd, geom=geom, select_ugid=ugid, output_format='nc', prefix='one_file').execute()
# geos

for key in ncd.keys():
    # rd = ocgis.RequestDataset(ncd[key], crs=ocgis.crs.Spherical(), dimension_map=dmap)
    # geos = ocgis.OcgOperations(rd,
    #                            geom=geom, select_ugid=ugid,
    #                            output_format='nc',
    #                            prefix=key,
    #                            add_auxiliary_files=False).execute()
    geos = call(ncd[key], geom=geom, select_ugid=ugid, output_format='nc', prefix=key,
                variable='tas', crs=ocgis.crs.Spherical(), dimension_map=dmap)
    print geos

#
# rd = RequestDataset(ncd[keys[0]][0])
# geos = OcgOperations(rd, geom=geom, select_ugid=ugid, output_format='nc').execute()
#
# ncd[keys[0]]
#
# rd = RequestDataset(ncd[keys[0]])
#
# geos = OcgOperations(rd, geom=geom, select_ugid=ugid, output_format='nc').execute()
