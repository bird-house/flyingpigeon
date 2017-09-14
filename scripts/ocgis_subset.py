from os import path, listdir
import ocgis

from flyingpigeon import subset
from flyingpigeon import utils
from flyingpigeon.ocgis_module import call

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
