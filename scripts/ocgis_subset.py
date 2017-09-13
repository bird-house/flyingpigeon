from os import path, listdir
from flyingpigeon import subset
from flyingpigeon import utils


p = "/home/nils/data/AFR-44/tas/"
ncs = [path.join(p, nc) for nc in listdir(p)]
ncd = utils.sort_by_filename(ncs)
geom = subset.get_geom('CMR')
ugid = subset.get_ugid('CMR', geom=geom)


from ocgis import RequestDataset, OcgOperations

keys = ncd.keys()


OcgOperations?

rd = RequestDataset(ncd[keys[0]][0])
geos = OcgOperations(rd, geom=geom, select_ugid=ugid, output_format='nc').execute()

ncd[keys[0]]

rd = RequestDataset(ncd[keys[0]])
geos = OcgOperations(rd, geom=geom, select_ugid=ugid, output_format='nc').execute()
