# from datetime import datetime as dt
from ocgis import RequestDataset, OcgOperations
from os import listdir
from os import path

p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/CLMcom/MPI-M-MPI-ESM-LR/historical/r1i1p1/CLMcom-CCLM4-8-17/v1/day/tas/v20140401/'
resource = [path.join(p, nc) for nc in listdir(p)]
resource

indice = 'TG'
percentile = 90
var = 'tas'
window_width = 5

# dt1 = dt(1970, 01, 01)
# dt2 = dt(2000, 12, 31)
# period = [dt1, dt2]  # we will calculate the indice for 10 years
#

rd = RequestDataset(resource, 'tas')

from ocgis.constants import DimensionMapKey
rd.dimension_map.set_bounds(DimensionMapKey.TIME, None)

kwds = {'percentile': percentile, 'window_width': window_width}
calc = [{'func': 'daily_perc', 'name': 'dp', 'kwds': kwds}]


ops = OcgOperations(dataset=rd,
                    calc=calc,
                    output_format='nc',
                    prefix='wholedomain'
                    ).execute()
print ops

from flyingpigeon.config import shapefiles_dir
from ocgis import env
from flyingpigeon.subset import get_geom, get_ugid

env.DIR_SHPCABINET = shapefiles_dir()
geom = get_geom('CMR')
ugid = get_ugid(polygons='CMR', geom=geom)

ops = OcgOperations(dataset=rd,
                    calc=calc,
                    geom=geom,
                    select_ugid=ugid,
                    output_format='nc',
                    prefix='polygonsubset'
                    ).execute()
print ops

from flyingpigeon import subset
ops = subset.clipping(resource=resource,
                      variable=None,
                      # dimension_map=None,
                      calc=calc,
                      output_format='nc',
                      # calc_grouping=None,
                      # time_range=None,
                      # time_region=None,
                      # historical_concatination=True,
                      prefix="clipping_call",
                      spatial_wrapping='wrap',
                      polygons='CMR',
                      mosaic=False,
                      dir_output=None,
                      memory_limit=None)

print ops
