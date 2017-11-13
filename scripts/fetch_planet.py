from shapely.geometry import mapping
from ocgis import GeomCabinetIterator
import json
from flyingpigeon import config

shp = '/home/nils/data/transfrontalier/transfrontalier.shp'
gci = GeomCabinetIterator(path=shp)

for row in gci:
    gjson = mapping(row['geom'])
    aoi = json.dumps(gjson)
    print(aoi)
    break

#
#
# aoi = {
#   "type": "Polygon",
#   "coordinates": [
#     [
#       [-122.54, 37.81],
#       [-122.38, 37.84],
#       [-122.35, 37.71],
#       [-122.53, 37.70],
#       [-122.54, 37.81]
#     ]
#   ]
# }
