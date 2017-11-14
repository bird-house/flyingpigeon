# environemtal variable needs to be set
# export PL_API_KEY=44620741f94a4c7d88f48263*******


from planet import api
import sys


client = api.ClientV1()
aoi = {
  "type": "Polygon",
  "coordinates": [
    [
      [-122.54, 37.81],
      [-122.38, 37.84],
      [-122.35, 37.71],
      [-122.53, 37.70],
      [-122.54, 37.81]
    ]
  ]
}

# build a filter for the AOI
query = api.filters.and_filter(
  api.filters.geom_filter(aoi)
)
item_types = ['PSScene4Band']
request = api.filters.build_search_request(query, item_types)
results = client.quick_search(request)

for item in results.items_iter(100):
    # each item is a GeoJSON feature
    sys.stdout.write('%s\n' % item['id'])


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


aoi = {
  "type": "Polygon",
  "coordinates": [
    [
      [-122.54, 37.81],
      [-122.38, 37.84],
      [-122.35, 37.71],
      [-122.53, 37.70],
      [-122.54, 37.81]
    ]
  ]
}
