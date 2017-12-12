# based on  https://pypi.python.org/pypi/sentinelsat
# http://sentinelsat.readthedocs.io/en/stable/api.html

from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt

from shapely.geometry import mapping
from ocgis import GeomCabinetIterator
import json
from flyingpigeon import config

from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt

api = SentinelAPI('nilshempelmann', '*****')

geom = {
  "type": "Polygon",
  "coordinates": [
          [
            [
              14.00,
              8.00
            ],
            [
              16.00,
              8.00
            ],
            [
              16.00,
              10.00
            ],
            [
              14.00,
              10.00
            ],
            [
              14.00,
              8.00
            ]
          ]
        ]
}

#
geom = {
  "type": "Polygon",
  "coordinates": [
          [
            [
              14.600830078125,
              8.677421123289992
            ],
            [
              14.797210693359375,
              8.677421123289992
            ],
            [
              14.797210693359375,
              8.90678000752024
            ],
            [
              14.600830078125,
              8.90678000752024
            ],
            [
              14.600830078125,
              8.677421123289992
            ]
          ]
        ]
}


# geom = {"type": "Polygon", "coordinates": [[[-69.87682044199994, 12.427394924000097], [-70.05809485599988, 12.537176825000088], [-70.04873613199993, 12.632147528000104], [-69.93639075399994, 12.53172435100005], [-69.87682044199994, 12.427394924000097]]]}
# footprint = geojson_to_wkt(read_geojson('search_polygon.geojson'))

footprint = geojson_to_wkt(geom)

from datetime import datetime as dt
from datetime import timedelta
end = dt.now()
start = dt.now() - timedelta(days=30)
period = (start, end)


products = api.query(footprint,
                     date = (start, end),
                     platformname = 'Sentinel-2',
                     cloudcoverpercentage = (0, 30))
for key in products.keys()
    products[key]
    api.get_product_odata(key, full=True)
    api.download(key, directory_path='/home/nils/data/')



# products = api.query(footprint,
#                      date=period,
#                      # producttype='SLC',
#                      orbitdirection='ASCENDING',
#                      platformname='Sentinel-2')
#
# api.download_all(products)

#
# /home/nils/data/transfrontalier/transfrontalier.shp
# gci = GeomCabinetIterator(path=join(sh_path, 'countries.shp'))
#
# for row in gci:
#     gjson = mapping(row['geom'])
#     dump = json.dumps(gjson)
#     print(dump)
#     break
#
#
#         env.DIR_SHPCABINET = config.shapefiles_path()
#         sc_iter = ShpCabinetIterator(geom)
#         result = []
#
#         if geom == 'countries':
#             for row in sc_iter:
#                 for polygon in polygons:
#                     if row['properties']['ADM0_A3'] == polygon:
#                         result.append(row['properties']['UGID'])
#
#
# api = SentinelAPI('user', 'password')
# geom = {"type": "Polygon", "coordinates": [[[-69.87682044199994, 12.427394924000097], [-70.05809485599988, 12.537176825000088], [-70.04873613199993, 12.632147528000104], [-69.93639075399994, 12.53172435100005], [-69.87682044199994, 12.427394924000097]]]}
#
# footprint = geojson_to_wkt(geom)
