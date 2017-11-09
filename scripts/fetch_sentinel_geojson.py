# based on  https://pypi.python.org/pypi/sentinelsat
# http://sentinelsat.readthedocs.io/en/stable/api.html

from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt

api = SentinelAPI('info@nilshempelmann.de', '*****')   0Zd0If0ed2G

geom = {"type": "Polygon", "coordinates": [[[-69.87682044199994, 12.427394924000097], [-70.05809485599988, 12.537176825000088], [-70.04873613199993, 12.632147528000104], [-69.93639075399994, 12.53172435100005], [-69.87682044199994, 12.427394924000097]]]}

footprint = geojson_to_wkt(read_geojson('search_polygon.geojson'))
products = api.query(footprint,
                     producttype='SLC',
                     orbitdirection='ASCENDING')
api.download_all(products)

from shapely.geometry import mapping
from ocgis import GeomCabinetIterator
import json
from flyingpigeon import config

gci = GeomCabinetIterator(path=join(sh_path, 'countries.shp'))

for row in gci:
    gjson = mapping(row['geom'])
    dump = json.dumps(gjson)
    print(dump)
    break


        env.DIR_SHPCABINET = config.shapefiles_path()
        sc_iter = ShpCabinetIterator(geom)
        result = []

        if geom == 'countries':
            for row in sc_iter:
                for polygon in polygons:
                    if row['properties']['ADM0_A3'] == polygon:
                        result.append(row['properties']['UGID'])


from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt

api = SentinelAPI('user', 'password')
geom = {"type": "Polygon", "coordinates": [[[-69.87682044199994, 12.427394924000097], [-70.05809485599988, 12.537176825000088], [-70.04873613199993, 12.632147528000104], [-69.93639075399994, 12.53172435100005], [-69.87682044199994, 12.427394924000097]]]}

footprint = geojson_to_wkt(geom)
