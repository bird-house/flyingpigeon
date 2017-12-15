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


size = 1000
for key in products.keys():
    if size > float(products[key]['size'].split(' ')[0]) and 50 < float(products[key]['size'].split(' ')[0]):
        size = float(products[key]['size'].split(' ')[0])
        test_file = products[key]

api.download('2e673c77-bb1c-4061-a16d-2db4f647d9bc', directory_path='/home/nils/data/')

import zipfile
zip_ref = zipfile.ZipFile('/home/nils/data/S2A_MSIL1C_20171205T092341_N0206_R093_T33PVL_20171205T130029.zip', 'r')
zip_ref.extractall('/home/nils/data/')
zip_ref.close()
history
