import os
import json
import requests
from requests.auth import HTTPBasicAuth
from tempfile import mkstemp
import shutil
import time
from os.path import join
from os import path, mkdir

from datetime import datetime as dt
from datetime import datetime, timedelta


# Stockton, CA bounding box (created via geojson.io)
# geojson_geometry = {
#   "type": "Polygon",
#   "coordinates": [
#     [
#       [-121.59290313720705, 37.93444993515032],
#       [-121.27017974853516, 37.93444993515032],
#       [-121.27017974853516, 38.065932950547484],
#       [-121.59290313720705, 38.065932950547484],
#       [-121.59290313720705, 37.93444993515032]
#     ]
#   ]
# }
#
#
# geojson_geometry = {
#   "type": "Polygon",
#   "coordinates": [
#             [
#               [
#                 14.38385009765625,
#                 8.339235543309647
#               ],
#               [
#                 14.940032958984375,
#                 8.339235543309647
#               ],
#               [
#                 14.940032958984375,
#                 9.087180443666833
#               ],
#               [
#                 14.38385009765625,
#                 9.087180443666833
#               ],
#               [
#                 14.38385009765625,
#                 8.339235543309647
#               ]
#             ]]
# }


geojson_geometry = {
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


# get images that overlap with our AOI
geometry_filter = {
  "type": "GeometryFilter",
  "field_name": "geometry",
  "config": geojson_geometry
}

t = dt.now()

y = t - timedelta(days=1)
yy = t - timedelta(days=11)

# get images acquired within a date range
date_range_filter = {
  "type": "DateRangeFilter",
  "field_name": "acquired",
  "config": {
    "gte": "%sT00:00:00.000Z" % (yy.strftime('%Y-%m-%d')),
    "lte": "%sT00:00:00.000Z" % (y.strftime('%Y-%m-%d'))
  }
}

# only get images which have <50% cloud coverage
cloud_cover_filter = {
  "type": "RangeFilter",
  "field_name": "cloud_cover",
  "config": {
    "lte": 0.5
  }
}

# combine our geo, date, cloud filters
combined_filter = {
  "type": "AndFilter",
  "config": [geometry_filter, date_range_filter, cloud_cover_filter]
}


# API Key stored as an env variable
PLANET_API_KEY = os.getenv('PL_API_KEY')

item_type = "PSScene4Band"

# API request object
search_request = {
  "interval": "day",
  "item_types": [item_type],
  "filter": combined_filter
}

DIR_archiv = "/home/nils/data/planet/"
DIR = join(DIR_archiv, item_type)
if not os.path.exists(DIR):
    mkdir(DIR)


# fire off the POST request
search_result = requests.post(
    'https://api.planet.com/data/v1/quick-search',
    auth=HTTPBasicAuth(PLANET_API_KEY, ''),
    json=search_request)

print(json.dumps(search_result.json(), indent=1))

# extract image IDs only
image_ids = [feature['id'] for feature in search_result.json()['features']]
print(image_ids)
# For demo purposes, just grab the first image ID
for image_id in image_ids:

    id0 = image_id

    filename = "%s.tif" % id0
    local_file = join(DIR, filename)  # mkstemp(dir="/home/nils/data/planet/", prefix=id0, suffix='.tif')

    id0_url = 'https://api.planet.com/data/v1/item-types/{}/items/{}/assets'.format(item_type, id0)

    # Returns JSON metadata for assets in this ID. Learn more: planet.com/docs/reference/data-api/items-assets/#asset
    result = requests.get(id0_url, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
    # List of asset types available for this particular satellite image
    print(result.json().keys())
    # This is "inactive" if the "visual" asset has not yet been activated; otherwise 'active'
    if 'analytic' in result.json().keys():
        print "****** down loading file ********"
        print(result.json()['analytic']['status'])
        # Parse out useful links
        links = result.json()[u"analytic"]["_links"]
        self_link = links["_self"]
        activation_link = links["activate"]

        # Request activation of the 'visual' asset:
        activate_result = requests.get(activation_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
        # Parse out useful links
        links = result.json()[u"analytic"]["_links"]
        self_link = links["_self"]
        activation_link = links["activate"]

        # Request activation of the 'visual' asset:
        activate_result = requests.get(activation_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))
        activation_status_result = requests.get(self_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))

        while activation_status_result.json()["status"] != 'active':
            print('*** File is sleeping. gently waking up ****')
            print(activation_status_result.json()["status"])
            time.sleep(30)
            activation_status_result = requests.get(self_link, auth=HTTPBasicAuth(PLANET_API_KEY, ''))

        print('File ready to download: %s' % (activation_status_result.json()["status"]))
        # Image can be downloaded by making a GET with your Planet API key, from here:
        download_link = activation_status_result.json()["location"]


        r = requests.get(download_link, stream=True, verify=False)
        with open(local_file, 'wb') as fp:
            shutil.copyfileobj(r.raw, fp)

        print(local_file)
