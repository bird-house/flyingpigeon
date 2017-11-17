import os
import json
import requests
from requests.auth import HTTPBasicAuth

import requests
import shutil
import tempfile


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

geojson_geometry = {
  "type": "Polygon",
  "coordinates": [
    [
        [9.15, 14.37],
        [8.30, 14.37],
        [8.30, 15.00],
        [9.15, 15.00],
        [9.15, 14.37],
     ]
  ]
}


# get images that overlap with our AOI
geometry_filter = {
  "type": "GeometryFilter",
  "field_name": "geometry",
  "config": geojson_geometry
}

# get images acquired within a date range
date_range_filter = {
  "type": "DateRangeFilter",
  "field_name": "acquired",
  "config": {
    "gte": "2016-08-31T00:00:00.000Z",
    "lte": "2016-09-01T00:00:00.000Z"
  }
}

# only get images which have <50% cloud coverage
cloud_cover_filter = {
  "type": "RangeFilter",
  "field_name": "cloud_cover",
  "config": {
    "lte": 1  # 0.5
  }
}

# combine our geo, date, cloud filters
combined_filter = {
  "type": "AndFilter",
  "config": [geometry_filter, date_range_filter, cloud_cover_filter]
}


# API Key stored as an env variable
PLANET_API_KEY = os.getenv('PL_API_KEY')

item_type = "PSScene3Band"

# API request object
search_request = {
  "interval": "day",
  "item_types": [item_type],
  "filter": combined_filter
}

# fire off the POST request
search_result = \
  requests.post(
    'https://api.planet.com/data/v1/quick-search',
    auth=HTTPBasicAuth(PLANET_API_KEY, ''),
    json=search_request)

print(json.dumps(search_result.json(), indent=1))
# extract image IDs only
image_ids = [feature['id'] for feature in search_result.json()['features']]
print(image_ids)
# For demo purposes, just grab the first image ID
id0 = image_ids[0]
id0_url = 'https://api.planet.com/data/v1/item-types/{}/items/{}/assets'.format(item_type, id0)

# Returns JSON metadata for assets in this ID. Learn more: planet.com/docs/reference/data-api/items-assets/#asset
result = \
  requests.get(
    id0_url,
    auth=HTTPBasicAuth(PLANET_API_KEY, '')
  )

# List of asset types available for this particular satellite image
print(result.json().keys())

print(result.json()['visual']['status'])
# Parse out useful links
links = result.json()[u"visual"]["_links"]
self_link = links["_self"]
activation_link = links["activate"]

# Request activation of the 'visual' asset:
activate_result = \
  requests.get(
    activation_link,
    auth=HTTPBasicAuth(PLANET_API_KEY, '')
  )
activation_status_result = \
  requests.get(
    self_link,
    auth=HTTPBasicAuth(PLANET_API_KEY, '')
  )

print(activation_status_result.json()["status"])
download_link = activation_status_result.json()["location"]
print(download_link)

r = requests.get(download_link, stream=True, verify=False)
_, local_filename = tempfile.mkstemp(dir=".", suffix='.tif')

url , token = download_link.split("?token=")

with open(local_filename, 'wb') as fp:
    shutil.copyfileobj(r.raw, fp)

print local_filename

#
#
#
#
# # environemtal variable needs to be set
# # export PL_API_KEY=44620741f94a4c7d88f48263*******
#
# # BSB Yamoussa, Nationalpark Cameroon Tschad)
# # geojson_geometry = {
# #   "type": "Polygon",
# #   "coordinates": [
# #     [
# #         [9.15, 14.37],
# #         [8.30, 14.37],
# #         [8.30, 15.00],
# #         [9.15, 15.00],
# #         [9.15, 14.37],
# #      ]
# #   ]
# # }
#
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
# # # build a filter for the AOI
# # query = api.filters.and_filter(
# #   api.filters.geom_filter(geojson_geometry)
# # )
# # item_types = ['PSScene4Band']
# # request = api.filters.build_search_request(query, item_types)
# # results = client.quick_search(request)
#
#
# # get images that overlap with our AOI
# geometry_filter = {
#   "type": "GeometryFilter",
#   "field_name": "geometry",
#   "config": geojson_geometry
# }
#
# # get images acquired within a date range
# date_range_filter = {
#   "type": "DateRangeFilter",
#   "field_name": "acquired",
#   "config": {
#     "gte": "2017-10-31T00:00:00.000Z",
#     "lte": "2017-11-10T00:00:00.000Z"
#   }
# }
#
# # only get images which have <50% cloud coverage
# cloud_cover_filter = {
#   "type": "RangeFilter",
#   "field_name": "cloud_cover",
#   "config": {
#     "lte": 0.5
#   }
# }
#
# # combine our geo, date, cloud filters
# combined_filter = {
#   "type": "AndFilter",
#   "config": [geometry_filter, date_range_filter, cloud_cover_filter]
# }
#
#
# import os
# import json
# import requests
# from requests.auth import HTTPBasicAuth
#
# # API Key stored as an env variable
# PLANET_API_KEY = os.getenv('PL_API_KEY')
#
# item_type = "PSScene3Band"
#
# # API request object
# search_request = {
#   "interval": "day",
#   "item_types": [item_type],
#   "filter": combined_filter
# }
#
# # fire off the POST request
# search_result = \
#   requests.post(
#     'https://api.planet.com/data/v1/quick-search',
#     auth=HTTPBasicAuth(PLANET_API_KEY, ''),
#     json=search_request)
#
# print(json.dumps(search_result.json(), indent=1))
#
# # extract image IDs only
# image_ids = [feature['id'] for feature in search_result.json()['features']]
# print(image_ids)
#
# # For demo purposes, just grab the first image ID
# id0 = image_ids[0]
# id0_url = 'https://api.planet.com/data/v1/item-types/{}/items/{}/assets'.format(item_type, id0)
#
# # Returns JSON metadata for assets in this ID. Learn more: planet.com/docs/reference/data-api/items-assets/#asset
# result = \
#   requests.get(
#     id0_url,
#     auth=HTTPBasicAuth(PLANET_API_KEY, '')
#   )
#
# # List of asset types available for this particular satellite image
# print(result.json().keys())
#
# # This is "inactive" if the "visual" asset has not yet been activated; otherwise 'active'
# print(result.json()['visual']['status'])
#
# # Parse out useful links
# links = result.json()[u"visual"]["_links"]
# self_link = links["_self"]
# activation_link = links["activate"]
#
# # Request activation of the 'visual' asset:
# activate_result = \
#   requests.get(
#     activation_link,
#     auth=HTTPBasicAuth(PLANET_API_KEY, '')
#   )
#
# activation_status_result = \
#   requests.get(
#     self_link,
#     auth=HTTPBasicAuth(PLANET_API_KEY, '')
#   )
#
# print(activation_status_result.json()["status"])
# download_link = activation_status_result.json()["location"]
#
# url = 'https://api.planet.com/data/v1/download?token=eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJCVHFUaWZ5QkVJQTQ3VlRRajJFN05ZRmJnL0dUQnQ4TU9TM0RHcjAreHUyQm4rdXVJdzRyK2l4cFl2VkY2ZWc2NHN2aSt0RHZMRmpwTWxmbzlPN21zQT09IiwiaXRlbV90eXBlX2lkIjoiUFNTY2VuZTNCYW5kIiwidG9rZW5fdHlwZSI6InR5cGVkLWl0ZW0iLCJleHAiOjE1MTA3NzIwNTAsIml0ZW1faWQiOiIyMDE2MDgzMV8yMTI3MDNfMGM0MyIsImFzc2V0X3R5cGUiOiJ2aXN1YWwifQ.XFWfpnCuxv76R7Gfrs5Ognt5-rhto96r23oV4jPhHyI5vjgVcAgjjjbohCEOkgMuuGm3CAd-pI0gX0Qn-PwxyA'
#
#
# url = "https://api.planet.com/data/v1/download"
# param = {'token' : 'eyJhbGciOiJIUzUxMiIsInR5cCI6IkpXVCJ9.eyJzdWIiOiJCVHFUaWZ5QkVJQTQ3VlRRajJFN05ZRmJnL0dUQnQ4TU9TM0RHcjAreHUyQm4rdXVJdzRyK2l4cFl2VkY2ZWc2NHN2aSt0RHZMRmpwTWxmbzlPN21zQT09IiwiaXRlbV90eXBlX2lkIjoiUFNTY2VuZTNCYW5kIiwidG9rZW5fdHlwZSI6InR5cGVkLWl0ZW0iLCJleHAiOjE1MTA3NzIwNTAsIml0ZW1faWQiOiIyMDE2MDgzMV8yMTI3MDNfMGM0MyIsImFzc2V0X3R5cGUiOiJ2aXN1YWwifQ.XFWfpnCuxv76R7Gfrs5Ognt5-rhto96r23oV4jPhHyI5vjgVcAgjjjbohCEOkgMuuGm3CAd-pI0gX0Qn-PwxyA'}
#
# requests.post(url, data=param)
#
#
# from planet import api
# import sys
# client = api.ClientV1()
#
# aoi = {
#   "type": "Polygon",
#   "coordinates": [
#     [
#         [9.15, 14.37],
#         [8.30, 14.37],
#         [8.30, 15.00],
#         [9.15, 15.00],
#         [9.15, 14.37],
#      ]
#   ]
# }
# #
# #
# # {
# #   "type": "Polygon",
# #   "coordinates": [
# #     [
# #       [-122.54, 37.81],
# #       [-122.38, 37.84],
# #       [-122.35, 37.71],
# #       [-122.53, 37.70],
# #       [-122.54, 37.81]
# #     ]
# #   ]
# # }
#
# # build a filter for the AOI
# query = api.filters.and_filter(
#   api.filters.geom_filter(aoi)
# )
# # we are requesting PlanetScope 4 Band imagery
# item_types = ['PSScene4Band']
# request = api.filters.build_search_request(query, item_types)
# # this will cause an exception if there are any API related errors
# results = client.quick_search(request)
#
# # items_iter returns an iterator over API response pages
# for item in results.items_iter(10):
#   # each item is a GeoJSON feature
#   sys.stdout.write('%s\n' % item['id'])
#
#
# # import os
# # import requests
# #
# # item_id = "20160707_195147_1057916_RapidEye-1"
# # item_type = "REOrthoTile"
# # asset_type = "visual"
# #
# # # setup auth
# # session = requests.Session()
# # session.auth = (os.environ['PL_API_KEY'], '')
# #
# # # request an item
# # item = \
# #   session.get(
# #     ("https://api.planet.com/data/v1/item-types/" +
# #     "{}/items/{}/assets/").format(item_type, item_id))
# #
# # # extract the activation url from the item for the desired asset
# # item_activation_url = item.json()[asset_type]["_links"]["activate"]
# #
# # # request activation
# # response = session.post(item_activation_url)
# #
# # print response.status_code
#
#
#
# #
# # import os
# # import requests
# #
# # item_id = "20160707_195147_1057916_RapidEye-1"
# # item_type = "REOrthoTile"
# # asset_type = "visual"
# #
# # # setup auth
# # session = requests.Session()
# # session.auth = (os.environ['PL_API_KEY'], '')
# #
# # # request an item
# # item = \
# #   session.get(
# #     ("https://api.planet.com/data/v1/item-types/" +
# #     "{}/items/{}/assets/").format(item_type, item_id))
# #
# # # extract the activation url from the item for the desired asset
# # item_activation_url = item.json()[asset_type]["_links"]["activate"]
# #
# # # request activation
# # response = session.post(item_activation_url)
# #
# # print response.status_code
