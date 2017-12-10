from sentinelsat import SentinelAPI, read_geojson, geojson_to_wkt


region = {"type": "Polygon", "coordinates": [[[-69.87682044199994, 12.427394924000097], [-70.05809485599988, 12.537176825000088], [-70.04873613199993, 12.632147528000104], [-69.93639075399994, 12.53172435100005], [-69.87682044199994, 12.427394924000097]]]}

api = SentinelAPI('nilshempelmann', '****')
footprint = geojson_to_wkt(region)
products = api.query(footprint,
                     producttype='SLC',
                     orbitdirection='ASCENDING')
api.download_all(products)
