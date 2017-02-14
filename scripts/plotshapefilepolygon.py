import shapefile as shp
import matplotlib.pyplot as plt
from os.path import join

from flyingpigeon import config
from flyingpigeon.subset import get_ugid
DIR_SHP = config.shapefiles_dir()

sf = shp.Reader(join(DIR_SHP, "countries.shp"))

import cartopy.io.shapereader as shpreader

reader = shpreader.Reader(join(DIR_SHP, "countries.shp"))
countries = reader.records()
# ugid = get_ugid(polygons='DEU', geom='countries')
# print ugid

plt.figure()

for i, country in enumerate(countries):
    if country.attributes['ISO_ A3'] in ['DEU']:
        shape = sf.scountry[i]
<<<<<<< HEAD
        xs = [xor x in sha  pe.shape.points[:]]
=======
        xs = [x for x in shape.shape.points[:]]
>>>>>>> factsheetgenerator
        ys = [y[1] for y in shape.shape.points[:]]
        plt.plot(xs, ys)
plt.show()


ax = plt.axes()
shape_feature = ShapelyFeature(geoms, ccrs.GOOGLE_MERCATOR)
ax.add_feature(shape_feature)
<<<<<<< HEAD
plt.show()


from flyingpigeon import config
DIR_SHP = config.shapefiles_dir()


import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature

fname = join(DIR_SHP, "countries.shp")  # r'simplified-land-polygons-complete-3857\simplified_land_polygons.shp'

ax = plt.axes(projection=ccrs.Robinson())

countries = reader.records()

for country in countries:
    if country.attributes['ISO_A3'] in ['DEU']:
        shape_feature = ShapelyFeature(country.geometries(), ccrs.PlateCarree(), edgecolor='black')
        ax.add_feature(shape_feature)
plt.show()


=======
plt.show()


from flyingpigeon import config
DIR_SHP = config.shapefiles_dir()


import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature

fname = join(DIR_SHP, "countries.shp")  # r'simplified-land-polygons-complete-3857\simplified_land_polygons.shp'

ax = plt.axes(projection=ccrs.Robinson())

countries = reader.records()

for country in countries:
    if country.attributes['ISO_A3'] in ['DEU']:
        shape_feature = ShapelyFeature(country.geometries(), ccrs.PlateCarree(), edgecolor='black')
        ax.add_feature(shape_feature)
plt.show()


>>>>>>> factsheetgenerator
import matplotlib.pyplot as plt
import cartopy.crs as ccrs
from cartopy.io.shapereader import Reader
from cartopy.feature import ShapelyFeature

from flyingpigeon import config
from os.path import join
DIR_SHP = config.shapefiles_dir()

fname = join(DIR_SHP, "countries.shp")
geos = Reader(fname).geometries()
<<<<<<< HEAD
records = Reader(fname).records()
=======
countries = reader.records()
>>>>>>> factsheetgenerator

ax = plt.axes(projection=ccrs.Robinson())
for r in records:
    geo = geos.next()
<<<<<<< HEAD
    if r.attributes['ISO_A3'] in ['DEU']:
        shape_feature = ShapelyFeature(geo, ccrs.PlateCarree(), edgecolor='black')
        ax.add_feature(shape_feature)
plt.show()



=======
    shape_feature = ShapelyFeature(geo, ccrs.PlateCarree(), edgecolor='black')
    ax.add_feature(shape_feature)
# plt.save('country_polygon.png')
plt.show()


>>>>>>> factsheetgenerator
#
#
# for c, shape in enumerate(sf.shapeRecords()):
#     if c in ugid:
#         x = [i[0] for i in shape.shape.points[:]]
#         y = [i[1] for i in shape.shape.points[:]]
#         plt.plot(x, y)
# plt.show()
#
# import matplotlib.pyplot as plt
# import matplotlib.patches as patches
# from matplotlib.patches import Polygon
# from matplotlib.collections import PatchCollection
#
# import cartopy.io.shapereader as shpreader
# from os.path import join
# from flyingpigeon import config
# DIR_SHP = config.shapefiles_dir()
#
# reader = shpreader.Reader(join(DIR_SHP, "countries.shp"))
# countries = reader.records()
# geos = reader.geometries()
#
# fig, ax = plt.subplots()
# polygons = []
# for i in countries:
#     polygon = geos.next()
#     if i.attributes['ISO_A3'] in ['DEU']:
#         polygons.append(polygon[0])
#
# p = PatchCollection(polygons, alpha=0.4)
# ax.add_collection(p)
#
# plt.show()
#
#
#
# import matplotlib.pyplot as plt
# import matplotlib.patches as mpatches
#
# import cartopy.crs as ccrs
#
# desired_projections = [ccrs.PlateCarree(),
#                        ccrs.RotatedPole(pole_latitude=45, pole_longitude=180)]
# for plot_num, desired_proj in enumerate(desired_projections):
#
#     ax = plt.subplot(2, 1, plot_num + 1, projection=desired_proj)
#
#     ax.set_global()
#
#     ax.add_patch(mpatches.Rectangle(xy=[-70, -45], width=90, height=90,
#                                     facecolor='blue',
#                                     alpha=0.2,
#                                     transform=ccrs.PlateCarree())
#                  )
#
#     ax.add_patch(mpatches.Rectangle(xy=polygon[0], width=90, height=90,
#                                     facecolor='red',
#                                     alpha=0.2,
#                                     transform=ccrs.Geodetic())
#                  )
#
#     ax.gridlines()
#     ax.coastlines()
#
# plt.show()
