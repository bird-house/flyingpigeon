
import numpy as np

import matplotlib.pyplot as plt
import matplotlib
from matplotlib.patches import Polygon
import matplotlib.patches as mpatches
from matplotlib.collections import PatchCollection

from cartopy import config as cartopy_config
import cartopy.feature as cfeature
from cartopy.util import add_cyclic_point
import cartopy.crs as ccrs

fig = plt.figure(dpi=90, facecolor='w', edgecolor='k')

import re

pat = re.compile(r'''(-*\d+\.\d+ -*\d+\.\d+);*''')
s = 'POLYGON ((15.71888453311329 9.045763865974665,15.7018748825589 8.97110837227606,15.66795226563288 8.822558900399137,15.639498612331632 8.69721920092792,15.63428409805786 8.674303514900869,15.600477269179995 8.525798537094156,15.566734239298787 8.377334323160321,15.53315342410745 8.228822837291709,15.499521168391912 8.080353481086165,15.493321895031096 8.052970059354971,14.999818486685434 8.053569047879877,14.999818016115439 9.046743365203026,15.71888453311329 9.045763865974665))'

matches = pat.findall(s)
if matches:
    xy = np.array([map(float, m.split()) for m in matches])

#
# xy = np.array([[14.091911258909906, 8.141018593002007],
# [15.088597794983281, 8.142024478978072],
# [15.088393218629665, 7.14880742704817],
# [14.09400773070998, 7.147925563681057],
# [14.091911258909906, 8.141018593002007]])
#


#
# polygon = Polygon([[14.091911258909906, 8.141018593002007],
# [15.088597794983281, 8.142024478978072],
# [15.088393218629665, 7.14880742704817],
# [14.09400773070998, 7.147925563681057],
# [14.091911258909906, 8.141018593002007]])
# #Polygon(np.random.rand(num_sides ,2), True)
#patches.append(polygon)

projection = ccrs.PlateCarree()

# projection = ccrs.Orthographic(central_longitude=np.mean(xy[:, 0]),
#                   central_latitude=np.mean(xy[:, 1]),
#                   globe=None)  # Robinson()

ax = plt.axes(projection=projection)
ax.set_extent([10, 20, 5, 15])
ax.stock_img()
ax.coastlines()
ax.add_feature(cfeature.BORDERS)
ax.add_patch(mpatches.Polygon(xy, closed=True,  transform=ccrs.PlateCarree(), color='coral', alpha=0.6))
ax.add_patch(mpatches.Polygon(xy+2, closed=True,  transform=ccrs.PlateCarree(), color='coral', alpha=0.6))
# ccrs.Geodetic()
ax.gridlines(draw_labels=True,)

# plt.show()
#

# ax.add_patch(polygon)

from flyingpigeon import visualisation as vs
img = vs.fig2plot(fig, output_dir='/home/nils/data')

print img
plt.show()

# xy = np.array([[14.091911258909906, 8.141018593002007],
# [15.088597794983281, 8.142024478978072],
# [15.088393218629665, 7.14880742704817],
# [14.09400773070998, 7.147925563681057],
# [14.091911258909906, 8.141018593002007]])
#
#
# fig, ax = plt.subplots()
# patches = []
# num_polygons = 5
# num_sides = 5
#
# for i in range(num_polygons):
#     polygon = Polygon(np.random.rand(num_sides ,2), True)
#     patches.append(polygon)
#
# p = PatchCollection(patches, cmap=matplotlib.cm.jet, alpha=0.4)
#
# colors = 100*np.random.rand(len(patches))
# p.set_array(np.array(colors))
#
# ax.add_collection(p)
#
# plt.show()

# fig, ax = plt.subplots()
# patches = []
#
# num_polygons = 5
# num_sides = 5

#   # tiler = MapQuestOpenAerial()
#   #   ax = plt.axes(projection=tiler.crs)
#   #   plt.title('The effect of incorrectly referencing the Solomon Islands')
#   #
#   #   # Pick the area of interest. In our case, roughly the Solomon Islands, and
#   #   # get hold of the coastlines for that area.
#   #   extent = (155, 163, -11.5, -6)
#   #   ax.set_extent(extent, geodetic)
#   #   geoms = list(dataset.intersecting_geometries(extent))
#   #
#   #   # Add the MapQuest aerial imagery at zoom level 7.
#   #   ax.add_image(tiler, 7)
#
#
# url = 'http://map1c.vis.earthdata.nasa.gov/wmts-geo/wmts.cgi'
#
# #
# # p = PatchCollection(patches, cmap=matplotlib.cm.jet, alpha=0.4)
# #
# # colors = 100*np.random.rand(len(patches))
# # p.set_array(np.array(colors))
# #
# # ax.add_collection(p)
# plt.imgsave(img)
# plt.show()
#
#
#
#
#
# xy = np.array([[np.min(lons), np.min(lats)],
#                [np.max(lons), np.min(lats)],
#                [np.max(lons), np.max(lats)],
#                [np.min(lons), np.max(lats)]])
#
# fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
# projection = ccrs.Robinson()
#
# #  ccrs.Orthographic(central_longitude=np.mean(xy[:, 0]),
# #  central_latitude=np.mean(xy[:, 1]),
# #  globe=None)  # Robinson()
#
# ax = plt.axes(projection=projection)
# ax.stock_img()
# ax.coastlines()
# ax.add_patch(mpatches.Polygon(xy, closed=True,  transform=ccrs.PlateCarree(), color='coral', alpha=0.6))
# # ccrs.Geodetic()
# ax.gridlines()
# plt.show()
#
# if file_extension is None:
#     map_graphic = fig
# else:
#     map_graphic = fig2plot(fig=fig, file_extension=file_extension)
# plt.close()
#
# return map_graphic
