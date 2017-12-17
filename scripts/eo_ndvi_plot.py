
import numpy as np
import struct
from osgeo import gdal
from osgeo import ogr
from osgeo import osr
from osgeo import gdal_array
from osgeo.gdalconst import *
import matplotlib.pyplot as plt

from tempfile import mkstemp

# im = '/home/nils/birdhouse/flyingpigeon/scripts/20171129mWt2Eh.tif'
im = '/home/nils/data/planet/ndvi_mosaic_94eF5p.tif'

cube = gdal.Open(im)
bnd1 = cube.GetRasterBand(1)
# bnd2 = cube.GetRasterBand(2)
# bnd3 = cube.GetRasterBand(3)

img = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
# img2 = bnd2.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
# img3 = bnd3.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)

# img = np.dstack((img1, img2, img3))

f = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k') # , bbox='tight'
plt.imshow(img)

_, picname = mkstemp(dir='/home/nils/data/', suffix='.tif')
plt.savefig(picname)
plt.show()


# import os
# import matplotlib.pyplot as plt
#
# from cartopy import config
# import cartopy.crs as ccrs
#
#
# fig = plt.figure(figsize=(8, 12))
#
# # get the path of the file. It can be found in the repo data directory.
# fname = os.path.join(config["repo_data_dir"],
#                      'raster', 'sample', 'Miriam.A2012270.2050.2km.jpg'
#                      )
# img_extent = (-120.67660000000001, -106.32104523100001, 13.2301484511245, 30.766899999999502)
# img = plt.imread(fname)
#
# ax = plt.axes(projection=ccrs.PlateCarree())
# plt.title('Hurricane Miriam from the Aqua/MODIS satellite\n'
#           '2012 09/26/2012 20:50 UTC')
#
# # set a margin around the data
# ax.set_xmargin(0.05)
# ax.set_ymargin(0.10)
#
# # add the image. Because this image was a tif, the "origin" of the image is in the
# # upper left corner
# ax.imshow(img, origin='upper', extent=img_extent, transform=ccrs.PlateCarree())
# ax.coastlines(resolution='50m', color='black', linewidth=1)
#
# # mark a known place to help us geo-locate ourselves
# ax.plot(-117.1625, 32.715, 'bo', markersize=7, transform=ccrs.Geodetic())
# ax.text(-117, 33, 'San Diego', transform=ccrs.Geodetic())
