"""
https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
"""

from osgeo import gdal, osr
from matplotlib import pyplot as plt
from flyingpigeon import visualisation as vs

gdal.UseExceptions()


fname = '/home/nils/data/ndvi_S2B_MSIL1C_20171130T092329_N0206_R093_T33PVK_20171130T1308031Qgjl5.tif'

ds = gdal.Open(fname)
# data = ds.ReadAsArray()
gt = ds.GetGeoTransform()
proj = ds.GetProjection()

inproj = osr.SpatialReference()
inproj.ImportFromWkt(proj)

print(inproj)

bnd1 = ds.GetRasterBand(1)

# img = bnd1.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize, dtype = uint8)
# img = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize//100, buf_ysize=ds.RasterYSize//100, resample_alg=gdal.GRIORA_Mode)

img = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize/2, buf_ysize=ds.RasterYSize/2,)
fig = plt.figure( dpi=90, facecolor='w', edgecolor='k') # , bbox='tight'
img_plot = plt.imshow(img, cmap="summer",  origin='upper')
ndvi_img = vs.fig2plot(fig, output_dir='/home/nils/data/')

print ndvi_img


#
# from osgeo import gdal, osr
#
# gdal.UseExceptions()
#
#
# fname = './data/manhattan2.tif'
#
# ds = gdal.Open(fname)
# data = ds.ReadAsArray()
# gt = ds.GetGeoTransform()
# proj = ds.GetProjection()
#
# inproj = osr.SpatialReference()
# inproj.ImportFromWkt(proj)
#
# print(inproj)
#
# import cartopy.crs as ccrs
#
#
# projcs = inproj.GetAuthorityCode('PROJCS')
# projection = ccrs.epsg(projcs)
# print(projection)
#
# import matplotlib.pyplot as plt
#
# subplot_kw = dict(projection=projection)
# fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)
#
# extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
#           gt[3] + ds.RasterYSize * gt[5], gt[3])
#
# img = ax.imshow(data[:3, :, :].transpose((1, 2, 0)), extent=extent,
#                 origin='upper')
#



#
#
#
#
# import numpy as np
# import struct
# from osgeo import gdal
# from osgeo import ogr
# from osgeo import osr
# from osgeo import gdal_array
# from osgeo.gdalconst import *
# import matplotlib.pyplot as plt
#
# from tempfile import mkstemp
#
# # im = '/home/nils/birdhouse/flyingpigeon/scripts/20171129mWt2Eh.tif'
# im = '/home/nils/data/planet/ndvi_mosaic_94eF5p.tif'
#
# cube = gdal.Open(im)
# bnd1 = cube.GetRasterBand(1)
# # bnd2 = cube.GetRasterBand(2)
# # bnd3 = cube.GetRasterBand(3)
#
# img = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
# # img2 = bnd2.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
# # img3 = bnd3.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
#
# # img = np.dstack((img1, img2, img3))
#
# f = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k') # , bbox='tight'
# plt.imshow(img)
#
# _, picname = mkstemp(dir='/home/nils/data/', suffix='.tif')
# plt.savefig(picname)
# plt.show()
#

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
