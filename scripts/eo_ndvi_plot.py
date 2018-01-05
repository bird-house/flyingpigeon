"""
https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
"""

from osgeo import gdal, osr
import rasterio
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from flyingpigeon import visualisation as vs

dataf = '/home/nils/data/ndvi_SENTINE2ogmvEh.tif'



import rasterio
from matplotlib import pyplot
src = rasterio.open(dataf)
pyplot.imshow(src.read(1), cmap='pink')
pyplot.show = lambda : None  # prevents showing during doctests
pyplot.show()



ds = gdal.Open(dataf)
proj = ds.GetProjection()
inproj = osr.SpatialReference()
inproj.ImportFromWkt(proj)
#
# print('File projection %s ' % inproj)
#
projcs = inproj.GetAuthorityCode('PROJCS')
projection = ccrs.epsg(projcs)
gt = ds.GetGeoTransform()
extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
gt[3] + ds.RasterYSize * gt[5], gt[3])

ds = None

src = rasterio.open(dataf)

subplot_kw = dict(projection=projection)
fig, ax = plt.subplots( subplot_kw=subplot_kw)

plt.imsave(fname='/home/nils/data/test.png', arr=src.read(1), extent=extent,origin='upper', norm=norm, vmin=-1, vmax=1, cmap=plt.cm.BrBG)

# from flyingpigeon import visualisation as vs
# ndvi_plot = vs.fig2plot(fig, output_dir='.')
# print ndvi_plot

plt.show()
history



gdal.UseExceptions()
norm = vs.MidpointNormalize(midpoint=0)
# fname = '/home/nils/data/ndvi_S2B_MSIL1C_20171130T092329_N0206_R093_T33PVK_20171130T1308031Qgjl5.tif'

ds = gdal.Open(fname)
proj = ds.GetProjection()
inproj = osr.SpatialReference()
inproj.ImportFromWkt(proj)
#
# print('File projection %s ' % inproj)
#
projcs = inproj.GetAuthorityCode('PROJCS')
projection = ccrs.epsg(projcs)
# print("Projection: %s  " % projection)
subplot_kw = dict(projection=projection)
fig, ax = plt.subplots( subplot_kw=subplot_kw)




ds = None

with rasterio.open(dataf) as src:
    for block_index, window in src.block_windows(1):
        block_array = src.read(window=window)
        plt.imshow(block_array)

#
#  import rasterio
# >>> from matplotlib import pyplot
# >>> src = rasterio.open("tests/data/RGB.byte.tif")
# >>> pyplot.imshow(src.read(1), cmap='pink')
# <matplotlib.image.AxesImage object at 0x...>
# >>> pyplot.show = lambda : None  # prevents showing during doctests
# >>> pyplot.show()
        #  src.meta
        # Out[7]:
        # {'affine': Affine(10.0, 0.0, 399960.0,
        #        0.0, -10.0, 1100040.0),
        #  'count': 1,
        #  'crs': CRS({'init': u'epsg:32633'}),
        #  'driver': u'GTiff',
        #  'dtype': 'float32',
        #  'height': 10980,
        #  'nodata': None,
        #  'transform': (399960.0, 10.0, 0.0, 1100040.0, 0.0, -10.0),
        #  'width': 10980}

        result_block = some_calculation(block_array)

        ds = gdal.Open(geotif)
        # data = ds.ReadAsArray( buf_xsize=ds.RasterXSize/2, buf_ysize=ds.RasterYSize/2,)
        gt = src.get_transform()   # ds.GetGeoTransform()

        extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
        gt[3] + ds.RasterYSize * gt[5], gt[3])


        bnd1 = ds.GetRasterBand(1)
        data = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize/10, buf_ysize=ds.RasterYSize/10,)

        img_ndvi = ax.imshow(data, extent=extent,origin='upper', norm=norm, vmin=-1, vmax=1, cmap=plt.cm.BrBG)
        # img_ndvi = ax.imshow(data, extent=extent, transform=projection,  # [:3, :, :].transpose((1, 2, 0))
        #                 origin='upper',norm=norm, vmin=-1, vmax=1, cmap=plt.cm.summer)

        plt.title('NDVI')
        plt.colorbar(img_ndvi)
        ax.gridlines() #draw_labels=True,

from flyingpigeon import visualisation as vs
ndvi_plot = vs.fig2plot(fig, output_dir='.')

#
#
# from osgeo import gdal, osr
# from matplotlib import pyplot as plt
# from flyingpigeon import visualisation as vs
#
# gdal.UseExceptions()
#
# fname = '/home/nils/data/ndvi_SENTINE2ogmvEh.tif'
#
# ds = gdal.Open(fname)
# # data = ds.ReadAsArray()
# gt = ds.GetGeoTransform()
# proj = ds.GetProjection()
#
# inproj = osr.SpatialReference()
# inproj.ImportFromWkt(proj)
#
# print(inproj)
#
# bnd1 = ds.GetRasterBand(1)
#
# # img = bnd1.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize, dtype = uint8)
# # img = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize//100, buf_ysize=ds.RasterYSize//100, resample_alg=gdal.GRIORA_Mode)
#
# img = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize/2, buf_ysize=ds.RasterYSize/2,)
# fig = plt.figure( dpi=90, facecolor='w', edgecolor='k') # , bbox='tight'
# img_plot = plt.imshow(img, cmap="summer",  origin='upper')
# ndvi_img = vs.fig2plot(fig, output_dir='/home/nils/data/')
#
# print ndvi_img
#
#
# from flyingpigeon import eodata
# fname = '/home/nils/data/ndvi_SENTINE2ogmvEh.tif'
# eodata.plot_ndvi(fname)
#
#
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

# with rasterio.open("your/data/geo.tif") as src:
#     for block_index, window in src.block_windows(1):
#         block_array = src.read(window=window)
#         result_block = some_calculation(block_array)
    #
    # from tempfile import mkstemp
    #
    #
    # _, ndvi_plot = mkstemp(dir='.', suffix='.png')
    # plt.savefig(ndvi_plot)
    #
    # # ndvi_plot = vs.fig2plot(fig, file_extension=file_extension, dpi=90, figsize=(5, 5))  # dpi=300
    #
    # # ds = None
    # plt.show()


    # import numpy as np
    # import struct
    #
    # # from osgeo import ogr
    # # from osgeo import osr
    # # from osgeo import gdal_array
    # # from osgeo.gdalconst import *
    #
    # from flyingpigeon import visualisation as vs
    #
    # import cartopy.crs as ccrs
    # from cartopy import feature
    # import matplotlib.pyplot as plt
    #
    # # im = '/home/nils/birdhouse/flyingpigeon/scripts/20171129mWt2Eh.tif'
    #
    # cube = gdal.Open(geotif)
    # bnd1 = cube.GetRasterBand(1)
    #
    # # proj = cube.GetProjection()
    # #
    # # inproj = osr.SpatialReference()
    # # inproj.ImportFromWkt(proj)
    # # # print(inproj)
    # # LOGGER.debug("projection of geotif %s " % inproj)
    # #
    # # projcs = inproj.GetAuthorityCode('PROJCS')  # requires internet connection
    # # projection = ccrs.epsg(projcs)
    #
    # # get the extent of the plot
    # gt = cube.GetGeoTransform()
    # extent = (gt[0], gt[0] + cube.RasterXSize * gt[1], gt[3] + cube.RasterYSize * gt[5], gt[3])
    #
    # img = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
    #
    # fig = plt.figure()  # , bbox='tight'
    # # ax = plt.axes(projection=ccrs.PlateCarree())
    # norm = vs.MidpointNormalize(midpoint=0)
    #
    # img_ndvi = plt.imshow(img[10000:-1,0:-1000],
    #                      origin='upper', extent=extent, # transform=ccrs.PlateCarree(),
    #                      norm=norm, vmin=-1, vmax=1, cmap=plt.cm.summer)
    #
    #
    # # img_ndvi = ax.imshow(img[0:-5000,0:-10000],
    # #                      origin='upper', extent=extent, # transform=ccrs.PlateCarree(),
    # #                      norm=norm, vmin=-1, vmax=1, cmap=plt.cm.summer)
    #
    # # ax.coastlines(resolution='50m', color='black', linewidth=1)
    # # ax.add_feature(feature.BORDERS, linestyle='-', alpha=.5)
    # # ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True)
    # # ax.gridlines()
    # plt.title('NDVI')
    # plt.colorbar(img_ndvi)
    # ndvi_plot = vs.fig2plot(fig, file_extension=file_extension, dpi=90)  # dpi=300
    #
    # plt.close()figsize=(9, 9),
