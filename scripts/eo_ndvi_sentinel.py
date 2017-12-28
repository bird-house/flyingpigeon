# import matplotlib.pyplot as plt
import rasterio
import numpy as np
from os import path, listdir
from tempfile import mkstemp
from osgeo import gdal
import os, rasterio

DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE/GRANULE/L1C_T33PVL_A004123_20171220T093259/IMG_DATA/'

jps = [path.join(DIR,jp) for jp in listdir(DIR) if ".jp2" in jp]

for jp in jps:
    # get red
    if "_B04" in jp:
        with rasterio.open(jp) as red:
            RED = red.read()
    # get nivr
    if "_B08" in jp:
        with rasterio.open(jp) as nir:
            NIR = nir.read()


# outfile = r'some\path\ndvi.tif'
# #url to the bands
# b4 = 'http://sentinel-s2-l1c.s3.amazonaws.com/tiles/30/T/TK/2017/4/12/0/B04.jp2'
# b8 = 'http://sentinel-s2-l1c.s3.amazonaws.com/tiles/30/T/TK/2017/4/12/0/B08.jp2'
#
# #open the bands (I can't believe how easy is this with rasterio!)
# with rasterio.open(b4) as red:
#     RED = red.read()
# with rasterio.open(b8) as nir:
#     NIR = nir.read()

#compute the ndvi
ndvi = (NIR.astype(float) - RED.astype(float)) / (NIR+RED)

# ndvi = (NIR-RED)/(NIR+RED)
#print(ndvi.min(), ndvi.max()) The problem is alredy here

profile = red.meta
profile.update(driver='GTiff')
profile.update(dtype=rasterio.float32)

prefix="ndvi_SENTINE2"
_, ndvifile = mkstemp(dir='/home/nils/data/', prefix=prefix, suffix='.tif')
with rasterio.open(ndvifile, 'w', **profile) as dst:
    dst.write(ndvi.astype(rasterio.float32))


from flyingpigeon import eodata

ndvi_img = eodata.plot_ndvi(ndvifile)

print ndvi_img


# print ndvifile
#
# from osgeo import gdal, osr
# from matplotlib import pyplot as plt
#
# gdal.UseExceptions()
#
#
# fname = '/home/nils/data/.tif'
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
# img = bnd1.ReadAsArray(0, 0, buf_xsize=ds.RasterXSize/10, buf_ysize=ds.RasterYSize/10, )
# fig = plt.figure( dpi=90, facecolor='w', edgecolor='k') # , bbox='tight'
# img_plot = plt.imshow(img, cmap="summer",  origin='upper')
# ndvi_img = vs.fig2plot(fig, output_dir='/home/nils/data/')
#
# print ndvi_img
#



# ndvifile = fname
# cube = gdal.Open(ndvifile)
# bnd1 = ds.GetRasterBand(1)
# bnd2 = cube.GetRasterBand(2)
# bnd3 = cube.GetRasterBand(3)

# img = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)

# img2 = bnd2.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
# img3 = bnd3.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)

# img = np.dstack((img1, img2, img3))
# figsize=(20, 10)
#
# f = plt.figure( dpi=90, facecolor='w', edgecolor='k') # , bbox='tight'
#
# plt.imshow(img)
#
# _, picname = mkstemp(dir='/home/nils/data/', suffix='.png')
# plt.savefig(picname)
#
# plt.show()



#
#
# from snappy import ProductIO
# import numpy as np
# import matplotlib.pyplot as plt
#
# p = ProductIO.readProduct('snappy/testdata/MER_FRS_L1B_SUBSET.dim')
# rad13 = p.getBand('radiance_13')
# w = rad13.getRasterWidth()
# h = rad13.getRasterHeight()
# rad13_data = np.zeros(w * h, np.float32)
# rad13.readPixels(0, 0, w, h, rad13_data)
# p.dispose()
# rad13_data.shape = h, w
# imgplot = plt.imshow(rad13_data)
# imgplot.write_png('radiance_13.png')

#
# # Create the file
#
# with rasterio.open(ndvifile, 'w', **kwargs) as dst:
#     dst.write_band(1, bn_ndvi.astype(rasterio.float32))
#
#
# #
# #
# # with rasterio.open(image_file) as src:
# #     band_red = src.read(3)
# #
# # with rasterio.open(image_file) as src:
# #     band_nir = src.read(4)
# np.seterr(divide='ignore', invalid='ignore')
#
# ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)
#
# plt.imsave('/home/nils/data/test_ndvi.png', ndvi, cmap=plt.cm.summer)
#
# ##########################
#
#
# import rasterio
# import numpy
#
# image_file = "20161228_101647_0e26_3B_AnalyticMS.tif"
#
# # Load red and NIR bands - note all PlanetScope 4-band images have band order BGRN
# with rasterio.open(image_file) as src:
#     band_red = src.read(3)
#
# with rasterio.open(image_file) as src:
#     band_nir = src.read(4)
#
#
# from xml.dom import minidom
#
# xmldoc = minidom.parse("20161218_101700_0e0d_3B_AnalyticMS_metadata.xml")
# nodes = xmldoc.getElementsByTagName("ps:bandSpecificMetadata")
#
# # XML parser refers to bands by numbers 1-4
# coeffs = {}
# for node in nodes:
#     bn = node.getElementsByTagName("ps:bandNumber")[0].firstChild.data
#     if bn in ['1', '2', '3', '4']:
#         i = int(bn)
#         value = node.getElementsByTagName("ps:reflectanceCoefficient")[0].firstChild.data
#         coeffs[i] = float(value)
#
# # Multiply by corresponding coefficients
# band_red = band_red * coeffs[3]
# band_nir = band_nir * coeffs[4]
#
# # Allow division by zero
# numpy.seterr(divide='ignore', invalid='ignore')
#
# # Calculate NDVI
# ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)
#
# # Set spatial characteristics of the output object to mirror the input
# kwargs = src.meta
# kwargs.update(
#     dtype=rasterio.float32,
#     count = 1)
#
# # Create the file
# with rasterio.open('ndvi.tif', 'w', **kwargs) as dst:
#         dst.write_band(1, ndvi.astype(rasterio.float32))
#
