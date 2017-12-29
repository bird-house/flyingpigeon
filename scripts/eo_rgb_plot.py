from flyingpigeon import eodata
import glob
import subprocess
# from subprocess import CalledProcessError

DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE'
jps = []

for filename in glob.glob(DIR + '/GRANULE/*/IMG_DATA/*jp2'):
    jps.append(filename)

for jp in jps:
    if '_B08.jp2' in jp:
        jp_red = jp
    if '_B03.jp2' in jp:
        jp_green = jp
    if '_B02.jp2' in jp:
        jp_blue = jp

# scaling the color values and trasform from jp2 to tif
try:
    # response.update_status('execution of CASTf90', 50)
    cmd = ['gdal_translate', '-scale', jp_red, 'B04.tif' ]
    # LOGGER.debug("translate command: %s", cmd)
    output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    # output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
    # LOGGER.info('translate output:\n %s', output)

    cmd = ['gdal_translate', '-scale', jp_green, 'B03.tif' ]
    # LOGGER.debug("translate command: %s", cmd)
    output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    # LOGGER.info('translate output:\n %s', output)

    cmd = ['gdal_translate', '-scale', jp_blue, 'B02.tif' ]
    # LOGGER.debug("translate command: %s", cmd)
    output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
    # LOGGER.info('translate output:\n %s', output)

    # response.update_status('**** scaling suceeded', 20)
except:
    msg = 'scaleing failed:\n{0}'.format(error)
    # LOGGER.exception(msg)

try:
    fname = 'merged_RGB.tif'
    cmd = ['gdal_merge.py', '-seperate', '-co', 'PHOTOMETRIC=RGB', '-o', fname, 'B04.tif', 'B03.tif', 'B02.tif' ]
    output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
except:
    msg = 'merging failed:\n{0}'.format(error)
    # LOGGER.exception(msg)

img = eodata.plot_RGB(fname, figsize=(7,7))

print img

#
#
#
# import rasterio
#
# fname = '/home/nils/data/merged_RGB.tif'

#
#
#
# import boto3
# import matplotlib.image as mpimg
# import numpy as np
# from PIL import Image
#
# s3 = boto3.resource('s3', region_name='us-east-2')
# bucket = s3.Bucket('sentinel-s2-l1c')
# path = 'tiles/36/R/UU/2017/5/14/0/'
#
# object = bucket.Object(path + 'B02.jp2')
# object.download_file('B02.jp2')
# object = bucket.Object(path + 'B03.jp2')
# object.download_file('B03.jp2')
# object = bucket.Object(path + 'B04.jp2')
# object.download_file('B04.jp2')
#
# Image.MAX_IMAGE_PIXELS = 1000000000
#
# print('Reading B04.jp2...')
# img_red = mpimg.imread('B04.jp2')
#
# print('Reading B03.jp2...')
# img_green = mpimg.imread('B03.jp2')
#
# print('Reading B02.jp2...')
# img_blue = mpimg.imread('B02.jp2')
#
# img = np.dstack((img_red, img_green, img_blue))
#
# img = np.divide(img, 256)
# img = img.astype(np.uint8)
#
# mpimg.imsave('MIX.jpeg', img, format='jpg')
#
#
#
# # David Trethewey 01-06-2016
# #
# # Sentinel2 Bands Stacker
# # Uses visible, NIR, SWIR bands
# #
# # Assumptions:
# #
# # the .jp2 files are in the current directory
# # that this script is being run from
# #
# # there is only one Sentinel2 scene in the directory
# # and no other jp2 files
# #
# # Converts jp2 files of each band to single stacked file
# #
# # imports
# import rsgislib
# import rsgislib.imageutils
# import os.path
# import sys
#
# # image list
# # find all *.jp2 files in the current directory
# directory = os.getcwd()
# dirFileList = os.listdir(directory)
# # print dirFileList
#
# jp2FileList = [f for f in dirFileList if (f[-4:].lower()=='.jp2')]
#
# bands = ['01', '02', '03', '04', '05', '06', '07', '08', '09', '10', '11', '12', '8A']
# # bands that are already at 10m resolution
# bands_10m = ['02', '03', '04', '08']
#
# # resample other bands to resolution of blue image (B02)
# bands_toberesam = [b for b in bands if b not in bands_10m]
#
# # identify the band number by counting backwards from the end in the filename
# Bands_VIS_NIR_SWIR_FileList = [f for f in jp2FileList if (f[-6:-4] in bands)and(f[-7]=='B')]
#
# # list of bands to be resampled
# Bands_resam_FileList = [f for f in jp2FileList if (f[-6:-4] in bands_toberesam)and(f[-7]=='B')]
#
# # find the filename of the blue image
# blue_image = [f for f in jp2FileList if (f[-6:-4] == '02')and(f[-7]=='B')][0]
#
# # bands to be resampled 20m --> 10m
# # 05, 06, 07, 8b, 11, 12
# # bands to be resampled 60m --> 10m
# # 01, 09, 10
#
# for b in Bands_resam_FileList:
#     print("resampling band {q} to 10m".format(q=b[-6:-4]))
#     outFile = b[:-4]+'_10m.kea'
#     rsgislib.imageutils.resampleImage2Match(blue_image, b, outFile, 'KEA', 'cubic')
#     Bands_VIS_NIR_SWIR_FileList.remove(b)
#     Bands_VIS_NIR_SWIR_FileList.append(outFile)
#
# Bands_VIS_NIR_SWIR_FileList = sorted(Bands_VIS_NIR_SWIR_FileList)
#
# fileNameBase = blue_image[:-7]
#
# # Sentinel2 bands
# bandNamesList = ["B1Coastal443nm", "B2Blue490nm", "B3Green560nm", "B4Red665nm", "B5NIR705nm", "B6NIR740nm",
#                  "B7NIR783nm", "B8NIR_broad842nm", "B9NIR940nm", "B10_1375nm", "B11_SWIR1610nm",
#                  "B12_SWIR2190nm", "B8A_NIR865nm"]
#
# #output file name
# outputImage = fileNameBase + 'B'+''.join(bands)+'_stack.kea'
#
# #output format (GDAL code)
# outFormat = 'KEA'
# outType = rsgislib.TYPE_32UINT
#
# # stack bands using rsgislib
# rsgislib.imageutils.stackImageBands(Bands_VIS_NIR_SWIR_FileList, bandNamesList, outputImage, None, 0, outFormat, outType)
# # stats and pyramids
# rsgislib.imageutils.popImageStats(outputImage,True,0.,True)
#
# # remove individual resampled 10m files
# print("removing intermediate resampled files")
# for b in Bands_resam_FileList:
#     outFile = b[:-4]+'_10m.kea'
#     os.remove(outFile)
#
#
#
#
#
# import rasterio
# import numpy as np
# from os import path, listdir
# from tempfile import mkstemp
# from osgeo import gdal
# import os, rasterio
# from numpy import linspace, dstack
#
# # DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2A_MSIL1C_20171225T092411_N0206_R093_T33PVL_20171225T112331.SAFE/GRANULE/L1C_T33PVL_A013103_20171225T093559/IMG_DATA/'
#
# DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE/GRANULE/L1C_T33PVL_A004123_20171220T093259/IMG_DATA/'
#
#
# jps = [path.join(DIR,jp) for jp in listdir(DIR) if ".jp2" in jp]
#
# for jp in jps:
#     # get red
#     if "_B04" in jp:
#         with rasterio.open(jp) as r:
#             red = r.read(1)
#             # img_r = red.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#     # get nivr
#     if "_B03" in jp:
#         with rasterio.open(jp) as g:
#             green = g.read(1)
#             # img_g = green.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#
#     # get nivr
#     if "_B02" in jp:
#         with rasterio.open(jp) as b:
#             blue = b.read(1)
#             # img_b = blue.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#
# import rsgislib
# from rsgislib import imageutils
# from os import path, listdir
#
# DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE/GRANULE/L1C_T33PVL_A004123_20171220T093259/IMG_DATA/'
#
#
# jps = [path.join(DIR,jp) for jp in listdir(DIR) if ".jp2" in jp]
#
# for jp in jps:
#     # get red
#     if "_B04" in jp:
#         red = jp
#     if "_B03" in jp:
#         green = jp
#     if "_B02" in jp:
#         blue = jp
#
#
# images = [red, green, blue]
#
# band_names = ["B4Red665nm", "B3Green560nm", "B2Blue490nm" ]
#
# out_image = '/home/nils/data/sentinel_image_stack.tif'
#
# # data_type = rsgislib.TYPE_16UINT
# data_type = rsgislib.TYPE_8UINT
#
# imageutils.stackImageBands(images, band_names, out_image, None, 0, 'Gtiff', data_type)
#
# # import matplotlib.pyplot as plt
#
#
# from osgeo import gdal, osr
#
# gdal.UseExceptions()
#
#
# ds = gdal.Open(out_image)
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
# rgb = dstack([red, green, blue])
# img = np.divide(rgb, 256)
# img = img.astype(np.uint8)
#
# # img = ax.imshow(rgb)
#
# from rasterio.plot import show
# import matplotlib.pyplot as plt
# # from flyingpigeon import visualisation as vs
# fig, ax = plt.subplots()
#
# # show(src)
# plt.imshow(img) # , transform=r.transform
# # show(img, transform=r.transform)
#
#
#
# # fig, (axr, axg, axb) = plt.subplots(1,3, figsize=(21,7))
# # show(red, ax=axr, cmap='Reds', title='red channel')
# # show(green, ax=axg, cmap='Greens', title='green channel')
# # show(blue, ax=axb, cmap='Blues', title='blue channel')
# # plt.show()
#
#
#
# # rgb = dstack((data[0, :, :], data[1, :, :], data[2, :, :]))
#
# # from numpy import linspace, dstack
#
# # subplot_kw = dict(projection=projection)
# # fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)
#
#
# # img = ax.imshow(rgb.transpose((1, 2, 0)), extent=extent,
# #                 origin='upper')
#
# # ax.gridlines(color='lightgrey', linestyle='-')
# # ax.set_xticks()
#
# tcc_plot = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension='jpg')
#
#
# import rasterio
# from matplotlib import pyplot
# for jp in jps:
#     # get red
#     if "_B04" in jp:
#         with rasterio.open(jp) as r:
#             red = r.read_band(1)
#             fig, ax = plt.subplots()
#             # src = rasterio.open("tests/data/RGB.byte.tif")
#             pyplot.imshow(red, cmap='red')
#             pyplot.show = lambda : None  # prevents showing during doctests
#             pyplot.show()
#             tcc_plot = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension='jpg')
#             print tcc_plot
#
#
# def plot_truecolorcomposite(geotif, rgb_bands=[1,2,3], file_extension='png', dpi=300, figsize=(5,5)):
#     """
#     Calculates a RGB image (True color composite) based on red, greed, and blue bands.
#
#     :param geotif: geotif file containning one band with NDVI values
#     :param file_extension: format of the output graphic. default='png'
#     :param rgb_bands: order of bands storing red, green and blue values default=[1,2,3]
#
#     :result str: path to graphic file
#     """
#
#     gdal.UseExceptions()
#
#     ds = gdal.Open(geotif)
#     data = ds.ReadAsArray()
#     gt = ds.GetGeoTransform()
#     proj = ds.GetProjection()
#
#     inproj = osr.SpatialReference()
#     inproj.ImportFromWkt(proj)
#
#     # import cartopy.crs as ccrs
#     #
#     # projcs = inproj.GetAuthorityCode('PROJCS')
#     # projection = ccrs.epsg(projcs)
#     # print(projection)
#
#     import matplotlib.pyplot as plt
#     from flyingpigeon import visualisation as vs
#     from numpy import linspace, dstack
#
#     # subplot_kw = dict(projection=projection)
#     # fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)
#
#     fig, ax = plt.subplots()
#
#     extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
#               gt[3] + ds.RasterYSize * gt[5], gt[3])
#
#     print  extent
#
#     red = ds.GetRasterBand(rgb_bands[0])
#     green = ds.GetRasterBand(rgb_bands[1])
#     blue = ds.GetRasterBand(rgb_bands[2])   # band 1 PSSCINE4Band blue
#
#     img_r = red.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#     img_g = green.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#     img_b = blue.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
#
#     # rgb = dstack((data[0, :, :], data[1, :, :], data[2, :, :]))
#
#     rgb = dstack([img_r, img_g, img_b])
#     img = ax.imshow(rgb)
#
#     # img = ax.imshow(rgb.transpose((1, 2, 0)), extent=extent,
#     #                 origin='upper')
#
#     # ax.gridlines(color='lightgrey', linestyle='-')
#     # ax.set_xticks()
#
#     tcc_plot = vs.fig2plot(fig, dpi=dpi, figsize=figsize)
#
#     plt.close()
#     ds = None
#     return tcc_plot
#
#
# from osgeo import gdal, osr
#
# infile = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE/MTD_MSIL1C.xml'
# dataset = gdal.Open(infile, gdal.GA_ReadOnly)
# subdatasets = dataset.GetSubDatasets()
# subdatasets
