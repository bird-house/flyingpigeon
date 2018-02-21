from tempfile import mkstemp
from osgeo import gdal, osr
import cartopy.crs as ccrs
import matplotlib.pyplot as plt
from flyingpigeon import visualisation as vs
import io
from PIL import Image
import rasterio
import numpy as np
from os import path, listdir
import glob
import subprocess

import logging
LOGGER = logging.getLogger("PYWPS")

def get_bai(basedir, product='Sentinel2'):
    """
    :param basedir: path of basedir for EO data
    :param product: EO product e.g. "Sentinel2" (default)

    :retrun: bai file
    """

    LOGGER.debug("Start calculating BAI")

    prefix = path.basename(path.normpath(basedir)).split('.')[0]

    jps = []
    fname = basedir.split('/')[-1]
    ID = fname.replace('.SAFE','')

    LOGGER.debug("Start calculating BAI for %s " % ID)

    for filename in glob.glob(basedir + '/GRANULE/*/IMG_DATA/*jp2'):
        jps.append(filename)

    jp_B04 = [jp for jp in jps if '_B04.jp2' in jp][0]
    jp_B08 = [jp for jp in jps if '_B08.jp2' in jp][0]

    with rasterio.open(jp_B04) as red:
        RED = red.read()
    with rasterio.open(jp_B08) as nir:
        NIR = nir.read()

    try:
        #compute the BAI burned area index
        # 1 / ((0.1 - RED)^2 + (0.06 -NIR)^2)
        bai = 1 / (np.power((0.1 - RED) ,2) + np.power((0.06 - NIR) ,2))

        LOGGER.debug("BAI values are calculated")

        profile = red.meta
        profile.update(driver='GTiff')
        profile.update(dtype=rasterio.float32)

        _, bai_file = mkstemp(dir='.', prefix=prefix, suffix='.tif')
        with rasterio.open(bai_file, 'w', **profile) as dst:
            dst.write(bai.astype(rasterio.float32))
    except:
        LOGGER.exception("Failed to Calculate BAI for %s " % prefix)
    return bai_file


def get_timestamp(tile):
    """
    returns the creation timestamp of a tile image as datetime.

    :param tile: path to geotiff confom to gdal metadata http://www.gdal.org/gdal_datamodel.html

    :return datetime: timestamp
    """

    from datetime import datetime as dt
    try:
        ds = gdal.Open(tile, 0)
        ts = ds.GetMetadataItem("TIFFTAG_DATETIME")

        LOGGER.debug("timestamp: %s " % ts)
        ds = None  # to close the dataset

        timestamp = dt.strptime(ts, '%Y:%m:%d %H:%M:%S')
    except:
        LOGGER.exception('failed to get timestamp for: %s' % tile)
    return timestamp


def plot_products(products, extend=[10, 20, 5, 15]):
    """
    plot the products extends of the search result

    :param products: output of sentinel api search

    :return graphic: map of extents
    """

    import numpy as np

    from matplotlib.patches import Polygon
    import matplotlib.patches as mpatches
    from matplotlib.collections import PatchCollection

    from cartopy import config as cartopy_config
    import cartopy.feature as cfeature
    from cartopy.util import add_cyclic_point
    import re


    fig = plt.figure(dpi=90, facecolor='w', edgecolor='k')
    projection = ccrs.PlateCarree()
    ax = plt.axes(projection=projection)
    ax.set_extent(extend)
    ax.stock_img()
    ax.coastlines()
    ax.add_feature(cfeature.BORDERS)

    pat = re.compile(r'''(-*\d+\.\d+ -*\d+\.\d+);*''')

    for key in products.keys():
        polygon = str(products[key]['footprint'])

        # s = 'POLYGON ((15.71888453311329 9.045763865974665,15.7018748825589 8.97110837227606,15.66795226563288 8.822558900399137,15.639498612331632 8.69721920092792,15.63428409805786 8.674303514900869,15.600477269179995 8.525798537094156,15.566734239298787 8.377334323160321,15.53315342410745 8.228822837291709,15.499521168391912 8.080353481086165,15.493321895031096 8.052970059354971,14.999818486685434 8.053569047879877,14.999818016115439 9.046743365203026,15.71888453311329 9.045763865974665))'
        matches = pat.findall(polygon)
        if matches:
            xy = np.array([map(float, m.split()) for m in matches])
            ax.add_patch(mpatches.Polygon(xy, closed=True,  transform=ccrs.PlateCarree(), alpha=0.4)) # color='coral'
    # ccrs.Geodetic()

    ax.gridlines(draw_labels=True,)
    img = vs.fig2plot(fig, output_dir='.')

    return img


def plot_band(source, file_extension='PNG', colorscheem=None):
    """
    plots the first band of a geotif file

    :param source: geotif file containning one band with NDVI values
    :param file_extension: format of the output graphic. default='png'
    :param colorscheem: predifined colorscheem
                        allowed values: "NDVI", "BAI"
                        if None (default), plot will given as grayscale

    :result str: path to graphic file
    """

    from snappy import ProductIO
    from snappy import ProductUtils
    # from snappy import ProgressMonitor
    from snappy import jpy

    from os.path import splitext, basename

    try:
        LOGGER.debug('Start plotting Band')
        sourceProduct = ProductIO.readProduct(source)
        # bandname = list(sourceProduct.getBandNames())[0]
        # LOGGER.debug('bandname found: %s ' % bandname)
        ndvi = sourceProduct.getBand("band_1")
    except:
        LOGGER.exception("failed to read ndvi values")
    try:
        LOGGER.debug('read in org.esa information')
        # More Java type definitions required for image generation
        Color = jpy.get_type('java.awt.Color')
        ColorPoint = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef$Point')
        ColorPaletteDef = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef')
        ImageInfo = jpy.get_type('org.esa.snap.core.datamodel.ImageInfo')
        ImageLegend = jpy.get_type('org.esa.snap.core.datamodel.ImageLegend')
        ImageManager = jpy.get_type('org.esa.snap.core.image.ImageManager')
        JAI = jpy.get_type('javax.media.jai.JAI')
        RenderedImage = jpy.get_type('java.awt.image.RenderedImage')

        # Disable JAI native MediaLib extensions
        System = jpy.get_type('java.lang.System')
        System.setProperty('com.sun.media.jai.disableMediaLib', 'true')
    except:
        LOGGER.exception('failed to read in org.esa information')

    # points = [ColorPoint(-1.0, Color.WHITE),
    #           # ColorPoint(50.0, Color.RED),
    #           ColorPoint(1.0, Color.GREEN)]
    # cpd = ColorPaletteDef(points)
    # ii = ImageInfo(cpd)
    # ndvi.setImageInfo(ii)

    try:
        LOGGER.debug('write image')
        img_name = 'INDICE_%s.png' % (splitext(basename(source))[0])

        image_format = 'PNG'

        im = ImageManager.getInstance().createColoredBandImage([ndvi], ndvi.getImageInfo(), 0)
        JAI.create("filestore", im, img_name, image_format)
    except:
        LOGGER.exception('failed to write image')
    return img_name


def resample(DIR, band, resolution):
    """
    resamples a band of a SENTINEL product to a given target resolution

    :param DIR: base directory of Sentinel2 directory tree
    :param band: band name (e.g. B4)
    :param resolution: target resolution in meter (e.g 10)

    :return: resampled band
    """

    from snappy import GPF

    GPF.getDefaultInstance().getOperatorSpiRegistry().loadOperatorSpis()

    HashMap = jpy.get_type('java.util.HashMap')
    BandDescriptor = jpy.get_type('org.esa.snap.core.gpf.common.BandMathsOp$BandDescriptor')

    parameters = HashMap()
    parameters.put('targetResolution', resolution)
    parameters.put('upsampling','Bicubic')
    parameters.put('downsampling','Mean')
    parameters.put('flagDownsampling','FlagMedianAnd')
    parameters.put('resampleOnPyramidLevels',True)

    product = ProductIO.readProduct(DIR)
    product=GPF.createProduct('Resample', parameters, product)

    rsp_band = product.getBand(band)

    return rsp_band

def plot_RGB(DIR, colorscheem='natural_color'):
    """
    Extracts the files for RGB bands of Sentinel2 directory tree, scales and merge the values.
    Output is a merged tif including 3 bands.

    :param DIR: base directory of Sentinel2 directory tree
    :param colorscheem: usage of bands (default=natural_color will use B4,B3,B2 for red,green,blue)

    :returns: png image
    """
    from snappy import ProductIO
    from snappy import ProductUtils
    from snappy import ProgressMonitor
    from snappy import jpy

    from os.path import join

    mtd = 'MTD_MSIL1C.xml'
    fname = DIR.split('/')[-1]
    ID = fname.replace('.SAFE','')

    # _, rgb_image = mkstemp(dir='.', prefix=prefix , suffix='.png')
    source = join(DIR, mtd)

    sourceProduct = ProductIO.readProduct(source)

    if colorscheem == 'naturalcolors':
        red = sourceProduct.getBand('B4')
        green = sourceProduct.getBand('B3')
        blue = sourceProduct.getBand('B2')

    elif colorscheem == 'falsecolors-vegetation':
        red = sourceProduct.getBand('B8')
        green = sourceProduct.getBand('B4')
        blue = sourceProduct.getBand('B3')

    elif colorscheem == 'falsecolors-urban':
        red = sourceProduct.getBand('B12')
        green = sourceProduct.getBand('B11')
        blue = resample(source, 'B4', 20)  # sourceProduct.getBand('B4')

    elif colorscheem == 'athmospheric-penetration':
        red = sourceProduct.getBand('B12')
        green = sourceProduct.getBand('B11')
        blue = sourceProduct.getBand('B8a')

    elif colorscheem == 'agriculture':
        red = sourceProduct.getBand('B11')
        green = resample(source, 'B8', 20)
        blue = resample(source, 'B2', 20)

    elif colorscheem == 'healthy-vegetation':
        red = sourceProduct.getBand('B8')
        green = resample(source, 'B11', 10)
        blue = sourceProduct.getBand('B2')

    elif colorscheem == 'land-water':
        red =  resample(source, 'B8', 20)
        green = sourceProduct.getBand('B11')
        blue = resample(source, 'B4', 20 )

    elif colorscheem == 'naturalcolors-athmosphericremoval':
        red = sourceProduct.getBand('B12')
        green = resample(source, 'B8', 20)
        blue = resample(source, 'B3', 20)

    elif colorscheem == 'shortwave-infrared':
        red = sourceProduct.getBand('B12')
        green = resample(source, 'B8', 20)
        blue = resample(source, 'B4',20)

    elif colorscheem == 'vegetation-analyses':
        red = sourceProduct.getBand('B11')
        green = resample(source, 'B8', 20)
        blue = resample(source, 'B4',20)

    else:
        LOGGER.debug('colorscheem %s not found ' % colorscheem)

    Color = jpy.get_type('java.awt.Color')
    ColorPoint = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef$Point')
    ColorPaletteDef = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef')
    ImageInfo = jpy.get_type('org.esa.snap.core.datamodel.ImageInfo')
    ImageLegend = jpy.get_type('org.esa.snap.core.datamodel.ImageLegend')
    ImageManager = jpy.get_type('org.esa.snap.core.image.ImageManager')
    JAI = jpy.get_type('javax.media.jai.JAI')
    RenderedImage = jpy.get_type('java.awt.image.RenderedImage')

    # Disable JAI native MediaLib extensions
    System = jpy.get_type('java.lang.System')
    System.setProperty('com.sun.media.jai.disableMediaLib', 'true')

    #
    legend = ImageLegend(blue.getImageInfo(), blue)
    legend.setHeaderText(blue.getName())

    # red = product.getBand('B4')
    # green = product.getBand('B3')
    # blue = product.getBand('B2')
    # from tempfile import mkstemp
    # from PIL import Image
    #
    # _ , snapfile = mkstemp(dir='.', prefix='RGB_', suffix='.png')

    imagefile = '%s_%s.png' % (colorscheem, ID)

    image_info = ProductUtils.createImageInfo([red, green, blue], True, ProgressMonitor.NULL)
    im = ImageManager.getInstance().createColoredBandImage([red, green, blue], image_info, 0)
    JAI.create("filestore", im, imagefile, 'PNG')

    #
    # basewidth = 600
    # img = Image.open(snapfile)
    # wpercent = (basewidth / float(img.size[0]))
    # hsize = int((float(img.size[1]) * float(wpercent)))
    # img = img.resize((basewidth, hsize), Image.ANTIALIAS)
    # img.save(imagefile)

    return imagefile


def merge(tiles, prefix="mosaic_"):
    """
    merging a given list of files with gdal_merge.py

    :param tiles: list of geotiffs to be merged_tiles

    :return geotiff: mosaic of merged files
    """

    from flyingpigeon import gdal_merge as gm
    from os.path import join, basename
    import subprocess
    from subprocess import CalledProcessError
    from flyingpigeon.config import _PATH

    try:
        LOGGER.debug('start merging of %s files' % len(tiles))
        # prefix = dt.strftime(date, "%Y%m%d")
        _, filename = mkstemp(dir='.', prefix=prefix, suffix='.tif')
        gdal_merge = '%s/gdal_merge.py' % _PATH
        cmd = ['python', gdal_merge, '-o', filename, '-of', 'GTiff', '-v']
        for tile in tiles:
            LOGGER.debug('extent tile %s ', tile)
            cmd.append(tile)

        LOGGER.debug('cmd: %s' % cmd)
        output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
        LOGGER.debug('gdal_merge log: \n %s', output)

    except CalledProcessError as e:
        LOGGER.exception('failed to merge tiles:\n{0}'.format(e.output))

    # import sys
    # try:
    #     LOGGER.debug('start merging')
    #     # prefix = dt.strftime(date, "%Y%m%d")
    #     _, filename = mkstemp(dir='.', prefix=prefix, suffix='.tif')
    #     call = ['-o',  "%s" % filename, '-of', 'GTiff', '-v']
    #     #
    #     # tiles_day = [tile for tile in tiles if date.date() == get_timestamp(tile).date()]
    #
    #     for tile in tiles:
    #         call.extend([tile])
    #     sys.argv[1:] = call
    #     gm.main()
    #
    #     LOGGER.debug("files merged for %s tiles " % len(tiles))
    # except:
    #     LOGGER.exception("failed to merge tiles")

    return filename


def get_ndvi(basedir, product='Sentinel2'):
    """
    :param basedir: path of basedir for EO data
    :param product: EO product e.g. "Sentinel2" (default)

    :retrun files, plots : list of calculated files and plots
    """
    import rasterio
    import numpy as np
    from os import path, listdir
    from tempfile import mkstemp
    from osgeo import gdal
    # import os, rasterio
    import glob
    import subprocess

    prefix = path.basename(path.normpath(basedir)).split('.')[0]

    jps = []
    fname = basedir.split('/')[-1]
    ID = fname.replace('.SAFE','')

    for filename in glob.glob(basedir + '/GRANULE/*/IMG_DATA/*jp2'):
        jps.append(filename)

    jp_B04 = [jp for jp in jps if '_B04.jp2' in jp][0]
    jp_B08 = [jp for jp in jps if '_B08.jp2' in jp][0]

    with rasterio.open(jp_B04) as red:
        RED = red.read()
    with rasterio.open(jp_B08) as nir:
        NIR = nir.read()

    try:
        # compute the ndvi
        ndvi = (NIR.astype(float) - RED.astype(float)) / (NIR + RED )

        profile = red.meta
        profile.update(driver='GTiff')
        profile.update(dtype=rasterio.float32)

        _, ndvifile = mkstemp(dir='.', prefix=prefix, suffix='.tif')
        with rasterio.open(ndvifile, 'w', **profile) as dst:
            dst.write(ndvi.astype(rasterio.float32))
    except:
        LOGGER.exception("Failed to Calculate NDVI for %s " % prefix)
    return ndvifile


# def get_RGB(DIR, false_color=False):
#     """
#     Extracts the files for RGB bands of Sentinel2 directory tree, scales and merge the values.
#     Output is a merged tif including 3 bands.
#
#     :param DIR: base directory of Sentinel2 directory tree
#     :param false_color: if set to True the near infrared band (B08) will be taken as red band
#
#     :returns geotif: merged geotiff
#     """
#     # from subprocess import CalledProcessError
#
#     jps = []
#     fname = DIR.split('/')[-1]
#     ID = fname.replace('.SAFE','')
#
#     for filename in glob.glob(DIR + '/GRANULE/*/IMG_DATA/*jp2'):
#         jps.append(filename)
#
#     jp_b = [jp for jp in jps if '_B02.jp2' in jp][0]
#     jp_g = [jp for jp in jps if '_B03.jp2' in jp][0]
#     if false_color:
#         jp_r = [jp for jp in jps if '_B08.jp2' in jp][0]
#     else:
#         jp_r = [jp for jp in jps if '_B04.jp2' in jp][0]
#
#     # scaling the color values and trasform from jp2 to tif
#     try:
#         # response.update_status('execution of CASTf90', 50)
#         red = 'RED_{0}.tif'.format(ID)
#         cmd = ['gdal_translate', '-scale', jp_r, red ]
#         # LOGGER.debug("translate command: %s", cmd)
#         output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
#         # output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
#         LOGGER.info('translate output:\n %s', output)
#
#         green = 'GREEN_{0}.tif'.format(ID)
#         cmd = ['gdal_translate', '-scale', jp_g, green ]
#         LOGGER.debug("translate command: %s", cmd)
#         output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
#         LOGGER.info('translate output:\n %s', output)
#
#         blue = 'BLUE_{0}.tif'.format(ID)
#         cmd = ['gdal_translate', '-scale', jp_b, blue ]
#         LOGGER.debug("translate command: %s", cmd)
#         output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
#         LOGGER.info('translate output:\n %s', output)
#         # response.update_status('**** scaling suceeded', 20)
#     except:
#         msg = 'scaleing failed:\n{0}'.format(error)
#         LOGGER.exception(msg)
#
#     # merge tree files  to one geotiff with tree seperated bands
#     try:
#         merged_RGB = 'RGB_{0}.tif'.format(ID)
#         cmd = ['gdal_merge.py', '-seperate', '-co', 'PHOTOMETRIC=RGB', '-o', merged_RGB , red , green, blue ]
#         output, error = subprocess.Popen(cmd, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
#     except:
#         msg = 'merging failed:\n{0}'.format(error)
#         # LOGGER.exception(msg)
#     return merged_RGB



# def plot_bai(geotif, file_extension='jpg', dpi=150, figsize=(10,10)):
#     """
#     plots a BAI image
#
#     :param geotif: geotif file containning one band with BAI values
#     :param file_extension: format of the output graphic. default='png'
#
#     :result str: path to graphic file
#     """
#     #     https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
#     from os.path import basename
#     from snappy import ProductIO
#     from snappy import ProductUtils
#     from snappy import ProgressMonitor
#     from snappy import jpy
#
#
#     gdal.UseExceptions()
#     # norm = vs.MidpointNormalize(midpoint=0)
#     ds = gdal.Open(geotif)
#
#     gt = ds.GetGeoTransform()
#     proj = ds.GetProjection()
#     inproj = osr.SpatialReference()
#     inproj.ImportFromWkt(proj)
#     projcs = inproj.GetAuthorityCode('PROJCS')
#     projection = ccrs.epsg(projcs)
#     # print("Projection: %s  " % projection)
#     subplot_kw = dict(projection=projection)
#     fig, ax = plt.subplots( subplot_kw=subplot_kw, dpi=dpi, figsize=figsize) #,dpi=90, figsize=(10,10)
#
#     extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
#     gt[3] + ds.RasterYSize * gt[5], gt[3])
#
#
#     bnd1 = ds.GetRasterBand(1)
#     data = bnd1.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize) # buf_xsize=ds.RasterXSize/10, buf_ysize=ds.RasterYSize/10,
#
#     img_bai = ax.imshow(data, extent=extent,origin='upper', cmap=plt.cm.afmhot_r, transform=projection)
#
#     title = basename(geotif).split('_')[2]
#     plt.title('BAI')
#     plt.colorbar(img_bai, fraction=0.046, pad=0.04)
#     ax.gridlines() #draw_labels=True,
#
#     bai_img = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension=file_extension)
#
#     return bai_img # bai_plot


# def plot_ndvi(geotif, file_extension='jpg', dpi=150, figsize=(10,10)):
#     """
#     plots a NDVI image
#
#     :param geotif: geotif file containning one band with NDVI values
#     :param file_extension: format of the output graphic. default='png'
#
#     :result str: path to graphic file
#     """
#     #     https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
#     from os.path import basename
#
#     gdal.UseExceptions()
#     # norm = vs.MidpointNormalize(midpoint=0)
#     ds = gdal.Open(geotif)
#
#     gt = ds.GetGeoTransform()
#     proj = ds.GetProjection()
#     inproj = osr.SpatialReference()
#     inproj.ImportFromWkt(proj)
#     projcs = inproj.GetAuthorityCode('PROJCS')
#     projection = ccrs.epsg(projcs)
#     # print("Projection: %s  " % projection)
#     subplot_kw = dict(projection=projection)
#     fig, ax = plt.subplots( subplot_kw=subplot_kw, dpi=dpi, figsize=figsize) #,dpi=90, figsize=(10,10)
#
#     extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
#     gt[3] + ds.RasterYSize * gt[5], gt[3])
#
#
#     bnd1 = ds.GetRasterBand(1)
#     data = bnd1.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize) # buf_xsize=ds.RasterXSize/10, buf_ysize=ds.RasterYSize/10,
#
#     img_ndvi = ax.imshow(data, extent=extent,origin='upper', vmin=-1, vmax=1, cmap=plt.cm.BrBG, transform=projection)
#
#     title = basename(geotif).split('_')[2]
#     plt.title('NDVI')
#     plt.colorbar(img_ndvi, fraction=0.046, pad=0.04)
#     ax.gridlines() #draw_labels=True,
#
#
#     ndvi_img = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension=file_extension)
#
#     #
#     # ndvi_img = mkstemp(dir='.', file_extension='jpg')
#     #
#     # buf = io.FileIO(ndvi_img, 'a')
#     #
#     # plt.savefig(buf)
#     # buf.seek(0)
#     #
#     # buf.close()
#
#     return ndvi_img # ndvi_plot


# def plot_RGB(geotif, rgb_bands=[1,2,3], file_extension='jpg', dpi=50, figsize=(5,5)):
#     """
#     Calculates a RGB image (True color composite) based on red, greed, and blue bands.
#
#     :param geotif: geotif file containning one band with NDVI values
#     :param file_extension: format of the output graphic. default='png'
#     :param rgb_bands: order of bands storing red, green and blue values default=[1,2,3]
#
#     :result str: path to graphic file
#     """
#     from numpy import dstack
#
#     gdal.UseExceptions()
#     ds = gdal.Open(geotif)
#     data = ds.ReadAsArray()
#     gt = ds.GetGeoTransform()
#     proj = ds.GetProjection()
#
#     inproj = osr.SpatialReference()
#     inproj.ImportFromWkt(proj)
#
#     projcs = inproj.GetAuthorityCode('PROJCS')
#     projection = ccrs.epsg(projcs)
#     # print(projection)
#
#     subplot_kw = dict(projection=projection)
#     fig, ax = plt.subplots( subplot_kw=subplot_kw)
#
#     extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
#               gt[3] + ds.RasterYSize * gt[5], gt[3])
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
#     img = ax.imshow(rgb, extent=extent, origin='upper', transform=projection)
#
#     # img = ax.imshow(rgb.transpose((1, 2, 0)), extent=extent,
#     #                 origin='upper')
#
#     ax.gridlines(color='lightgrey', linestyle='-')
#     # ax.set_xticks()
#
#     tcc_plot = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension='jpg')
#
#     plt.close()
#     ds = None
#     return tcc_plot



# def get_ndvi(tiles, product='PlanetScope'):
#     """
#     :param tiles: list of tiles including appropriate metadata files
#     :param product: EO product e.g. "PlanetScope" (default)
#
#     :retrun files, plots : list of calculated files and plots
#     """
#
#     import rasterio
#     import numpy
#     from xml.dom import minidom
#     import matplotlib.pyplot as plt
#
#     ndvifiles = []
#     ndviplots = []
#
#     if product == 'PlanetScope':
#         tiles_dic = ndvi_sorttiles(tiles, product=product)
#         for key in tiles_dic.keys():
#             try:
#                 LOGGER.debug("NDVI for %s" % key)
#                 if len(tiles_dic[key]) == 2:
#                     tile = next(x for x in tiles_dic[key] if ".tif" in x)
#                     meta = next(x for x in tiles_dic[key] if ".xml" in x)
#                 else:
#                     LOGGER.debug('Key %s data are not complete' % key)
#                     continue  # continue with next key
#                 # Load red and NIR bands - note all PlanetScope 4-band images have band order BGRN
#                 with rasterio.open(tile) as src:
#                     band_red = src.read(3)
#
#                 with rasterio.open(tile) as src:
#                     band_nir = src.read(4)
#
#                 LOGGER.debug("data read in memory")
#                 xmldoc = minidom.parse(meta)
#                 nodes = xmldoc.getElementsByTagName("ps:bandSpecificMetadata")
#
#                 # XML parser refers to bands by numbers 1-4
#                 coeffs = {}
#                 for node in nodes:
#                     bn = node.getElementsByTagName("ps:bandNumber")[0].firstChild.data
#                     if bn in ['1', '2', '3', '4']:
#                         i = int(bn)
#                         value = node.getElementsByTagName("ps:reflectanceCoefficient")[0].firstChild.data
#                         coeffs[i] = float(value)
#
#                 # Multiply by corresponding coefficients
#                 band_red = band_red * coeffs[3]
#                 band_nir = band_nir * coeffs[4]
#
#                 LOGGER.debug("data athmospheric corrected")
#                 # Allow division by zero
#                 numpy.seterr(divide='ignore', invalid='ignore')
#
#                 # Calculate NDVI
#                 bn_ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)
#
#                 # Set spatial characteristics of the output object to mirror the input
#                 kwargs = src.meta
#                 kwargs.update(
#                     dtype=rasterio.float32,
#                     count=1)
#
#                 # Create the file
#                 _, ndvifile = mkstemp(dir='.', prefix="ndvi_%s" % key, suffix='.tif')
#                 with rasterio.open(ndvifile, 'w', **kwargs) as dst:
#                     dst.write_band(1, bn_ndvi.astype(rasterio.float32))
#
#                 LOGGER.debug("NDVI calculated for %s " % key)
#
#                 ndvifiles.extend([ndvifile])
#                 LOGGER.debug("NDVI calculated: %s " % ndvifile)
#             except:
#                 LOGGER.exception("Failed to Calculate NDVI for %s " % key)
#     return ndvifiles

#
# def ndvi_sorttiles(tiles, product="PlanetScope"):
#     """
#     sort un list fo files to calculate the NDVI.
#     red nivr and metadata are sorted in an dictionary
#
#     :param tiles: list of scene files and metadata
#     :param product: EO data product e.g. "PlanetScope" (default)
#
#     :return dictionary: sorted files ordered in a dictionary
#     """
#
#     from os.path import splitext, basename
#     if product == "PlanetScope":
#         ids = []
#         for tile in tiles:
#             bn, _ = splitext(basename(tile))
#             ids.extend([bn])
#
#         tiles_dic = {key: None for key in ids}
#
#         for key in tiles_dic.keys():
#             tm = [t for t in tiles if key in t]
#             tiles_dic[key] = tm
#         # LOGGER.debug("files sorted in dictionary %s" % tiles_dic)
#     return tiles_dic
