from tempfile import mkstemp
from osgeo import gdal, osr

import logging
LOGGER = logging.getLogger("PYWPS")


def get_timestamp(tile):
    """
    returns the creation timestamp of a tile image as datetime.

    :param tile: path to geotiff confom to gdal metadata http://www.gdal.org/gdal_datamodel.html

    :return datetime: timestamp
    """
    from datetime import datetime as dt

    ds = gdal.Open(tile, 0)
    ts = ds.GetMetadataItem("TIFFTAG_DATETIME")

    LOGGER.debug("timestamp: %s " % ts)
    ds = None  # to close the dataset

    timestamp = dt.strptime(ts, '%Y:%m:%d %H:%M:%S')
    return timestamp


def plot_ndvi(geotif, file_extension='png'):
    """
    plots a NDVI image

    :param geotif: geotif file containning one band with NDVI values
    :param file_extension: format of the output graphic. default='png'

    :result str: path to graphic file
    """
    import numpy as np
    import struct

    # from osgeo import ogr
    # from osgeo import osr
    # from osgeo import gdal_array
    # from osgeo.gdalconst import *

    from flyingpigeon import visualisation as vs

    import cartopy.crs as ccrs
    from cartopy import feature
    import matplotlib.pyplot as plt

    # im = '/home/nils/birdhouse/flyingpigeon/scripts/20171129mWt2Eh.tif'

    cube = gdal.Open(geotif)
    bnd1 = cube.GetRasterBand(1)

    proj = cube.GetProjection()

    inproj = osr.SpatialReference()
    inproj.ImportFromWkt(proj)
    print(inproj)
    LOGGER.debug("projection of geotif %s " % inproj)

    projcs = inproj.GetAuthorityCode('PROJCS')  # requires internet connection
    projection = ccrs.epsg(projcs)

    # get the extent of the plot
    gt = cube.GetGeoTransform()
    extent = (gt[0], gt[0] + cube.RasterXSize * gt[1], gt[3] + cube.RasterYSize * gt[5], gt[3])

    img = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)

    fig = plt.figure()  # , bbox='tight'
    ax = plt.axes(projection=ccrs.PlateCarree())
    norm = vs.MidpointNormalize(midpoint=0)

    img_ndvi = ax.imshow(img,
                         origin='upper', extent=extent, transform=ccrs.PlateCarree(),
                         norm=norm, vmin=-1, vmax=1, cmap=plt.cm.summer)
    # ax.coastlines(resolution='50m', color='black', linewidth=1)
    # ax.add_feature(feature.BORDERS, linestyle='-', alpha=.5)
    # ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True)
    ax.gridlines()
    plt.title('NDVI')
    plt.colorbar(img_ndvi)
    ndvi_plot = vs.fig2plot(fig, file_extension=file_extension, dpi=300)

    plt.close()
    ds = None
    return ndvi_plot


def plot_truecolorcomposite(geotif, rgb_bands=[1,2,3], file_extension='png', dpi=300, figsize=(5,5)):
    """
    Calculates a RGB image (True color composite) based on red, greed, and blue bands.

    :param geotif: geotif file containning one band with NDVI values
    :param file_extension: format of the output graphic. default='png'
    :param rgb_bands: order of bands storing red, green and blue values default=[1,2,3]

    :result str: path to graphic file
    """

    gdal.UseExceptions()

    ds = gdal.Open(geotif)
    data = ds.ReadAsArray()
    gt = ds.GetGeoTransform()
    proj = ds.GetProjection()

    inproj = osr.SpatialReference()
    inproj.ImportFromWkt(proj)

    # import cartopy.crs as ccrs
    #
    # projcs = inproj.GetAuthorityCode('PROJCS')
    # projection = ccrs.epsg(projcs)
    # print(projection)

    import matplotlib.pyplot as plt
    from flyingpigeon import visualisation as vs
    from numpy import linspace, dstack

    # subplot_kw = dict(projection=projection)
    # fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)

    fig, ax = plt.subplots()

    extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
              gt[3] + ds.RasterYSize * gt[5], gt[3])

    print  extent

    red = ds.GetRasterBand(rgb_bands[0])
    green = ds.GetRasterBand(rgb_bands[1])
    blue = ds.GetRasterBand(rgb_bands[2])   # band 1 PSSCINE4Band blue

    img_r = red.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
    img_g = green.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)
    img_b = blue.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize)

    # rgb = dstack((data[0, :, :], data[1, :, :], data[2, :, :]))

    rgb = dstack([img_r, img_g, img_b])
    img = ax.imshow(rgb)

    # img = ax.imshow(rgb.transpose((1, 2, 0)), extent=extent,
    #                 origin='upper')

    # ax.gridlines(color='lightgrey', linestyle='-')
    # ax.set_xticks()

    tcc_plot = vs.fig2plot(fig, dpi=dpi, figsize=figsize)

    plt.close()
    ds = None
    return tcc_plot


def merge(tiles, prefix="mosaic_"):
    """
    merging a given list of files with gdal_merge.py

    :param tiles: list of geotiffs to be merged_tiles

    :return geotiff: mosaic of merged files
    """

    from flyingpigeon import gdal_merge as gm
    from os.path import join, basename
    import sys

    # merged_tiles = []
    # dates = set()
    # # dates = dates.union([basename(pic).split('_')[0] for pic in tiles])
    # dates = dates.union(get_timestamp(tile).date() for tile in tiles)
    #
    # for date in dates:

    try:
        LOGGER.debug('start merging')
        # prefix = dt.strftime(date, "%Y%m%d")
        _, filename = mkstemp(dir='.', prefix=prefix, suffix='.tif')
        call = ['-o',  "%s" % filename, '-of', 'GTiff', '-v']
        #
        # tiles_day = [tile for tile in tiles if date.date() == get_timestamp(tile).date()]

        for tile in tiles:
            call.extend([tile])
        sys.argv[1:] = call
        gm.main()

        LOGGER.debug("files merged for %s tiles " % len(tiles))
    except:
        LOGGER.exception("failed to merge tiles")

    return filename


def ndvi(tiles, product='PlanetScope'):
    """
    :param tiles: list of tiles including appropriate metadata files
    :param product: EO product e.g. "PlanetScope" (default)

    :retrun files, plots : list of calculated files and plots
    """

    import rasterio
    import numpy
    from xml.dom import minidom
    import matplotlib.pyplot as plt

    ndvifiles = []
    ndviplots = []

    if product == 'PlanetScope':
        tiles_dic = ndvi_sorttiles(tiles, product=product)
        for key in tiles_dic.keys():
            try:
                LOGGER.debug("NDVI for %s" % key)
                if len(tiles_dic[key]) == 2:
                    tile = next(x for x in tiles_dic[key] if ".tif" in x)
                    meta = next(x for x in tiles_dic[key] if ".xml" in x)
                else:
                    LOGGER.debug('Key %s data are not complete' % key )
                    continue  # continue with next key
                # Load red and NIR bands - note all PlanetScope 4-band images have band order BGRN
                with rasterio.open(tile) as src:
                    band_red = src.read(3)

                with rasterio.open(tile) as src:
                    band_nir = src.read(4)

                LOGGER.debug("data read in memory")
                xmldoc = minidom.parse(meta)
                nodes = xmldoc.getElementsByTagName("ps:bandSpecificMetadata")

                # XML parser refers to bands by numbers 1-4
                coeffs = {}
                for node in nodes:
                    bn = node.getElementsByTagName("ps:bandNumber")[0].firstChild.data
                    if bn in ['1', '2', '3', '4']:
                        i = int(bn)
                        value = node.getElementsByTagName("ps:reflectanceCoefficient")[0].firstChild.data
                        coeffs[i] = float(value)

                # Multiply by corresponding coefficients
                band_red = band_red * coeffs[3]
                band_nir = band_nir * coeffs[4]

                LOGGER.debug("data athmospheric corrected")
                # Allow division by zero
                numpy.seterr(divide='ignore', invalid='ignore')

                # Calculate NDVI
                bn_ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)

                # Set spatial characteristics of the output object to mirror the input
                kwargs = src.meta
                kwargs.update(
                    dtype=rasterio.float32,
                    count=1)

                # Create the file
                _, ndvifile = mkstemp(dir='.', prefix="ndvi_%s" % key, suffix='.tif')
                with rasterio.open(ndvifile, 'w', **kwargs) as dst:
                    dst.write_band(1, bn_ndvi.astype(rasterio.float32))

                LOGGER.debug("NDVI calculated for %s " % key)

                # _, ndviplot = mkstemp(dir='.', prefix="ndvi_%s" % key, suffix='.png')
                #
                # plt.imsave(ndviplot, ndvi, cmap=plt.cm.summer)
                #
                # ndvifiles.extend([ndvifile])
                # ndviplots.extend([ndviplot])

            except:
                LOGGER.exception("Failed to Calculate NDVI for %s " % key)
    return ndvifiles


def ndvi_sorttiles(tiles, product="PlanetScope"):
    """
    sort un list fo files to calculate the NDVI.
    red nivr and metadata are sorted in an dictionary

    :param tiles: list of scene files and metadata
    :param product: EO data product e.g. "PlanetScope" (default)

    :return dictionary: sorted files ordered in a dictionary
    """

    from os.path import splitext, basename
    if product == "PlanetScope":
        ids = []
        for tile in tiles:
            bn, _ = splitext(basename(tile))
            ids.extend([bn])

        tiles_dic = {key: None for key in ids}

        for key in tiles_dic.keys():
            tm = [t for t in tiles if key in t]
            tiles_dic[key] = tm
        LOGGER.debug("files sorted in dictionary %s" % tiles_dic)
    return tiles_dic
