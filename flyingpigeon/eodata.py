from tempfile import mkstemp

import logging
LOGGER = logging.getLogger("PYWPS")


def get_timestamp(tile):
    """
    returns the creation timestamp of a tile image as datetime.

    :param tile: path to geotiff confom to gdal metadata http://www.gdal.org/gdal_datamodel.html

    :return datetime: timestamp
    """
    from datetime import datetime as dt
    from osgeo import gdal

    ds = gdal.Open(tile, 0)
    ts = ds.GetMetadataItem("TIFFTAG_DATETIME")

    LOGGER.debug("timestamp: %s " % ts)
    ds = None

    timestamp = dt.strptime(ts, '%Y:%m:%d %H:%M:%S')

    return timestamp


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
        call = ['-o',  filename]
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
                ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)

                # Set spatial characteristics of the output object to mirror the input
                kwargs = src.meta
                kwargs.update(
                    dtype=rasterio.float32,
                    count=1)

                # Create the file
                _, ndvifile = mkstemp(dir='.', prefix="ndvi_%s" % key, suffix='.tif')
                with rasterio.open(ndvifile, 'w', **kwargs) as dst:
                    dst.write_band(1, ndvi.astype(rasterio.float32))

                LOGGER.debug("NDVI calculated for %s " % key)
                _, ndviplot = mkstemp(dir='.', prefix="ndvi_%s" % key, suffix='.png')

                plt.imsave(ndviplot, ndvi, cmap=plt.cm.summer)

                ndvifiles.extend([ndvifile])
                ndviplots.extend([ndviplot])

            except:
                LOGGER.exception("Failed to Calculate NDVI for %s " % key)
    return ndvifiles, ndviplots
