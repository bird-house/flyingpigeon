import logging
LOGGER = logging.getLogger("PYWPS")


def merge(tiles):
    from flyingpigeon import gdal_merge as gm
    from os.path import join, basename
    from tempfile import mkstemp
    import sys

    merged_tiles = []
    dates = set()
    dates = dates.union([basename(pic).split('_')[0] for pic in tiles])

    for date in dates:
        try:
            LOGGER.debug('merge date %s' % date)
            _, filename = mkstemp(dir='.', prefix=date, suffix='.tif')
            call = ['-o',  filename]
            tiles_day = [tile for tile in tiles if date in tile]

            for tile_d in tiles_day:
                call.extend([tile_d])
            sys.argv[1:] = call
            gm.main()

            merged_tiles.extend([filename])
        except:
            LOGGER.exception("failed to merge tiles of date  %s " % date)

    return merged_tiles
