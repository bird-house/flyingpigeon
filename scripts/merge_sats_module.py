from flyingpigeon import eodata
from datetime import datetime as dt
from os import listdir
from os.path import join, basename
from flyingpigeon.utils import archiveextract
# DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic'
# # DIR = '/home/nils/data/planet/PSScene3Band/'
# tiles = [join(DIR, pic) for pic in listdir(DIR) if '.tif' in pic]


# DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic'
# DIR = '/home/nils/data/planet/ndvi/'
DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene3Band/visual'
# DIR = '/home/nils/data/planet/PSScene3Band/'
tiles = [join(DIR, pic) for pic in listdir(DIR) if '.tif' in pic]


# tiles = archiveextract('/home/nils/data/planet/tmpQyYDEX.tar')

dates = set()

for tile in tiles:
    dates = dates.union([eodata.get_timestamp(tile).date()])
dl = list(dates)

for date in dl:
    print "calculating date %s " % date
    tiles_day = [tile for tile in tiles if eodata.get_timestamp(tile).date() == date]
    # print(tiles_day)
    archive = eodata.merge(tiles_day)
    print archive


from flyingpigeon import gdal_merge as gm
from os.path import join, basename
import sys

# merged_tiles = []
# dates = set()
# # dates = dates.union([basename(pic).split('_')[0] for pic in tiles])
# dates = dates.union(get_timestamp(tile).date() for tile in tiles)
#
# for date in dates:


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
