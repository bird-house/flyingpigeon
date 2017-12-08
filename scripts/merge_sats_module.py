from flyingpigeon import eodata
from datetime import datetime as dt
from os import listdir
from os.path import join, basename
from flyingpigeon.utils import archiveextract
# DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic'
# # DIR = '/home/nils/data/planet/PSScene3Band/'
# tiles = [join(DIR, pic) for pic in listdir(DIR) if '.tif' in pic]


DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic'
# DIR = '/home/nils/data/planet/PSScene3Band/'
tiles = [join(DIR, pic) for pic in listdir(DIR) if '.tif' in pic]


tiles = archiveextract('/home/nils/data/planet/tmpQyYDEX.tar')

dates = set()

for tile in tiles:
    dates = dates.union([eodata.get_timestamp(tile).date()])
dl = list(dates)

for date in dl:
    print "calculating date %s " % date
    tiles_day = [tile for tile in tiles if eodata.get_timestamp(tile).date() == date]
    print(tiles_day)
    archive = eodata.merge(tiles_day)
    print archive
