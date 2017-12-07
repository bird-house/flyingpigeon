from flyingpigeon import eodata

from os import listdir
from os.path import join, basename

DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic'
# DIR = '/home/nils/data/planet/PSScene3Band/'
tiles = [join(DIR, pic) for pic in listdir(DIR) if '.tif' in pic]

archive = eodata.merge(tiles)

print archive
