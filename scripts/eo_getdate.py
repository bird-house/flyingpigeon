
from osgeo import gdal


an1 = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic/20171126_084818_100e.tif'
an2 = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic/20171126_084819_100e.tif'

raster = an1
ds = gdal.Open(raster, 0)

print ds.GetMetadataItem("TIFFTAG_DATETIME")
ds = None
