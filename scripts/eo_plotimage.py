
import numpy as np
import struct
from osgeo import gdal
from osgeo import ogr
from osgeo import osr
from osgeo import gdal_array
from osgeo.gdalconst import *
import matplotlib.pyplot as plt

from tempfile import mkstemp

im = '/home/nils/birdhouse/flyingpigeon/scripts/20171129mWt2Eh.tif'


cube = gdal.Open(im)
bnd1 = cube.GetRasterBand(1)
bnd2 = cube.GetRasterBand(2)
bnd3 = cube.GetRasterBand(3)

img1 = bnd1.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
img2 = bnd2.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)
img3 = bnd3.ReadAsArray(0, 0, cube.RasterXSize, cube.RasterYSize)

img = np.dstack((img1, img2, img3))

f = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k') # , bbox='tight'
plt.imshow(img3)

_, picname = mkstemp(dir='/home/nils/data/planet/', suffix='.tif')
plt.savefig(picname)
plt.show()
