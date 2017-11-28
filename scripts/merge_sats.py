from flyingpigeon import gdal_merge as gm
from os import listdir
from os.path import join
from tempfile import mkstemp

# Create an image with the pixels in all bands initialized to 255.
#
# % gdal_merge.py -init 255 -o out.tif in1.tif in2.tif
# Create an RGB image that shows blue in pixels with no data. The first two bands will be initialized to 0 and the third band will be initialized to 255.
#
# % gdal_merge.py -init "0 0 255" -o out.tif in1.tif in2.tif


DIR = '/home/nils/data/planet/PSScene3Band/'
pics = [join(DIR, pic) for pic in listdir(DIR)]

_, filename = mkstemp(dir='/home/nils/data/planet/', suffix='.tif')

call = ['-o',  filename]
for pic in pics:
    call.extend([pic])

sys.argv[1:] = call  # ['-o',  outfile, pics[0], pics[1], pics[2], pics[3], pics[4]]

gm.main()


import numpy as np
import struct
from osgeo import gdal
from osgeo import ogr
from osgeo import osr
from osgeo import gdal_array
from osgeo.gdalconst import *
import matplotlib.pyplot as plt

filename

cube = gdal.Open(filename)
bnd1 = cube.GetRasterBand(29)
bnd2 = cube.GetRasterBand(19)
bnd3 = cube.GetRasterBand(9)

img1 = bnd1.ReadAsArray(0,0,cube.RasterXSize, cube.RasterYSize)
img2 = bnd2.ReadAsArray(0,0,cube.RasterXSize, cube.RasterYSize)
img3 = bnd3.ReadAsArray(0,0,cube.RasterXSize, cube.RasterYSize)

img = np.dstack((img1,img2,img3))

f = plt.figure()
plt.imshow(img)
plt.show()
