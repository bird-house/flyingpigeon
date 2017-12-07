
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


**********
# https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
from osgeo import gdal, osr
from tempfile import mkstemp

gdal.UseExceptions()


fname = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene3Band/visual/20171126_084935_1029.tif'

ds = gdal.Open(fname)
data = ds.ReadAsArray()
gt = ds.GetGeoTransform()
proj = ds.GetProjection()

inproj = osr.SpatialReference()
inproj.ImportFromWkt(proj)

print(inproj)

import cartopy.crs as ccrs

projcs = inproj.GetAuthorityCode('PROJCS')
projection = ccrs.epsg(projcs)
print(projection)


import matplotlib.pyplot as plt

subplot_kw = dict(projection=projection)
fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)

extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
          gt[3] + ds.RasterYSize * gt[5], gt[3])

img = ax.imshow(data[:3, :, :].transpose((1, 2, 0)), extent=extent,
                origin='upper')

_, picname = mkstemp(dir='/home/nils/data/planet/', suffix='.tif')

plt.savefig(picname)
