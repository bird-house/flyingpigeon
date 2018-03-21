


**********
# https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
from osgeo import gdal, osr
from tempfile import mkstemp

gdal.UseExceptions()

# fname = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene3Band/visual/20171126_084935_1029.tif'
fname = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/EO_data/PSScene4Band/analytic/20171126_100525_103f.tif'

ds = gdal.Open(fname)
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
from numpy import linspace, dstack

# subplot_kw = dict(projection=projection)
# fig, ax = plt.subplots(figsize=(9, 9), subplot_kw=subplot_kw)

fig, ax = plt.subplots()

extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
          gt[3] + ds.RasterYSize * gt[5], gt[3])


rgb = dstack((data[0, :, :], data[1, :, :], data[2, :, :]))

img = ax.imshow(rgb)
# img = ax.imshow(rgb.transpose((1, 2, 0)), extent=extent,
#                 origin='upper')

# ax.gridlines(color='lightgrey', linestyle='-')
# ax.set_xticks()

_, picname = mkstemp(dir='/home/nils/data/planet/', suffix='.png')

plt.savefig(picname)

print picname
