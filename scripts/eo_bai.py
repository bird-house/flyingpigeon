"""
BAI burned area index

return = [1 / ((0.1 - B04) ^ 2 + (0.06 -B08)^2)]
"""



def get_bai(basedir, product='Sentinel2'):
    """
    :param basedir: path of basedir for EO data
    :param product: EO product e.g. "Sentinel2" (default)

    :retrun: bai file
    """
import rasterio
import numpy as np
from os import path, listdir
from tempfile import mkstemp
from osgeo import gdal
# import os, rasterio
import glob
import subprocess

prefix = path.basename(path.normpath(basedir)).split('.')[0]

jps = []
fname = basedir.split('/')[-1]
ID = fname.replace('.SAVE','')

for filename in glob.glob(basedir + '/GRANULE/*/IMG_DATA/*jp2'):
    jps.append(filename)

jp_B04 = [jp for jp in jps if '_B04.jp2' in jp][0]
jp_B08 = [jp for jp in jps if '_B08.jp2' in jp][0]

with rasterio.open(jp_B04) as red:
    RED = red.read()
with rasterio.open(jp_B08) as nir:
    NIR = nir.read()

    try:
        #compute the BAI burned area index
        bai = 1 / (np.power((0.1 - RED) ,2) + np.power((0.06 -NIR) ,2))

        print bai.shape

        profile = red.meta
        profile.update(driver='GTiff')
        profile.update(dtype=rasterio.float32)

        _, bai_file = mkstemp(dir='.', prefix=prefix, suffix='.tif')
        with rasterio.open(bai_file, 'w', **profile) as dst:
            dst.write(bai.astype(rasterio.float32))
    except:
        print("Failed to Calculate BAI for %s " % prefix)
    return bai_file


def plot_bai(geotif, file_extension='jpg', dpi=150, figsize=(10,10)):
    """
    plots a BAI image

    :param geotif: geotif file containning one band with BAI values
    :param file_extension: format of the output graphic. default='png'

    :result str: path to graphic file
    """
    #     https://ocefpaf.github.io/python4oceanographers/blog/2015/03/02/geotiff/
from os.path import basename

gdal.UseExceptions()
# norm = vs.MidpointNormalize(midpoint=0)
ds = gdal.Open(geotif)

gt = ds.GetGeoTransform()
proj = ds.GetProjection()
inproj = osr.SpatialReference()
inproj.ImportFromWkt(proj)
projcs = inproj.GetAuthorityCode('PROJCS')
projection = ccrs.epsg(projcs)
# print("Projection: %s  " % projection)
subplot_kw = dict(projection=projection)
fig, ax = plt.subplots( subplot_kw=subplot_kw, dpi=dpi, figsize=figsize) #,dpi=90, figsize=(10,10)

extent = (gt[0], gt[0] + ds.RasterXSize * gt[1],
gt[3] + ds.RasterYSize * gt[5], gt[3])


bnd1 = ds.GetRasterBand(1)
data = bnd1.ReadAsArray(0, 0, ds.RasterXSize, ds.RasterYSize) # buf_xsize=ds.RasterXSize/10, buf_ysize=ds.RasterYSize/10,

    img_bai = ax.imshow(data, extent=extent,origin='upper', cmap=plt.cm.BrBG, transform=projection)

    title = basename(geotif).split('_')[2]
    plt.title('BAI')
    plt.colorbar(img_bai, fraction=0.046, pad=0.04)
    ax.gridlines() #draw_labels=True,

    bai_img = vs.fig2plot(fig, dpi=dpi, figsize=figsize, file_extension=file_extension)

    return bai_img # bai_plot




DIR = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2B_MSIL1C_20171220T092359_N0206_R093_T33PVL_20171220T130518.SAFE/'

bai = get_bai(DIR)

print bai
