import matplotlib.pyplot as plt

import rasterio
import numpy as np

image_file = '/home/nils/data/planet/PSScene4Band/20171118_100713_0f3b.tif'

with rasterio.open(image_file) as src:
    band_red = src.read(3)

with rasterio.open(image_file) as src:
    band_nir = src.read(4)

np.seterr(divide='ignore', invalid='ignore')


ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)

plt.imsave('/home/nils/data/planet/test_ndvi.png', ndvi, cmap=plt.cm.summer)
