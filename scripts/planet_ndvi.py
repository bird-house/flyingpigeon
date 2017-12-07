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

##########################


import rasterio
import numpy

image_file = "20161228_101647_0e26_3B_AnalyticMS.tif"

# Load red and NIR bands - note all PlanetScope 4-band images have band order BGRN
with rasterio.open(image_file) as src:
    band_red = src.read(3)

with rasterio.open(image_file) as src:
    band_nir = src.read(4)


from xml.dom import minidom

xmldoc = minidom.parse("20161218_101700_0e0d_3B_AnalyticMS_metadata.xml")
nodes = xmldoc.getElementsByTagName("ps:bandSpecificMetadata")

# XML parser refers to bands by numbers 1-4
coeffs = {}
for node in nodes:
    bn = node.getElementsByTagName("ps:bandNumber")[0].firstChild.data
    if bn in ['1', '2', '3', '4']:
        i = int(bn)
        value = node.getElementsByTagName("ps:reflectanceCoefficient")[0].firstChild.data
        coeffs[i] = float(value)

# Multiply by corresponding coefficients
band_red = band_red * coeffs[3]
band_nir = band_nir * coeffs[4]

# Allow division by zero
numpy.seterr(divide='ignore', invalid='ignore')

# Calculate NDVI
ndvi = (band_nir.astype(float) - band_red.astype(float)) / (band_nir + band_red)

# Set spatial characteristics of the output object to mirror the input
kwargs = src.meta
kwargs.update(
    dtype=rasterio.float32,
    count = 1)

# Create the file
with rasterio.open('ndvi.tif', 'w', **kwargs) as dst:
        dst.write_band(1, ndvi.astype(rasterio.float32))

import matplotlib.pyplot as plt
plt.imsave("ndvi_cmap.png", ndvi, cmap=plt.cm.summer)
