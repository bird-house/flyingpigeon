from snappy import ProductIO
from snappy import ProductUtils
from snappy import ProgressMonitor
from snappy import jpy

from os.path import join
#
# mtd = 'MTD_MSIL1C.xml'
# fname = DIR.split('/')[-1]
# ID = fname.replace('.SAFE','')
#
# # _, rgb_image = mkstemp(dir='.', prefix=prefix , suffix='.png')
# source = join(DIR, mtd)

uncorr = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2A_MSIL1C_20170129T092221_N0204_R093_T33PVK_20170129T093530.SAFE/MTD_MSIL1C.xml'

corr = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/scihub.copernicus/S2A_MSIL2A_20170129T092221_N0204_R093_T33PVK_20170129T093530.SAFE/MTD_MSIL2A.xml'

sourceProduct = ProductIO.readProduct(uncorr)

red = sourceProduct.getBand('B4')
green = sourceProduct.getBand('B3')
blue = sourceProduct.getBand('B2')


Color = jpy.get_type('java.awt.Color')
ColorPoint = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef$Point')
ColorPaletteDef = jpy.get_type('org.esa.snap.core.datamodel.ColorPaletteDef')
ImageInfo = jpy.get_type('org.esa.snap.core.datamodel.ImageInfo')
ImageLegend = jpy.get_type('org.esa.snap.core.datamodel.ImageLegend')
ImageManager = jpy.get_type('org.esa.snap.core.image.ImageManager')
JAI = jpy.get_type('javax.media.jai.JAI')
RenderedImage = jpy.get_type('java.awt.image.RenderedImage')

# Disable JAI native MediaLib extensions
System = jpy.get_type('java.lang.System')
System.setProperty('com.sun.media.jai.disableMediaLib', 'true')

#
legend = ImageLegend(blue.getImageInfo(), blue)
legend.setHeaderText(blue.getName())


imagefile = '/home/nils/data/uncorr_RGB.png'

image_info = ProductUtils.createImageInfo([red, green, blue], True, ProgressMonitor.NULL)
im = ImageManager.getInstance().createColoredBandImage([red, green, blue], image_info, 0)
JAI.create("filestore", im, imagefile, 'PNG')



sourceProduct = ProductIO.readProduct(corr)

red = sourceProduct.getBand('B4')
green = sourceProduct.getBand('B3')
blue = sourceProduct.getBand('B2')

#
legend = ImageLegend(blue.getImageInfo(), blue)
legend.setHeaderText(blue.getName())

imagefile = '/home/nils/data/corr_RGB.png'

image_info = ProductUtils.createImageInfo([red, green, blue], True, ProgressMonitor.NULL)
im = ImageManager.getInstance().createColoredBandImage([red, green, blue], image_info, 0)
JAI.create("filestore", im, imagefile, 'PNG')
