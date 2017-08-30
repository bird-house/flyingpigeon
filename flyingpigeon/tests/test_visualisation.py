from matplotlib import pyplot as plt

def test_plotlibs():
    import matplotlib
    matplotlib.use('Agg')   # use this if no xserver is available
    from matplotlib import pyplot as plt
    from matplotlib.colors import Normalize
    from cartopy import config as cartopy_config
    from cartopy.util import add_cyclic_point
    import cartopy.crs as ccrs
    from cartopy.io.shapereader import Reader
    from cartopy.feature import ShapelyFeature
    from PyPDF2 import PdfFileWriter, PdfFileReader
    from reportlab.pdfgen import canvas


def test_polygons():
    from flyingpigeon.visualisation import plot_polygons
    from os.path import exists
    from os import remove

    png = plot_polygons(['DEU', 'ESP'])

    assert exists(png) is True
    remove(png)

def test_map_spatial_analog():
    from common import TESTDATA
    from flyingpigeon.visualisation import map_spatial_analog

    fig = map_spatial_analog(TESTDATA['dissimilarity.nc'][7:])
    plt.close()
