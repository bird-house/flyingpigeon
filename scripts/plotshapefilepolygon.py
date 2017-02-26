import os
from os.path import join
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date
import numpy as np
import logging
import matplotlib
matplotlib.use('Agg')   # use this if no xserver is available

from matplotlib import pyplot as plt
from matplotlib.colors import Normalize
from cartopy import config as cartopy_config
from cartopy.util import add_cyclic_point
import cartopy.crs as ccrs
from flyingpigeon import utils

logger = logging.getLogger(__name__)

os.environ['HOME'] = os.curdir


class MidpointNormalize(Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        self.midpoint = midpoint
        Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))


def plot_polygons()regions):
    """
    extract the polygon coordinate and plot it on a worldmap

    :param regions: list of ISO abreviations for polygons

    :return png: map_graphic.png
    """

    from cartopy.io.shapereader import Reader
    from cartopy.feature import ShapelyFeature
    from os.path import curdir, abspath

    from flyingpigeon import config
    DIR_SHP=config.shapefiles_dir()

    if type(regions) == str:
        regions=list([regions])

    fname=join(DIR_SHP, "countries.shp")
    geos=Reader(fname).geometries()
    records=Reader(fname).records()

    logger.debug('')

    fig = plt.figure(figsize=(10, 10), facecolor='w', edgecolor='k')  # dpi=600,
    projection = ccrs.Orthographic(central_longitude=0.0, central_latitude=0.0, globe=None)  # Robinson()
    ax = plt.axes(projection=projection)

    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            shape_feature = ShapelyFeature(geo, ccrs.PlateCarree(), edgecolor='black')
            ax.add_feature(shape_feature)
        ax.coastlines()
        # ax.set_global()

    o1, map_graphic = mkstemp(dir=abspath(curdir), suffix='.png')
    fig.savefig(map_graphic)
    plt.close()

    return map_graphic

png =   plot_polygons()
