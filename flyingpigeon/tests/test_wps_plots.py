from .common import WpsTestClient, TESTDATA, assert_response_success


def test_wps_plot_timeseries():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};variable=tasmax]".format(
        TESTDATA['cmip5_tasmax_2006_nc'], TESTDATA['cmip5_tasmax_2007_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='plot_timeseries',
                   datainputs=datainputs)
    assert_response_success(resp)


def wps_plot_libs():
    import matplotlib
    matplotlib.use('Agg')   # use this if no xserver is available

    from matplotlib import pyplot as plt
    from matplotlib.colors import Normalize
    from cartopy import config as cartopy_config
    from cartopy.util import add_cyclic_point
    import cartopy.crs as ccrs
    from cartopy.io.shapereader import Reader
    from cartopy.feature import ShapelyFeature

    from flyingpigeon import config
    DIR_SHP = config.shapefiles_dir()
