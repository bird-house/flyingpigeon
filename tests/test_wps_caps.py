from pywps import Service
from pywps.tests import assert_response_success

from .common import client_for
from flyingpigeon.processes import processes


def test_wps_caps():
    client = client_for(Service(processes=processes))
    resp = client.get(service='wps', request='getcapabilities', version='1.0.0')
    names = resp.xpath_text('/wps:Capabilities'
                            '/wps:ProcessOfferings'
                            '/wps:Process'
                            '/ows:Identifier')
    assert sorted(names.split()) == [
        'map_spatial_analog',
        'plot_timeseries',
        'pointinspection',
        'spatial_analog',
        'subset',
        'subset_WFS',
        'subset_bbox',
        'subset_continents',
        'subset_countries']
