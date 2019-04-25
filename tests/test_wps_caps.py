from pywps import Service
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
        'plot_timeseries',
        'pointinspection',
        'subset-bbox',
        'subset-wfs-polygon',
        'subset_continents',
        'subset_countries',
        ]
