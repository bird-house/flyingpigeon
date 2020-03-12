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
        'climatechange_signal',
        'plot_map_timemean',
        'plot_spaghetti',
        'plot_spatial_analog',
        'plot_uncertainty',
        'plot_uncertaintyrcp',
        'pointinspection',
        'robustness_statistic',
        'spatial_analog',
        'subset-wfs-polygon',
        'subset_bbox',
        'subset_continents',
        'subset_countries',
    ]
