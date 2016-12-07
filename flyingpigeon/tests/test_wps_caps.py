from .common import WpsTestClient


def test_caps():
    wps = WpsTestClient()
    resp = wps.get(service='wps', request='getcapabilities')
    names = resp.xpath_text('/wps:Capabilities'
                            '/wps:ProcessOfferings'
                            '/wps:Process'
                            '/ows:Identifier')
    sorted_names = sorted(names.split())
    expected_names = [
        'analogs_compare',
        'analogs_detection',
        'analogs_model',
        'analogs_viewer',
        'fetch',
        'indices_percentile',
        'indices_simple',
        'plot_timeseries',
        'robustness',
        'sdm_allinone',
        'sdm_csv',
        'sdm_csvindices',
        'sdm_gbiffetch',
        'sdm_getindices',
        'segetalflora',
        'spatial_analog',
        'subset_continents',
        'subset_countries',
        'subset_points',
        'subset_regionseurope',
        'weatherregimes_model',
        'weatherregimes_projection',
        'weatherregimes_reanalyse',
        'wps_c4i_simple_indice',
    ]

    assert sorted_names == expected_names
