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
        'analogs_detection',
        'analogs_viewer',
        'fetch',
        'indices_percentile',
        'indices_simple',
        'plot_timeseries',
        'sdm_csv',
        'sdm_gbifsearch',
        'segetalflora',
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

