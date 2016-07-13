from .common import WpsTestClient, TESTDATA, assert_response_success

def test_wps_plot_timeseries():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};variable=tasmax]".format(
        TESTDATA['cmip5_tasmax_r1_nc'], TESTDATA['cmip5_tasmax_r2_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='plot_timeseries',
                   datainputs=datainputs)
    assert_response_success(resp)

