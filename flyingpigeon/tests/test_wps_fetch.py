from .common import WpsTestClient, TESTDATA, assert_response_success


def test_wps_fetch():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1}]".format(
        TESTDATA['cmip5_tasmax_2006_nc'], TESTDATA['cmip5_tasmax_2007_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='fetch',
                   datainputs=datainputs)
    assert_response_success(resp)
