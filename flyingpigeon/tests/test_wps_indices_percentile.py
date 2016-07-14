import pytest

from .common import WpsTestClient, TESTDATA, assert_response_success

@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_indices_percentile():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};indices=TX;percentile=90]".format(
        TESTDATA['cordex_tasmax_2006_nc'], TESTDATA['cordex_tasmax_2007_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='indices_percentile',
                   datainputs=datainputs)
    assert_response_success(resp)

