import pytest

from .common import WpsTestClient, TESTDATA, assert_response_success


@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_indices_simple():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};indices=SU]".format(
        TESTDATA['cordex_tasmax_2006_nc'], TESTDATA['cordex_tasmax_2007_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0',
                   identifier='indices_simple',
                   datainputs=datainputs)
    assert_response_success(resp)
