import pytest

from .common import WpsTestClient, TESTDATA, assert_response_success

@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_c4i_simple_indice():
    wps = WpsTestClient()
    datainputs = "[files={0};files={1};indiceName=SU;varName=tasmax]".format(
        TESTDATA['cmip5_tasmax_2006_nc'], TESTDATA['cmip5_tasmax_2007_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='wps_c4i_simple_indice',
                   datainputs=datainputs)
    assert_response_success(resp)

