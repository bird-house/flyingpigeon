import pytest

from .common import WpsTestClient, TESTDATA, assert_response_success


@pytest.mark.online
@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_analogs_detection():
    wps = WpsTestClient()
    datainputs = "[dateSt=2013-07-15;dateEn=2013-12-31;refSt=2013-01-01;refEn=2013-12-31]"
    resp = wps.get(service='wps', request='execute', version='1.0.0',
                   identifier='analogs_detection',
                   datainputs=datainputs)
    assert_response_success(resp)
