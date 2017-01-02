import pytest

from .common import WpsTestClient, TESTDATA, assert_response_success


@pytest.mark.online
# @pytest.mark.skip(reason="no way of currently testing this")
def test_wps_sdm_gbiffetch():
    wps = WpsTestClient()
    datainputs = "[taxon_name=Fagus sylvatica;BBox=-10,20,10,40;]"
    resp = wps.get(service='wps', request='execute', version='1.0.0',
                   identifier='sdm_gbiffetch',
                   datainputs=datainputs)
    assert_response_success(resp)
