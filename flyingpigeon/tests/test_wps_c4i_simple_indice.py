import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for


@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_c4i_simple_indice():
    client = client_for(Service(processes=[]))
    datainputs = "files@xlink:href={0};indiceName=SU;varName=tasmax".format(TESTDATA['cmip5_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='wps_c4i_simple_indice',
        datainputs=datainputs)
    assert_response_success(resp)
