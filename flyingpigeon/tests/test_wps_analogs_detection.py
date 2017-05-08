import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import client_for
from flyingpigeon.processes import processes


@pytest.mark.online
@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_analogs_detection():
    client = client_for(Service(processes=[]))
    datainputs = "dateSt=2013-07-15;dateEn=2013-12-31;refSt=2013-01-01;refEn=2013-12-31"
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='analogs_detection',
        datainputs=datainputs)
    assert_response_success(resp)
