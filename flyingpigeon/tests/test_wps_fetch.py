import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import FetchProcess


@pytest.mark.online
@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_fetch():
    client = client_for(Service(processes=[FetchProcess()]))
    datainputs = "resource@xlink:href={0}".format(TESTDATA['cmip5_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='fetch',
        datainputs=datainputs)
    assert_response_success(resp)
