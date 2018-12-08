import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for, CFG_FILE
try:
    from flyingpigeon.processes import FetchProcess
except Exception:
    pytestmark = pytest.mark.skip


def test_wps_fetch():
    client = client_for(Service(processes=[FetchProcess()], cfgfiles=CFG_FILE))
    datainputs = "resource=files@xlink:href={0}".format(TESTDATA['cmip5_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='fetch_resources',
        datainputs=datainputs)
    assert_response_success(resp)
