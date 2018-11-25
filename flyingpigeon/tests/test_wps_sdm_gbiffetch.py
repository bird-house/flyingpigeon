import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import client_for, CFG_FILE
try:
    from flyingpigeon.processes import GBIFfetchProcess
except Exception:
    pytestmark = pytest.mark.skip


@pytest.mark.skip  # This is much too long. I suggest we create a test that requires less data to be downloaded.
@pytest.mark.slow
@pytest.mark.online
def test_wps_sdm_gbiffetch():
    client = client_for(Service(processes=[GBIFfetchProcess()], cfgfiles=CFG_FILE))
    datainputs = "taxon_name=Fagus sylvatica;BBox=-10,20,10,40;"  # BBox is not supported in the process.
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='sdm_gbiffetch',
        datainputs=datainputs)
    assert_response_success(resp)
