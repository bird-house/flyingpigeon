import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import GBIFfetchProcess


@pytest.mark.online
# @pytest.mark.skip(reason="no way of currently testing this")
def test_wps_sdm_gbiffetch():
    client = client_for(Service(processes=[IndicespercentileProcess()]))
    datainputs = "taxon_name=Fagus sylvatica;BBox=-10,20,10,40;"
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='sdm_gbiffetch',
        datainputs=datainputs)
    assert_response_success(resp)
