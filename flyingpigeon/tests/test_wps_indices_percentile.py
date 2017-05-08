import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import IndicespercentileProcess


@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_indices_percentile():
    client = client_for(Service(processes=[IndicespercentileProcess()]))
    datainputs = "resource@xlink:href={0};indices=TX;percentile=90".format(TESTDATA['cordex_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='indices_percentile',
        datainputs=datainputs)
    assert_response_success(resp)
