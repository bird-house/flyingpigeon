import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import IndicessingleProcess


@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_indices_simple():
    client = client_for(Service(processes=[IndicessingleProcess()]))
    datainputs = "resource@xlink:href={0};indices=SU".format(TESTDATA['cordex_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='indices_simple',
        datainputs=datainputs)
    assert_response_success(resp)
