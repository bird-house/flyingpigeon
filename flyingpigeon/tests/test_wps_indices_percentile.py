import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import IndicespercentiledaysProcess


@pytest.mark.skip(reason="ocgis fails in this test")
def test_wps_indices_percentiledays():
    client = client_for(Service(processes=[IndicespercentiledaysProcess()]))
    datainputs = "resource@xlink:href={0};percentile=90".format(TESTDATA['cordex_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='indices_percentiledays',
        datainputs=datainputs)
    assert_response_success(resp)
