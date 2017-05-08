import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA, client_for
from flyingpigeon.processes import PlottimeseriesProcess


@pytest.mark.online
@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_plot_timeseries():
    client = client_for(Service(processes=[PlottimeseriesProcess()]))
    datainputs = "resource@xlink:href={0};variable=tasmax".format(TESTDATA['cmip5_tasmax_2006_nc'])
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='plot_timeseries',
        datainputs=datainputs)
    assert_response_success(resp)
