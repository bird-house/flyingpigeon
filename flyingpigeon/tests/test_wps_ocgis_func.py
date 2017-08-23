import pytest

from pywps import Service
from pywps.tests import assert_response_success

from common import TESTDATA, client_for
from flyingpigeon.processes import IcclimTXProcess

def test_wps_ocgis_func():
    client = client_for(Service(processes=[IcclimTXProcess()]))
    datainputs = "resource=files@xlink:href={0};grouping={1}".format(TESTDATA['cmip5_tasmax_2006_nc'], 'yr')
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='icclim_TX',
        datainputs=datainputs)
    assert_response_success(resp)

test_wps_ocgis_func()
