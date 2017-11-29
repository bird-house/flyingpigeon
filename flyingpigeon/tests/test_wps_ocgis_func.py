import pytest

from pywps import Service
from pywps.tests import assert_response_success

from common import TESTDATA, client_for
from flyingpigeon.processes.wps_ocgis_func import *

def test_wps_FreezeThaw():
    client = client_for(Service(processes=[FreezeThawProcess(),]))
    datainputs = "resource=files@xlink:href={0};grouping={1};threshold={2}".format(
        TESTDATA['cmip3_tas_sresb1_da_nc'], 'yr', 10.)
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='freezethaw',
        datainputs=datainputs)
    assert_response_success(resp)

def test_wps_ICCLIM_TX():
    client = client_for(Service(processes=[ICCLIM_TXProcess(),]))
    datainputs = "resource=files@xlink:href={0};grouping={1}".format(TESTDATA['cmip3_tas_sresb1_da_nc'], 'yr')
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='icclim_TX',
        datainputs=datainputs)
    assert_response_success(resp)

def test_wps_Duration():
    client = client_for(Service(processes=[Duration(),]))
    datainputs = "resource=files@xlink:href={0};grouping={1};threshold={2};operation={3};summary={4}".format(
        TESTDATA['cmip3_tas_sresb1_da_nc'],
        'yr',
        300.,
        'gt',
        'mean')
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='duration',
        datainputs=datainputs)
    assert_response_success(resp)

def test_wps_ICCLIM_DTR():
    client = client_for(Service(processes=[ICCLIM_DTRProcess(),]))
    datainputs = "tasmax=files@xlink:href={0};tasmin=files@xlink:href={1};grouping={2}".format(
        TESTDATA['cmip3_tasmax_sresa2_da_nc'],
        TESTDATA['cmip3_tasmin_sresa2_da_nc'],
        'mon')
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='icclim_DTR',
        datainputs=datainputs)
    assert_response_success(resp)

pytest.skip("Slow")
def test_wps_ICCLIM_TX10P():
    client = client_for(Service(processes=[ICCLIM_TX10PProcess(),]))
    datainputs = "resource=files@xlink:href={0};grouping={1}".format(
        TESTDATA['cmip3_tas_sresb1_da_nc'],
        'yr')
    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='icclim_TX10p',
        datainputs=datainputs)
    assert_response_success(resp)

#test_wps_FreezeThaw()
#test_wps_Duration()
