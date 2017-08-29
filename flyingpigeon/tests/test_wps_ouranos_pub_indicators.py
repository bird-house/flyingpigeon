import pytest

from pywps import Service
from pywps.tests import assert_response_success

from common import TESTDATA, client_for
from flyingpigeon.processes.wps_ouranos_pub_indicators import OuranosPublicIndicatorProcess

def test_wps_OuranosPublicIndicatorProcess():
    client = client_for(Service(processes=[OuranosPublicIndicatorProcess(),]))
    datainputs = "tas=files@xlink:href={0};tasmin=files@xlink:href={1};tasmax=files@xlink:href={2};pr=files@xlink:href={3};grouping={4};".format(
                     TESTDATA['cmip3_tas_sresa2_da_nc'],
                     TESTDATA['cmip3_tasmin_sresa2_da_nc'],
                     TESTDATA['cmip3_tasmax_sresa2_da_nc'],
                     TESTDATA['cmip3_pr_sresa2_da_nc'],
                     'yr',)

    resp = client.get(
        service='WPS', request='Execute', version='1.0.0',
        identifier='ouranos_public_indicators',
        datainputs=datainputs)
    1/0
    assert_response_success(resp)
