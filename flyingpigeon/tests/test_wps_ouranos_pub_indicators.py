import pytest

from pywps import Service
from pywps.tests import assert_response_success

import numpy as np
from numpy.testing import assert_array_almost_equal as aaae

from common import TESTDATA, client_for
from flyingpigeon.processes.wps_ouranos_pub_indicators import OuranosPublicIndicatorProcess
from ocgis import RequestDataset, OcgOperations, env

#@pytest.mark.skip
def test_wps_OuranosPublicIndicatorProcess():
    env.OVERWRITE = True
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

    assert_response_success(resp)



def test_ocgis_average():
    v1 = TESTDATA['cmip3_tasmin_sresa2_da_nc'][6:]
    v2 = TESTDATA['cmip3_tasmax_sresa2_da_nc'][6:]

    rd1 = RequestDataset(v1)
    rd2 = RequestDataset(v2)

    ops = OcgOperations([rd1, rd2], calc=[{'func':'average', 'name':'tas', 'kwds':{'v1':'tasmin', 'v2':'tasmax'}}])
    ret  = ops.execute()
    t = ret.get_element()['tas'][0,:,:].get_value()

    t1 = rd1.get_field()['tasmin'][0, :, :].get_value()
    t2 = rd2.get_field()['tasmax'][0, :, :].get_value()
    aaae(np.mean([t1,t2], axis=0), t)





