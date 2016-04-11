import nose.tools
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

def test_wps_ensemble_robustness():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};start=2006;end=2006;timeslice=1;variable=tasmax]".format(
        TESTDATA['cmip5_tasmax_r1_nc'],
        TESTDATA['cmip5_tasmax_r2_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='ensembleRobustness',
                   datainputs=datainputs)
    assert_response_success(resp)

