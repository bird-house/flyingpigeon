import nose.tools
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

@attr('online')
def test_wps_ensemble_robustness():
    wps = WpsTestClient()
    datainputs = "[resource={0};start=1980;end=2010]".format(TESTDATA['eurocordex_nc_1'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='ensembleRobustness',
                   datainputs=datainputs)
    assert_response_success(resp)

