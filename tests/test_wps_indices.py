import nose.tools
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

@attr('online')
def test_wps_indices_single():
    wps = WpsTestClient()
    datainputs = "[resource={0};indices=SU]".format(TESTDATA['eurocordex_nc_1'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='indices_single',
                   datainputs=datainputs)
    assert_response_success(resp)
