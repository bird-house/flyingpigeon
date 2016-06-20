import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

def test_wps_indices_single():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[resource={0};indices=SU]".format(TESTDATA['cordex_tasmax_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='indices_single',
                   datainputs=datainputs)
    assert_response_success(resp)
