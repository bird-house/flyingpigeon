import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, assert_response_success

@attr('online')
def test_wps_eobs_to_cordex():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[var_eobs=tx]"
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='eobs_to_cordex',
                   datainputs=datainputs)
    assert_response_success(resp)


