import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, assert_response_success

@attr('online')
def test_wps_analogs():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[experiment=NCEP;dateSt=1958-07-15;dateEn=1958-12-31]"
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='analogs',
                   datainputs=datainputs)
    assert_response_success(resp)


