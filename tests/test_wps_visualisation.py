import nose.tools
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

def test_wps_visualisation():
    wps = WpsTestClient()
    datainputs = "[resource={0};variable=tasmax]".format(TESTDATA['cmip5_tasmax_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='visualisation',
                   datainputs=datainputs)
    assert_response_success(resp)

