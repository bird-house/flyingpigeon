import nose.tools
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

@attr('online')
def test_wps_visualisation():
    wps = WpsTestClient()
    datainputs = "[resource={0};resource={1};variable=slp]".format(
        TESTDATA['noaa_slp_1955_nc'], TESTDATA['noaa_slp_1956_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='visualisation',
                   datainputs=datainputs)
    assert_response_success(resp)

