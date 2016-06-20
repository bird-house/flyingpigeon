import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, assert_response_success, TESTDATA

@attr('online')
def test_wps_vbd():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[netcdf_file={0}]".format(TESTDATA['eurocordex_nc_1'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='vbd',
                   datainputs=datainputs)
    assert_response_success(resp)
