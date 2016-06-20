import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, TESTDATA, assert_response_success

@attr('online')
def test_wps_segetalflora():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[netcdf_file={0}]".format(TESTDATA['eurocordex_nc_1'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='segetalflora',
                   datainputs=datainputs)
    assert_response_success(resp)

