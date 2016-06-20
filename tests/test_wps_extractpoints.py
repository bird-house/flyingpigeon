import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import WpsTestClient, assert_response_success, TESTDATA

def test_wps_extractpoints():
    raise SkipTest
    wps = WpsTestClient()
    datainputs = "[netcdf_file={0};coords=2.356138,48.846450;type_nc=True;type_csv=True]".format(TESTDATA['cmip5_tasmax_nc'])
    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='extractpoints',
                   datainputs=datainputs)
    assert_response_success(resp)
