import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

from __init__ import SERVICE, TESTDATA

def setup():
    pass

@attr('online')   
def test_indices():
    result = wpsclient.execute(
        service = SERVICE,
        identifier = "extractpoints",
        inputs = [('netcdf_file', TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc']),
          ('coords',['2.356138, 48.846450']), 
          ('type_nc', True), 
          ('type_csv', True)],
        outputs = [('tarout', True)],
        verbose = False,
        sleep_secs=2
        )
    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)
    nose.tools.ok_(len(result['processOutputs']) == 2, result)
    
