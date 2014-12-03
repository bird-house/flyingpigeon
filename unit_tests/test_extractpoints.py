import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

from __init__ import SERVICE, TESTDATA

def setup():
    pass

@attr('online')   
def test_extractpoints():
    result = wpsclient.execute(
        service = SERVICE,
        identifier = "extractpoints",
        inputs = [('netcdf_file', TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc']),
          ('coords','2.356138, 48.846450'),
          #('coords','3.356138, 49.846450'), 2nd point
          ('type_nc', 'True'), 
          ('type_csv', 'True')],
        outputs = [('tarout', True)],
        verbose = False,
        sleep_secs=2
        )
    # was the process successfull
    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)

    # ... never trust the status ;) we look at the results:
    
    # we expect only one result (tarout)
    nose.tools.ok_(len(result['processOutputs']) == 1, result)

    # open remote result file
    import requests
    url = result['processOutputs'][0]['reference']
    response = requests.get(url)

    # check size of files in remote tar file
    import tarfile
    from StringIO import StringIO
    tar = tarfile.open(fileobj=StringIO(response.content))
    nose.tools.ok_(len(tar.getmembers()) == 3, "num members should be 3, is: %d" % len(tar.getmembers()))
    for member in tar.getmembers():
        if member.isfile():
            nose.tools.ok_(member.size > 0, "content %s has zero size" % member.name)
