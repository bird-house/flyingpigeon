import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

from __init__ import SERVICE, TESTDATA

def setup():
    pass

@attr('online')   
def test_segetalflora():
    result = wpsclient.execute(
        service = SERVICE,
        identifier = "segetalflora",
        inputs = [('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20510101-20551231.nc']),
                  ],
        outputs = [('out_fieldmeans', True),('out_polygons', True),('out_tas', True)], # ('cvout', True),
        verbose = False,
        sleep_secs=2
                  )

    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)
    nose.tools.ok_(len(result['processOutputs']) == 3, result)
    #nose.tools.ok_(False, result)

    #nose.tools.ok_('txt' in result[0]['reference'], result)
    #content = urllib.urlopen(result[0]['reference']).read()
    #nose.tools.ok_(not ('failed' in content), content)
    
