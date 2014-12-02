import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

import __init__ as base

import json
import tempfile
import urllib

def setup():
    pass

@attr('online')
def test_visualisation():
    raise SkipTest

    result = wpsclient.execute(
    service = base.SERVICE,
    identifier = "visualisation",
    inputs = [('file_identifier', 'http://localhost:8080/thredds/fileServer/test/nils.hempelmann@hzg.de/tas_MNA-44_ICHEC-EC-EARTH_historical_r12i1p1_SMHI-RCA4_v1_day_20010101-20051231.nc')],
    outputs = [('output', True)],
    verbose=True
    ) 

    nose.tools.ok_('html' in result, result)

@attr('online')
def test_extractpoints():
    raise SkipTest
    result = wpsclient.execute(
        service = base.SERVICE,
        identifier = "extractpoints",
        inputs = [('file_identifier', 'http://localhost:8090/thredds/fileServer/test/nils.hempelmann@hzg.de/tasmax_day_MPI-ESM-LR_historical_r1i1p1_20040101-20051231.nc'),
        ('SU','True'),
        ], #http://localhost:8090/thredds/fileServer/test/nils.hempelmann_hzg.de/tasmax_EUR11_test-pywpsInputbtel_q.nc
        outputs = [('output', True)],
        verbose=False
        )

    nose.tools.ok_('txt' in result[0]['reference'], result)
    content = urllib.urlopen(result[0]['reference']).read()
    nose.tools.ok_(not ('failed' in content), content)
    
