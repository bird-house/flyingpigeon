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

    
