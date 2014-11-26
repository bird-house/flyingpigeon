import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

import __init__ as base

def setup():
    pass

@attr('online')   
def test_analogs():
    result = wpsclient.execute(
        service = base.SERVICE,
        identifier = "analogs",
        inputs = [
            ('experiment', 'NCEP'),
            ('dateSt','2013-07-15'),
            ('dateEn','2013-12-31'),
            ('refSt', '1955-01-01'),
            ('refEn', '1957-12-31'),
        ],
        outputs = [('ncout', True), ('tarout', True), ('Rlogout', True)],
        verbose=False
        )

    nose.tools.ok_(len(result) == 3, result)
    nose.tools.ok_(False, result)
    
