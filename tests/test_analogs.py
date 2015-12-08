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
            ('dateSt','1958-07-15'),
            ('dateEn','1958-12-31'),
            #('refSt', '1955-01-01'),
            #('refEn', '1957-12-31'),
        ],
        outputs = [('ncout', True), ('tarout', True), ('Rlogout', True)],
        verbose=False,
        sleep_secs=120,
        )

    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)
    nose.tools.ok_(len(result['processOutputs']) == 3, result)
    #nose.tools.ok_(False, result)
    
