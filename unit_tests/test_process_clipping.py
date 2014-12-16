import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

from netCDF4 import Dataset
import urllib2
from owslib.wps import monitorExecution

class WpsTestCase(TestCase):
    """
    Base TestCase class, sets up a wps
    """

    @classmethod
    def setUpClass(cls):
        from owslib.wps import WebProcessingService
        cls.wps = WebProcessingService(SERVICE, verbose=False, skip_caps=False)

class SimpleClippingTestCase(WpsTestCase):

    @attr('online')
    @attr('testdata')
    def test_eur11_fra(self):
        inputs = []
        inputs.append(('region', 'FRA'))
        inputs.append(
            ('resource',
             TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc']
             ))

        output=[('output', True)]
        execution = self.wps.execute(identifier="clipping", inputs=inputs, output=output)
        monitorExecution(execution, sleepSecs=1)

        nose.tools.ok_(execution.status == 'ProcessSucceeded', execution.status)

        # TODO: check contents





