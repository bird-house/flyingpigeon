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

class SimpleIndiceTestCase(WpsTestCase):

    @attr('online')
    @attr('testdata')
    def test_tx_tasmax(self):
        inputs = []
        inputs.append(('indice', 'TX'))
        inputs.append(('grouping', 'ONDJFM'))
        inputs.append(
            ('resource',
             TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19710101-19751231.nc']
             ))

        output=[('output', True)]
        execution = self.wps.execute(identifier="simple_indice", inputs=inputs, output=output)
        monitorExecution(execution, sleepSecs=1)
        nose.tools.ok_(execution.status == 'ProcessSucceeded', execution.status)
