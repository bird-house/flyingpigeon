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

class MultipleIndicesTestCase(WpsTestCase):

    @attr('online')
    @attr('testdata')
    def test_su_tasmax(self):
        inputs = []
        inputs.append(('indice', 'SU'))
        inputs.append(('indice', 'TG'))
        inputs.append(('grouping', 'year'))
        inputs.append(
            ('resource',
             TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc']
             ))
        inputs.append(
            ('resource',
             TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc']
             ))

        output=[('output', True)]
        execution = self.wps.execute(identifier="multiple_indices", inputs=inputs, output=output)
        monitorExecution(execution, sleepSecs=1)

        nose.tools.ok_(execution.status == 'ProcessSucceeded', execution.status)

        # TODO: check contents

    @attr('online')
    @attr('testdata')
    @attr('slow')
    def test_eur11_day(self):
        #raise SkipTest
        inputs = []
        inputs.append(('indice', 'SU'))
        inputs.append(('indice', 'TG'))
        inputs.append(('grouping', 'year'))
        inputs.append(
            ('resource',
             TESTDATA['tasmax_EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_day_20060101-20101231.nc']
             ))
        inputs.append(
            ('resource',
             TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc']
             ))
        inputs.append(
            ('resource',
             TESTDATA['tas_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc']
             ))

        output=[('output', True)]
        execution = self.wps.execute(identifier="multiple_indices", inputs=inputs, output=output)
        monitorExecution(execution, sleepSecs=1)

        nose.tools.ok_(execution.status == 'ProcessSucceeded', execution.status)

        # TODO: check contents





