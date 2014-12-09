import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

from flyingpigeon.dispel import indice_workflow

def my_monitor(execution):
    print execution.status
    print execution.percentCompleted
    print execution.statusMessage

class WorkflowTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.tas_2001_nc = TESTDATA["tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc"]
        cls.tas_2006_nc = TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc']
        cls.tasmax_nc = TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc']

    def test_indice_workflow(self):
        result = indice_workflow(
            url=SERVICE,
            resources=[self.tasmax_nc],
            indices=['SU'],
            grouping='year',
            monitor=my_monitor)

        #nose.tools.ok_(False, result)
        
        
        
    
