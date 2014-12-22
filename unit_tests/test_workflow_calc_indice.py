import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from os.path import basename, join

from __init__ import TESTDATA, SERVICE

class WorkflowTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.nc_files = []
        cls.nc_files.append( TESTDATA["tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc"] )
        cls.nc_files.append( TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'] )
        cls.nc_files.append( TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'] )
        #cls.nc_files.append( TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc'] )
        #cls.nc_files.append( TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc'] )

    def test_indice_workflow(self):
        import tempfile
        from flyingpigeon.workflow import calc_indice

        out_dir = tempfile.mkdtemp()
        
        result = calc_indice(
            resources=self.nc_files,
            indices=['SU', 'TG'],
            grouping='year',
            monitor=None,
            out_dir=out_dir)

        nose.tools.ok_(len(result) == 3, result)
        with open(join(out_dir, "status.log")) as fp:
            status_log = fp.readlines()
            nose.tools.ok_(len(status_log) == 3, status_log)

        wanted = [
            "TG_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon.nc",
            "TG_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon.nc",
            "SU_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day.nc",
        ]

        for filename in result:
            nose.tools.ok_(basename(filename) in wanted, filename)
        
        
        
        
    
