import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import prepare_env
prepare_env()

import tempfile
from netCDF4 import Dataset

from flyingpigeon import utils

class UtilsTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        pass
        
    def test_local_path(self):
        nose.tools.ok_(utils.local_path('file:///tmp/test.nc') == '/tmp/test.nc')
        nose.tools.ok_(utils.local_path('/tmp/test.nc') == '/tmp/test.nc')

    @attr('testdata')
    def test_sort_by_time(self):
        raise SkipTest
        result = utils.sort_by_time([self.tas_historical_2001_nc, self.tas_historical_1996_nc])
        nose.tools.ok_('19960101' in result[0], result)
        nose.tools.ok_('20010101' in result[1], result)

        #result = utils.sort_by_time([self.tasmax_historical_1996_nc, self.tasmax_historical_1991_nc])
        #nose.tools.ok_('19910101' in result[0], result)
        #nose.tools.ok_('19960101' in result[1], result)

    @attr('testdata')
    def test_get_timestamps(self):
        raise SkipTest
        start,end = utils.get_timestamps(self.tas_historical_2001_nc)
        nose.tools.ok_("20010101" == start, start)
        nose.tools.ok_("20051231" == end, end)

    @attr('testdata')
    def test_get_variable(self):
        raise SkipTest
        variable = utils.get_variable(self.tas_historical_2001_nc)
        nose.tools.ok_("tas" == variable, variable)

        #variable = utils.get_variable(self.tas_historical_2001_nc)
        #nose.tools.ok_("tas" == variable, variable)

    @attr('testdata')
    def test_drs_filename(self):
        raise SkipTest
        filename = utils.drs_filename(self.tas_historical_2001_nc)
        nose.tools.ok_(
            filename == "tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc", filename)

        #filename = utils.drs_filename(self.tas_historical_1996_nc)
        #nose.tools.ok_(
            #filename == "tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc",
            #filename)

        # skip timestamp 
        filename = utils.drs_filename(self.tas_historical_2001_nc, skip_timestamp=True)
        nose.tools.ok_(
            filename == "tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc",
            filename)

        ## cmip5
        #filename = utils.drs_filename(self.cmip5_historical_1850_nc)
        #nose.tools.ok_(
            #filename == "cct_MPI-ESM-LR_historical_r1i1p1_19491216-21051115.nc",
            #filename)

    @attr('testdata')
    def test_aggregations(self):
        raise SkipTest
        nc_files = []
        nc_files.append(self.tas_historical_2001_nc)
        nc_files.append(self.tas_historical_1996_nc)
        #nc_files.append(self.tasmax_historical_1991_nc)
        #nc_files.append(self.tasmax_historical_1996_nc)

        aggs = utils.aggregations(nc_files)
        
        nose.tools.ok_(len(aggs) == 3, aggs)
        nose.tools.ok_("tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19960101-20001231.nc" in aggs, aggs)
        agg = aggs["tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc"]

        # check aggregation files
        agg_files = agg['files']
        nose.tools.ok_(len(agg_files) == 2, agg)
        nose.tools.ok_("tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc" in agg_files[0], agg)
        nose.tools.ok_("tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc" in agg_files[1], agg)

        # check timestamps
        nose.tools.ok_(agg['from_timestamp'] == '20010101', agg)
        nose.tools.ok_(agg['to_timestamp'] == '20051231', agg)

        # check variable
        nose.tools.ok_(agg['variable'] == "tas", agg)

        # check filename
        nose.tools.ok_(agg['filename'] == 'tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-20001231.nc', agg)

    @attr('testdata')
    def test_has_variable(self):
        raise SkipTest
        nose.tools.ok_(utils.has_variable(self.tasmax_historical_2001_nc, 'tas') == True)
        nose.tools.ok_(utils.has_variable(self.tasmax_historical_2001_nc, 'tasmax') == False)

    def test_calc_grouping(self):
        raise SkipTest
        nose.tools.ok_(utils.calc_grouping('year') == ['year'])
        nose.tools.ok_(utils.calc_grouping('month') == ['month'])
        nose.tools.ok_(utils.calc_grouping('sem') == [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] )

        # check invalid value: should raise an exception
        try:
            nose.tools.ok_(indices.calc_grouping('unknown') == ['year'])
            assert False
        except:
            assert True

        
        
        
    
