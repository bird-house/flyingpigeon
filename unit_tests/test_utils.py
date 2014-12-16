import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

from flyingpigeon import utils

class UtilsTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        # TODO: ocgis does not like file:// urls
        from urllib2 import urlparse
        # tas
        url_parts = urlparse.urlparse(
            TESTDATA["tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_200101-200512.nc"])
        cls.tas_historical_2001_nc = url_parts.path
        url_parts = urlparse.urlparse(
            TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'])
        cls.tas_rcp45_2006_nc = url_parts.path
        
        # tasmax
        url_parts = urlparse.urlparse(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'])
        cls.tasmax_historical_2001_nc = url_parts.path
        
        url_parts = urlparse.urlparse(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc'])
        cls.tasmax_historical_1996_nc = url_parts.path
        
        url_parts = urlparse.urlparse(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc'])
        cls.tasmax_historical_1991_nc = url_parts.path

        # cmip5 ...
        url_parts = urlparse.urlparse(
            TESTDATA['cct_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc'])
        cls.cmip5_historical_1850_nc = url_parts.path

    def test_local_path(self):
        nose.tools.ok_(utils.local_path('file:///tmp/test.nc') == '/tmp/test.nc')
        nose.tools.ok_(utils.local_path('/tmp/test.nc') == '/tmp/test.nc')

    @attr('testdata')
    def test_sort_by_time(self):
        result = utils.sort_by_time([self.tasmax_historical_1991_nc, self.tasmax_historical_1996_nc])
        nose.tools.ok_('19910101' in result[0], result)
        nose.tools.ok_('19960101' in result[1], result)

        result = utils.sort_by_time([self.tasmax_historical_1996_nc, self.tasmax_historical_1991_nc])
        nose.tools.ok_('19910101' in result[0], result)
        nose.tools.ok_('19960101' in result[1], result)

    @attr('testdata')
    def test_get_timestamps(self):
        start,end = utils.get_timestamps(self.tasmax_historical_1991_nc)
        nose.tools.ok_("19910101" == start, start)
        nose.tools.ok_("19951231" == end, end)

    @attr('testdata')
    def test_get_variable(self):
        variable = utils.get_variable(self.tasmax_historical_1991_nc)
        nose.tools.ok_("tasmax" == variable, variable)

        variable = utils.get_variable(self.tas_historical_2001_nc)
        nose.tools.ok_("tas" == variable, variable)

    @attr('testdata')
    def test_drs_filename(self):
        filename = utils.drs_filename(self.tas_historical_2001_nc)
        nose.tools.ok_(
            filename == "tas_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_mon_20010116-20051216.nc", filename)

        filename = utils.drs_filename(self.tasmax_historical_1996_nc)
        nose.tools.ok_(
            filename == "tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc",
            filename)

        # skip timestamp 
        filename = utils.drs_filename(self.tasmax_historical_1996_nc, skip_timestamp=True)
        nose.tools.ok_(
            filename == "tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day.nc",
            filename)

        # cmip5
        filename = utils.drs_filename(self.cmip5_historical_1850_nc)
        nose.tools.ok_(
            filename == "cct_MPI-ESM-LR_historical_r1i1p1_19491216-21051115.nc",
            filename)

    @attr('testdata')
    def test_aggregations(self):
        nc_files = []
        nc_files.append(self.tas_historical_2001_nc)
        nc_files.append(self.tas_rcp45_2006_nc)
        nc_files.append(self.tasmax_historical_1991_nc)
        nc_files.append(self.tasmax_historical_1996_nc)

        aggs = utils.aggregations(nc_files)
        
        nose.tools.ok_(len(aggs) == 3, aggs)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day" in aggs, aggs)
        agg = aggs["tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day"]

        # check aggregation files
        agg_files = agg['files']
        nose.tools.ok_(len(agg_files) == 2, agg)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc" in agg_files[0], agg)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc" in agg_files[1], agg)

        # check timestamps
        nose.tools.ok_(agg['from_timestamp'] == '19910101', agg)
        nose.tools.ok_(agg['to_timestamp'] == '20001231', agg)

        # check variable
        nose.tools.ok_(agg['variable'] == "tasmax", agg)

        # check filename
        nose.tools.ok_(agg['filename'] == 'tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-20001231.nc', agg)

    @attr('testdata')
    def test_has_variable(self):
        nose.tools.ok_(utils.has_variable(self.tasmax_historical_1996_nc, 'tasmax') == True)
        nose.tools.ok_(utils.has_variable(self.tasmax_historical_1996_nc, 'tas') == False)


        
        
        
    
