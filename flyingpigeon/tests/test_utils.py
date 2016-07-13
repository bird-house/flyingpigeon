import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import prepare_env, TESTDATA
prepare_env()

import os.path
import tempfile
import tarfile
import zipfile
from netCDF4 import Dataset

from flyingpigeon import utils

class UtilsTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.resources = []
        cls.resources.append( utils.local_path(TESTDATA['cmip5_tasmax_2006_nc']) )
        cls.resources.append( utils.local_path(TESTDATA['cmip5_tasmax_2007_nc']) )

    def test_download_with_cache(self):
        filename = utils.download(TESTDATA['cmip5_tasmax_2006_nc'], cache=True)
        assert os.path.basename(filename) == 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc'

    def test_archive_tar(self):
        result = utils.archive(self.resources, format='tar', dir_output=tempfile.mkdtemp())
        tar = tarfile.open(result)
        assert len(tar.getnames()) == 2

    def test_archive_zip(self):
        result = utils.archive(self.resources, format='zip', dir_output=tempfile.mkdtemp())
        zipf = zipfile.ZipFile(result)
        assert len(zipf.namelist()) == 2
    
    def test_local_path(self):
        nose.tools.ok_(utils.local_path('file:///tmp/test.nc') == '/tmp/test.nc')
        nose.tools.ok_(utils.local_path('/tmp/test.nc') == '/tmp/test.nc')

    def test_sort_by_time(self):
        result = utils.sort_by_time( [utils.local_path(TESTDATA['cmip5_tasmax_2007_nc']),
                                      utils.local_path(TESTDATA['cmip5_tasmax_2006_nc'])] )
        nose.tools.ok_('200601' in result[0], result)
        nose.tools.ok_('200701' in result[1], result)

        
    def test_get_timestamps(self):
        start,end = utils.get_timestamps(utils.local_path(TESTDATA['cmip5_tasmax_nc']))
        nose.tools.ok_("20060116" == start, start)
        nose.tools.ok_("20061216" == end, end)

        
    def test_get_variable(self):
        variable = utils.get_variable(utils.local_path(TESTDATA['cmip5_tasmax_nc']))
        nose.tools.ok_("tasmax" == variable, variable)


    def test_drs_filename(self):
        # cordex
        filename = utils.drs_filename(utils.local_path(TESTDATA['cordex_tasmax_nc']))
        nose.tools.ok_(filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_20060215-20061216.nc", filename)

        # cordex ... skip timestamp
        filename = utils.drs_filename(utils.local_path(TESTDATA['cordex_tasmax_nc']), skip_timestamp=True)
        nose.tools.ok_(filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon.nc", filename)
        
        # cmip5
        filename = utils.drs_filename(utils.local_path(TESTDATA['cmip5_tasmax_nc']))
        nose.tools.ok_(filename == "tasmax_MPI-ESM-MR_RCP4.5_r1i1p1_20060116-20061216.nc", filename)

    def test_aggregations(self):
        nc_files = []
        nc_files.append(utils.local_path(TESTDATA['cmip5_tasmax_2007_nc']))
        nc_files.append(utils.local_path(TESTDATA['cmip5_tasmax_2006_nc']))

        aggs = utils.aggregations(nc_files)
        
        nose.tools.ok_(len(aggs) == 1, len(aggs))
        nose.tools.ok_("tasmax_MPI-ESM-MR_RCP4.5_r1i1p1" in aggs, aggs)
        agg = aggs["tasmax_MPI-ESM-MR_RCP4.5_r1i1p1"]

        # check aggregation files
        agg_files = agg['files']
        nose.tools.ok_(len(agg_files) == 2, agg)
        nose.tools.ok_("tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc" in agg_files[0], agg)
        nose.tools.ok_("tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200701-200712.nc" in agg_files[1], agg)

        # check timestamps
        nose.tools.ok_(agg['from_timestamp'] == '20060116', agg['from_timestamp'])
        nose.tools.ok_(agg['to_timestamp'] == '20071216', agg['to_timestamp'])

        # check variable
        nose.tools.ok_(agg['variable'] == "tasmax", agg['variable'])

        # check filename
        nose.tools.ok_(agg['filename'] == 'tasmax_MPI-ESM-MR_RCP4.5_r1i1p1_20060116-20071216.nc', agg['filename'])

    def test_has_variable(self):
        nose.tools.ok_(utils.has_variable(utils.local_path(TESTDATA['cmip5_tasmax_nc']), 'tasmax') == True)

        
    def test_calc_grouping(self):
        nose.tools.ok_(utils.calc_grouping('year') == ['year'])
        nose.tools.ok_(utils.calc_grouping('month') == ['month'])
        nose.tools.ok_(utils.calc_grouping('sem') == [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] )

        # check invalid value: should raise an exception
        try:
            nose.tools.ok_(indices.calc_grouping('unknown') == ['year'])
            assert False
        except:
            assert True

        
        
        
    
