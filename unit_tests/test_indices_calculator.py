import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

# ocgis needs GDAL_DATA on import
import os
from os.path import join
# TODO: set GDAL_DATA in a save way
os.environ['GDAL_DATA'] = join(os.environ['HOME'], 'anaconda', 'share', 'gdal')
import ocgis

from flyingpigeon import indices_calculator

class IndicesCalculatorTestCase(TestCase):

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

    def test_indices(self):
        nose.tools.ok_( 'SU' in indices_calculator.indices(), indices_calculator.indices() )

    def test_indices_description(self):
        nose.tools.ok_( 'SU: ' in indices_calculator.indices_description(), indices_calculator.indices_description() )

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        # SU expects tasmax
        result = indices_calculator.calc_indice(
            self.tasmax_historical_1991_nc, indice='SU', grouping='year', out_dir=out_dir)

        ds = Dataset(result)
        # SU variable must be in result
        nose.tools.ok_('SU' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))

    def test_calc_grouping(self):
        nose.tools.ok_(indices_calculator.calc_grouping('year') == ['year'])
        nose.tools.ok_(indices_calculator.calc_grouping('month') == ['month'])
        nose.tools.ok_(indices_calculator.calc_grouping('sem') == [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] )

        # check invalid value: should raise an exception
        try:
            nose.tools.ok_(indices_calculator._calc_grouping('unknown') == ['year'])
            assert False
        except:
            assert True

    @attr('testdata')
    def test_sort_by_time(self):
        result = indices_calculator.sort_by_time([self.tasmax_historical_1991_nc, self.tasmax_historical_1996_nc])
        nose.tools.ok_('19910101' in result[0], result)
        nose.tools.ok_('19960101' in result[1], result)

        result = indices_calculator.sort_by_time([self.tasmax_historical_1996_nc, self.tasmax_historical_1991_nc])
        nose.tools.ok_('19910101' in result[0], result)
        nose.tools.ok_('19960101' in result[1], result)

    @attr('testdata')
    def test_group_by_experiment(self):
        nc_files = []
        nc_files.append(self.tas_historical_2001_nc)
        nc_files.append(self.tas_rcp45_2006_nc)
        nc_files.append(self.tasmax_historical_1991_nc)
        nc_files.append(self.tasmax_historical_1996_nc)
        result = indices_calculator.group_by_experiment(nc_files)
        
        nose.tools.ok_(len(result) == 3, result)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day" in result, result)
        group = result["tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day"]
        nose.tools.ok_(len(group) == 2, result)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc" in group[0], result)
        nose.tools.ok_("tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc" in group[1], result)

    @attr('testdata')
    def test_has_variable(self):
        nose.tools.ok_(indices_calculator.has_variable(self.tasmax_historical_1996_nc, 'tasmax') == True)
        nose.tools.ok_(indices_calculator.has_variable(self.tasmax_historical_1996_nc, 'tas') == False)


        
        
        
    
