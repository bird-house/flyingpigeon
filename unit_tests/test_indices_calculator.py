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
        cls.tas_2001_nc = url_parts.path
        url_parts = urlparse.urlparse(
            TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'])
        cls.tas_2006_nc = url_parts.path
        # tasmax
        url_parts = urlparse.urlparse(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'])
        cls.tasmax_nc = url_parts.path

    def test_indices(self):
        nose.tools.ok_( 'SU' in indices_calculator.indices(), indices_calculator.indices() )

    def test_indices_description(self):
        nose.tools.ok_( 'SU: ' in indices_calculator.indices_description(), indices_calculator.indices_description() )

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        result = indices_calculator.calc_indice(
            self.tasmax_nc, indice='SU', variable='tasmax', grouping='year', out_dir=out_dir)

        ds = Dataset(result)
        # SU variable must be in result
        nose.tools.ok_('SU' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))

    def test_calc_grouping(self):
        nose.tools.ok_(indices_calculator._calc_grouping('year') == ['year'])
        nose.tools.ok_(indices_calculator._calc_grouping('month') == ['month'])
        nose.tools.ok_(indices_calculator._calc_grouping('sem') == [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] )

        # check invalid value: should raise an exception
        try:
            nose.tools.ok_(indices_calculator._calc_grouping('unknown') == ['year'])
            assert False
        except:
            assert True

    def test_sort_by_time(self):
        result = indices_calculator._sort_by_time([self.tas_2001_nc, self.tas_2006_nc])
        nose.tools.ok_('200101' in result[0], result)
        nose.tools.ok_('200601' in result[1], result)
        
        result = indices_calculator._sort_by_time([self.tas_2006_nc, self.tas_2001_nc])
        nose.tools.ok_('200101' in result[0], result)
        nose.tools.ok_('200601' in result[1], result)

        
        
        
    
