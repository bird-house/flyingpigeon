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
            TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'])
        cls.tas_nc = url_parts.path
        # tasmax
        url_parts = urlparse.urlparse(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'])
        cls.tasmax_nc = url_parts.path

    @attr('testdata')
    def test_indice_su_tasmax(self):
        out_dir = tempfile.mkdtemp()

        result = indices_calculator.calc_indice(
            self.tasmax_nc, indice='SU', variable='tasmax', out_dir=out_dir)

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
        
        
    
