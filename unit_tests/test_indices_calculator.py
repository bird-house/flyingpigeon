import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

from flyingpigeon import indices

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

        # cmip5 ...
        url_parts = urlparse.urlparse(
            TESTDATA['cct_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc'])
        cls.cmip5_historical_1850_nc = url_parts.path

    def test_indices(self):
        nose.tools.ok_( 'SU' in indices.indices(), indices.indices() )

    def test_indices_description(self):
        nose.tools.ok_( 'SU: ' in indices.indices_description(), indices.indices_description() )

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        # SU expects tasmax
        result = indices.calc_indice(
            [self.tasmax_historical_1991_nc], indice='SU', grouping='year', out_dir=out_dir)

        nose.tools.ok_(
            result['drs_filename'] == 'SU_tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day.nc',
            result)
        
        ds = Dataset(result['output'])
        # SU variable must be in result
        nose.tools.ok_('SU' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))

    def test_calc_grouping(self):
        nose.tools.ok_(indices.calc_grouping('year') == ['year'])
        nose.tools.ok_(indices.calc_grouping('month') == ['month'])
        nose.tools.ok_(indices.calc_grouping('sem') == [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] )

        # check invalid value: should raise an exception
        try:
            nose.tools.ok_(indices.calc_grouping('unknown') == ['year'])
            assert False
        except:
            assert True



        
    
