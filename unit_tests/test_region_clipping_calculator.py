import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

from flyingpigeon import region_clipping_calculator

class RegionClippingCalculatorTestCase(TestCase):

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


    @attr('testdata')
    def test_calc_region_clipping(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        # SU expects tasmax
        result = region_clipping_calculator.calc_region_clipping(
            [self.tasmax_historical_1991_nc], variable='tasmax', region='ITA', output_format='nc', out_dir=out_dir)

        nose.tools.ok_(
            result['drs_filename'] == 'SU_tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day.nc',
            result)
        
        ds = Dataset(result['output'])
        # SU variable must be in result
        #nose.tools.ok_('SU' in ds.variables, ds.variables.keys())





        
    
