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

    def test_select_ugid(self):
        from flyingpigeon.region_clipping_calculator import select_ugid
        nose.tools.ok_(select_ugid('AUT') == [17], select_ugid('AUT'))
        nose.tools.ok_(select_ugid('FIN') == [70], select_ugid('FIN'))
        nose.tools.ok_(select_ugid('ITA') == [107], select_ugid('ITA'))
        nose.tools.ok_(select_ugid('unknown') == [], select_ugid('unknown'))

    @attr('testdata')
    def test_calc_region_clipping(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        # SU expects tasmax
        result = region_clipping_calculator.calc_region_clipping(
            [self.tas_rcp45_2006_nc], variable='tas', region='ITA', output_format='nc', out_dir=out_dir)

        nose.tools.ok_(
            result['drs_filename'] == 'SU_tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day.nc',
            result)
        
        ds = Dataset(result['output'])
        # SU variable must be in result
        #nose.tools.ok_('SU' in ds.variables, ds.variables.keys())





        
    
