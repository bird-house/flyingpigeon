import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

from flyingpigeon import clipping

class ClippingTestCase(TestCase):

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
        # cmip5 ...
        url_parts = urlparse.urlparse(
            TESTDATA['cct_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc'])
        cls.cmip5_historical_1850_nc = url_parts.path
        # pr cordex ...
        url_parts = urlparse.urlparse(
            TESTDATA['pr_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20110101-20151231.nc'])
        cls.pr_rcp85_2011_nc = url_parts.path

    def test_select_ugid(self):
        from flyingpigeon.clipping import select_ugid
        nose.tools.ok_(select_ugid('AUT') == [17], select_ugid('AUT'))
        nose.tools.ok_(select_ugid('FIN') == [70], select_ugid('FIN'))
        nose.tools.ok_(select_ugid('ITA') == [107], select_ugid('ITA'))
        nose.tools.ok_(select_ugid('unknown') == [], select_ugid('unknown'))

    @attr('testdata')
    def test_calc_region_clipping_nc_cmip5(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        result = clipping.calc_region_clipping(
            [self.cmip5_historical_1850_nc], region='ITA', output_format='nc', out_dir=out_dir)

        nose.tools.ok_(
            result['drs_filename'] == 'cct_MPI-ESM-LR_historical_r1i1p1_19491216-21051115.nc',
            result)
        
        ds = Dataset(result['output'])
        nose.tools.ok_('cct' in ds.variables, ds.variables.keys())

    @attr('testdata')
    def test_calc_region_clipping_csv_cmip5(self):
        out_dir = tempfile.mkdtemp()

        result = clipping.calc_region_clipping(
            [self.cmip5_historical_1850_nc], region='ITA', output_format='csv', out_dir=out_dir)

        # TODO: check names and content
        nose.tools.ok_(
            result['drs_filename'] == 'cct_MPI-ESM-LR_historical_r1i1p1_19491216-21051115.nc',
            result)

        nose.tools.ok_(
            'ITA.csv' in result['output'],
            result)

    @attr('testdata')
    def test_calc_region_clipping_nc_cordex(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()

        result = clipping.calc_region_clipping(
            [self.pr_rcp85_2011_nc], region='FRA', output_format='nc', out_dir=out_dir)

        nose.tools.ok_(
            result['drs_filename'] == 'pr_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20110101-20151231.nc',
            result)
        
        ds = Dataset(result['output'])
        nose.tools.ok_('pr' in ds.variables, ds.variables.keys())

    def test_proj_cordex_1(self):
        #raise SkipTest
        import subprocess
        cmd = ['proj',
               '-f', '"%.6f"',
               '-m', '57.2957795130823',
               '+proj=ob_tran',
               '+o_proj=latlon',
               '+o_lon_p=-162.0',
               '+o_lat_p=39.25',
               '+lon_0=180',
               self.pr_rcp85_2011_nc
               ]
        subprocess.check_output(cmd)

    def test_proj_cordex_2(self):
        #raise SkipTest
        import subprocess
        import shlex
        cmd = "proj +proj=omerc +lat_0=0 +lonc=0 +alpha=0 +k=1 +x_0=0 +y_0=0 +gamma=0 +ellps=WGS84 +units=m +no_defs"
        subprocess.check_output(shlex.split(cmd))
        
        






        
    
