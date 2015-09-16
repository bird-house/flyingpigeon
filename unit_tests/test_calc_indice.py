import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset
from os.path import basename

from flyingpigeon import indices
from flyingpigeon.utils import local_path

class IndicesCalculatorTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.out_dir = tempfile.mkdtemp()
        # TODO: ocgis does not like file:// urls
        # tas
        ## cls.tas_historical_2001_nc = local_path(
        ##     TESTDATA["tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc"])
        ## cls.tas_historical_1996_nc = local_path(
        ##     TESTDATA["tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19960101-20001231.nc"])
        ## tasmax
        #cls.tasmax_historical_2001_nc = local_path(
            #TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'])
        #cls.tasmax_historical_1996_nc = local_path(
            #TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19960101-20001231.nc'])
        #cls.tasmax_historical_1991_nc = local_path(
            #TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_19910101-19951231.nc'])
        #cls.tasmax_eur11_day_2006_nc = local_path(
            #TESTDATA['tasmax_EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_day_20060101-20101231.nc'])
        #cls.tasmax_eur44_day_2021_nc = local_path(
            #TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20210101-20251231.nc'])
        # cmip5 ...
        #cls.cmip5_historical_1850_nc = local_path(
            #TESTDATA['cct_Amon_MPI-ESM-LR_historical_r1i1p1_185001-200512.nc'])

    def test_indices(self):
        nose.tools.ok_( 'TG' in indices.indices(), indices.indices() )

    def test_indices_description(self):
        nose.tools.ok_( 'TG: ' in indices.indices_description(), indices.indices_description() )

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax(self):
        #raise SkipTest

        # SU expects tasmax
        output = indices.calc_indice(
            [self.tasmax_historical_1991_nc], indice='TG', grouping='year', out_dir=self.out_dir)

        nose.tools.ok_(
            basename(output) == 'TG_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc',
            output)
        
        ds = Dataset(output)
        # SU variable must be in result
        nose.tools.ok_('SU' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax_eur11_day(self):
        #raise SkipTest

        # SU expects tasmax
        output = indices.calc_indice(
            [self.tasmax_eur11_day_2006_nc], indice='SU', grouping='year', out_dir=self.out_dir)

        nose.tools.ok_(
            basename(output) == 'TG_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc',
            output)
        
        ds = Dataset(output)
        # SU variable must be in result
        nose.tools.ok_('TG' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))

    @attr('testdata')
    @attr('slow')
    def test_indice_su_tasmax_eur44_day(self):
        #raise SkipTest

        # SU expects tasmax
        output = indices.calc_indice(
            [self.tasmax_eur44_day_2021_nc], indice='TG', grouping='year', out_dir=self.out_dir)

        nose.tools.ok_(
            basename(output) == 'TG_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc',
            output)
        
        ds = Dataset(output)
        # SU variable must be in result
        nose.tools.ok_('TG' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))


        
    
