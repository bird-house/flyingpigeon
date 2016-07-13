import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from tests.common import prepare_env, TESTDATA
prepare_env()

import os
import tempfile
from netCDF4 import Dataset

from flyingpigeon import indices
from flyingpigeon.utils import local_path

class IndicesCalculatorTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        pass
        
    def test_indices(self):
        nose.tools.ok_( 'TG' in indices.indices(), indices.indices() )

    def test_indices_description(self):
        nose.tools.ok_( 'TG: ' in indices.indices_description(), indices.indices_description() )

    def test_indice_su_tasmax(self):
        raise SkipTest
        out_dir = tempfile.mkdtemp()
        # SU expects tasmax
        output = indices.calc_indice_single(
            [local_path(TESTDATA['cordex_tasmax_nc'])],
            indices=['SU'], groupings='year', dir_output=out_dir)

        nose.tools.ok_(
            os.path.basename(output) == 'TG_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc',
            output)
        
        ds = Dataset(output)
        # SU variable must be in result
        nose.tools.ok_('SU' in ds.variables, ds.variables.keys())
        # 5 years
        nose.tools.ok_(len(ds.variables['time']) == 5, len(ds.variables['time']))




        
    
