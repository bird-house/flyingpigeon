import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

import tempfile
from netCDF4 import Dataset

from ocgis import RequestDataset, OcgOperations
from ocgis.util.large_array import compute

from flyingpigeon.utils import local_path

from tests.common import prepare_env, TESTDATA
prepare_env()

def test_ocgis_import():
    from ocgis import constants

def test_cdo_import():
    from cdo import Cdo
    cdo = Cdo()

class OCGISTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        pass

    def test_ocgis_inspect(self):
        rd = RequestDataset(local_path(TESTDATA['cordex_tasmax_nc']), 'tasmax')
        rd.inspect()

    def test_ocgis_su_tasmax(self):
        raise SkipTest
        out_dir = tempfile.mkdtemp()
        prefix = 'tasmax_su'
        
        calc_icclim = [{'func':'icclim_SU','name':'SU'}]
        rd = RequestDataset(local_path(TESTDATA['cordex_tasmax_nc']), "tasmax") 
        SU_file = OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=['year'],
            prefix=prefix,
            output_format='nc',
            dir_output=out_dir,
            add_auxiliary_files=False).execute()

        from os.path import join
        result = Dataset(join(out_dir, prefix + '.' + self.output_format))
        # SU variable must be in result
        nose.tools.ok_('SU' in result.variables, result.variables.keys())
        # 5 years
        nose.tools.ok_(len(result.variables['time']) == 5, len(result.variables['time']))

    def test_eur44_mon_with_compute(self):
        raise SkipTest
        rd = RequestDataset(local_path(TESTDATA['cordex_tasmax_nc']), variable="tasmax")
        calc = [{'func':'icclim_SU','name':'SU'}]
        calc_grouping = ['year']
        out_dir = tempfile.mkdtemp()
 
        # output must be netCDF. otherwise, all operations should be accepted. it could be fragile with some things, so
        # me know if you encounter any issues.
        ops = OcgOperations(
            dataset=rd,
            calc=calc,
            calc_grouping=calc_grouping,
            output_format='nc',
            dir_output=out_dir)
 
        # this is an estimate of the request size (in kilobytes) which could be useful.
        print ops.get_base_request_size()['total']
 
        # the tile dimension splits data into squares. edge effects are handled.
        tile_dimension = 20
        path_nc = compute(ops, tile_dimension, verbose=True)
        
  
