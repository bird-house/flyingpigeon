import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from ocgis import RequestDataset, OcgOperations
from ocgis.util.large_array import compute

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

from flyingpigeon.utils import local_path

def test_ocgis_import():
    from ocgis import constants

def test_cdo_import():
    from cdo import Cdo
    #cdo = Cdo()

class OCGISTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.output_format = 'nc'

        # TODO: ocgis does not like file:// urls
        from urllib2 import urlparse
        # tas
        cls.tas_nc = local_path(
            TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'])
        # tasmax
        cls.tasmax_nc = local_path(
            TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'])
        # eur-11, day
        cls.tasmax_eur11_day_2006_nc = local_path(
            TESTDATA['tasmax_EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_day_20060101-20101231.nc'])

    @attr('testdata')
    def test_ocgis_inspect(self):
        rd = RequestDataset(self.tas_nc, 'tas')
        rd.inspect()

    @attr('testdata')
    @attr('slow')
    def test_ocgis_su_tasmax(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()
        prefix = 'tasmax_su'
        
        calc_icclim = [{'func':'icclim_SU','name':'SU'}]
        rd = RequestDataset(self.tasmax_nc, "tasmax") 
        SU_file = OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=['year'],
            prefix=prefix,
            output_format=self.output_format,
            dir_output=out_dir,
            add_auxiliary_files=False).execute()

        from os.path import join
        result = Dataset(join(out_dir, prefix + '.' + self.output_format))
        # SU variable must be in result
        nose.tools.ok_('SU' in result.variables, result.variables.keys())
        # 5 years
        nose.tools.ok_(len(result.variables['time']) == 5, len(result.variables['time']))

    @attr('testdata')
    @attr('slow')
    def test_ocgis_eur11_day(self):
        #raise SkipTest
        out_dir = tempfile.mkdtemp()
        
        calc = [{'func':'icclim_SU','name':'SU'}]
        results = []
        for year in [2006, 2007, 2008, 2009, 2010]:
            prefix = 'tasmax_su_%s' % year
            rd = RequestDataset(self.tasmax_eur11_day_2006_nc, "tasmax", time_region = {'year':[year]}) 
            ops = OcgOperations(
                dataset=rd,
                calc=calc,
                calc_grouping=['year'],
                prefix=prefix,
                output_format=self.output_format,
                dir_output=out_dir,
                add_auxiliary_files=False)
            results.append(ops.execute())
        from cdo import Cdo
        cdo = Cdo()
        from os.path import join
        output = join(out_dir, "out.nc")
        cdo.mergetime(input=' '.join(results), output=output)

        result = Dataset(output)
        # SU variable must be in result
        nose.tools.ok_('SU' in result.variables, result.variables.keys())
        # 5 years
        nose.tools.ok_(len(result.variables['time']) == 5, len(result.variables['time']))

    @attr('testdata')
    @attr('slow')
    def test_eur11_day_with_compute(self):
        raise SkipTest
        rd = RequestDataset(self.tasmax_eur11_day_2006_nc, variable="tasmax")
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
        
  
