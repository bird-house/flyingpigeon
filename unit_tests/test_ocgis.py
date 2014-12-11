import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

import tempfile
from netCDF4 import Dataset

class OCGISTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        cls.output_format = 'nc'

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
    def test_ocgis_inspect(self):
        rd = ocgis.RequestDataset(self.tas_nc, 'tas')
        rd.inspect()

    @attr('testdata')
    def test_ocgis_su_tasmax(self):
        out_dir = tempfile.mkdtemp()
        prefix = 'tasmax_su'
        
        calc_icclim = [{'func':'icclim_SU','name':'SU'}]
        rd = ocgis.RequestDataset(self.tasmax_nc, "tasmax") # TODO: time_range=[dt1, dt2]
        SU_file = ocgis.OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=['year'],
            prefix=prefix,
            output_format=self.output_format,
            dir_output=out_dir,
            add_auxiliary_files=False).execute()

        result = Dataset(join(out_dir, prefix + '.' + self.output_format))
        # SU variable must be in result
        nose.tools.ok_('SU' in result.variables, result.variables.keys())
        # 5 years
        nose.tools.ok_(len(result.variables['time']) == 5, len(result.variables['time']))
        
        
    

