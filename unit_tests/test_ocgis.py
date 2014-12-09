import nose.tools
from unittest import TestCase
from nose import SkipTest
from nose.plugins.attrib import attr

from __init__ import TESTDATA, SERVICE

class OCGISTestCase(TestCase):

    @classmethod
    def setUpClass(cls):
        import os
        from os.path import join
        # TODO: set GDAL_DATA in a save way
        os.environ['GDAL_DATA'] = join(os.environ['HOME'], 'anaconda', 'share/gdal')
        from urllib2 import urlparse
        url_parts = urlparse.urlparse(
            TESTDATA['tas_EUR-11_ICHEC-EC-EARTH_rcp45_r1i1p1_KNMI-RACMO22E_v1_mon_200601-201012.nc'])
        cls.tas_nc = url_parts.path

    @attr('testdata')
    def test_ocgis_inspect(self):
        import ocgis
        rd = ocgis.RequestDataset(self.tas_nc, 'tas')
        rd.inspect()
        

    

