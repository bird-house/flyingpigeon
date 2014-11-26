import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr


def test_ocgis():
    raise SkipTest
    import ocgis

    ncfile = 'examples/test1.nc'
    rd = ocgis.RequestDataset(ncfile, 'tas')
    rd.inspect()
    

