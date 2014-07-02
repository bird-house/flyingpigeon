import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr


def test_icclim():
    import icclim
    nose.tools.ok_(icclim.__version__ >= '0.0.1', icclim.__version__)
