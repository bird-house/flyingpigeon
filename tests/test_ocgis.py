import pytest

from .common import TESTDATA

from flyingpigeon import ocgis_module
from flyingpigeon.utils import local_path


def test_ocgis_import():
    from ocgis import constants


def test_cdo_import():
    from cdo import Cdo
    cdo = Cdo()


def test_has_Lambert_Conformal():
    has_lambert = ocgis_module.has_Lambert_Conformal(
        [local_path(TESTDATA['cordex_tasmax_2006_nc']),
         local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert False == has_lambert


def test_gdal():
    from flyingpigeon.subset import clipping
