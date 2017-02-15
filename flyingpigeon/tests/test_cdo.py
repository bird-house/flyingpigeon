import pytest

from .common import TESTDATA
from flyingpigeon.utils import local_path

from cdo import Cdo
cdo = Cdo()


def test_sinfo():
    cdo.sinfo(input=local_path(TESTDATA['cmip5_tasmax_2006_nc']))
