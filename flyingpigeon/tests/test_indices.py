import pytest

from .common import TESTDATA

import os
import tempfile
from netCDF4 import Dataset

from flyingpigeon import indices
from flyingpigeon.utils import local_path
  
def test_indices():
    assert 'TG' in indices.indices()

def test_indices_description():
    assert 'TG: ' in indices.indices_description()

@pytest.mark.skip(reason="no way of currently testing this")
def test_indice_su_tasmax():
    # SU expects tasmax
    output = indices.calc_indice_single(
        [local_path(TESTDATA['cordex_tasmax_2006_nc'])],
        indices=['SU'], groupings='year', dir_output=tempfile.mkdtemp())

    assert os.path.basename(output) == 'TG_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day.nc'

    ds = Dataset(output)
    # SU variable must be in result
    assert 'SU' in ds.variables
    # 5 years
    assert len(ds.variables['time']) == 5




        
    
