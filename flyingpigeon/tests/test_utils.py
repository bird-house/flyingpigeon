import pytest

from .common import TESTDATA

import os.path
import tempfile
import tarfile
import zipfile
from netCDF4 import Dataset

from flyingpigeon import utils
from flyingpigeon.utils import local_path


def test_local_path():
    assert local_path('file:///tmp/test.nc') == '/tmp/test.nc'
    assert local_path('/tmp/test.nc') == '/tmp/test.nc'


def test_download_with_cache():
    filename = utils.download(TESTDATA['cmip5_tasmax_2006_nc'], cache=True)
    assert os.path.basename(filename) == 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc'


def test_archive_tar():
    result = utils.archive(
        [local_path(TESTDATA['cmip5_tasmax_2007_nc'])],
        format='tar',
        dir_output=tempfile.mkdtemp())
    tar = tarfile.open(result)
    assert len(tar.getnames()) == 1


def test_archive_zip():
    result = utils.archive(
        [local_path(TESTDATA['cmip5_tasmax_2007_nc'])],
        format='zip',
        dir_output=tempfile.mkdtemp())
    zipf = zipfile.ZipFile(result)
    assert len(zipf.namelist()) == 1


def test_get_frequency():
    freq = utils.get_frequency(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 'mon' == freq


def test_get_values():
    values = utils.get_values(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 12 == len(values)

    values = utils.get_values(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 12 == len(values)

    values = utils.get_values([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                               local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert 23 == len(values)


def test_get_time():
    timestamps = utils.get_time(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 12 == len(timestamps)

    timestamps = utils.get_time(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 12 == len(timestamps)

    values = utils.get_values([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                               local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert 23 == len(values)


def test_unrotate_pole():
    ncs = [local_path(TESTDATA['cordex_tasmax_2006_nc']),
           local_path(TESTDATA['cordex_tasmax_2007_nc'])]
    lats,  lons = utils.unrotate_pole(ncs)
    assert lats.shape == (103, 106)


def test_get_coodinates():
    ncs = [local_path(TESTDATA['cordex_tasmax_2006_nc']),
           local_path(TESTDATA['cordex_tasmax_2007_nc'])]

    lats, lons = utils.get_coordinates(ncs)
    assert 103 == len(lats)
    assert 106 == len(lons)


def test_get_variable():
    variable = utils.get_variable(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 'tasmax' == variable

    variable = utils.get_variable(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 'tasmax' == variable


def test_sort_by_time():
    result = utils.sort_by_time([local_path(TESTDATA['cmip5_tasmax_2007_nc']),
                                 local_path(TESTDATA['cmip5_tasmax_2006_nc'])])
    assert '200601' in result[0]
    assert '200701' in result[1]

# def test_get_timestamps():
#     start,end = utils.get_timestamps(local_path(TESTDATA['cmip5_tasmax_2006_nc']))
#     assert "20060116" == start
#     assert "20061216" == end


def test_get_timerange():
    start, end = utils.get_timerange(local_path(TESTDATA['cmip5_tasmax_2006_nc']))
    assert "20060116" == start
    assert "20061216" == end

    start, end = utils.get_timerange(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert "20070116" == start
    assert "20071216" == end

    start, end = utils.get_timerange([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                                     local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert "20060215" == start
    assert "20071216" == end


def test_drs_filename():
    # cordex
    filename = utils.drs_filename(local_path(TESTDATA['cordex_tasmax_2006_nc']), skip_timestamp=False)
    assert filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_20060215-20061216.nc"

    # cordex ... skip timestamp
    filename = utils.drs_filename(local_path(TESTDATA['cordex_tasmax_2006_nc']), skip_timestamp=True)
    assert filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon.nc"

    # cmip5
    filename = utils.drs_filename(local_path(TESTDATA['cmip5_tasmax_2006_nc']), skip_timestamp=False)
    assert filename == "tasmax_MPI-ESM-MR_RCP4.5_r1i1p1_20060116-20061216.nc"


def test_aggregations():
    nc_files = []
    nc_files.append(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    nc_files.append(local_path(TESTDATA['cmip5_tasmax_2006_nc']))

    aggs = utils.aggregations(nc_files)

    assert len(aggs) == 1
    assert "tasmax_MPI-ESM-MR_RCP4.5_r1i1p1" in aggs
    agg = aggs["tasmax_MPI-ESM-MR_RCP4.5_r1i1p1"]

    # check aggregation files
    agg_files = agg['files']
    assert len(agg_files) == 2
    assert "tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc" in agg_files[0]
    assert "tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200701-200712.nc" in agg_files[1]

    # check timestamps
    assert agg['from_timestamp'] == '20060116'
    assert agg['to_timestamp'] == '20071216'

    # check variable
    assert agg['variable'] == "tasmax"

    # check filename
    assert agg['filename'] == 'tasmax_MPI-ESM-MR_RCP4.5_r1i1p1_20060116-20071216.nc'


def test_has_variable():
    assert utils.has_variable(
        local_path(TESTDATA['cmip5_tasmax_2006_nc']), 'tasmax') is True


def test_calc_grouping():
    assert utils.calc_grouping('year') == ['year']
    assert utils.calc_grouping('month') == ['month']
    assert utils.calc_grouping('sem') == [
        [12, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], 'unique']

    # check invalid value: should raise an exception
    with pytest.raises(Exception) as e_info:
        indices.calc_grouping('unknown') == ['year']
