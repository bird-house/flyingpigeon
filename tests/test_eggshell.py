import pytest

from .common import TESTDATA

from os.path import basename, join
import tempfile
import tarfile
import zipfile
from netCDF4 import Dataset

from eggshell import utils
from eggshell.utils import local_path
from eggshell.nc import ocg_utils
from eggshell.nc import nc_utils
from eggshell.config import Paths
import flyingpigeon as fp
paths = Paths(fp)


def test_Paths():
    assert "flyingpigeon/tests/testdata" in paths.testdata
    assert 'flyingpigeon/data' in paths.data
    assert 'flyingpigeon/data/shapefiles' in paths.shapefiles


def test_local_path():
    assert local_path('file:///tmp/test.nc') == '/tmp/test.nc'
    assert local_path('/tmp/test.nc') == '/tmp/test.nc'


def test_ocgis_import():
    from ocgis import constants


# def test_has_Lambert_Conformal():
#     has_lambert = ocg_utils.has_Lambert_Conformal(
#         [local_path(TESTDATA['cordex_tasmax_2006_nc']),
#          local_path(TESTDATA['cordex_tasmax_2007_nc'])])
#     assert False == has_lambert


def test_gdal():
    from flyingpigeon.subset import clipping


@pytest.mark.skip(reason="no way of currently testing this")
def test_download_with_cache():
    filename = utils.download(TESTDATA['cmip5_tasmax_2006_nc'], cache=paths.cache)
    assert basename(filename) == 'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc'


def test_archive_tar():
    result = utils.archive(local_path(TESTDATA['cmip5_tasmax_2006_nc']),
                           format='tar',
                           output_dir=tempfile.mkdtemp())
    tar = tarfile.open(result)
    assert len(tar.getnames()) == 1

#  [local_path(TESTDATA['cmip5_tasmax_2007_nc'])],


def test_archive_zip():
    result = utils.archive(local_path(TESTDATA['cmip5_tasmax_2006_nc']),
        format='zip',
        output_dir=tempfile.mkdtemp())
    zipf = zipfile.ZipFile(result)
    assert len(zipf.namelist()) == 1


def test_get_frequency():
    freq = nc_utils.get_frequency(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 'mon' == freq


def test_get_values():
    values = nc_utils.get_values(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 12 == len(values)

    values = nc_utils.get_values(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 12 == len(values)

    values = nc_utils.get_values([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                               local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert 23 == len(values)


def test_get_time():
    timestamps = nc_utils.get_time(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 12 == len(timestamps)

    timestamps = nc_utils.get_time(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 12 == len(timestamps)

    values = nc_utils.get_values([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                               local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert 23 == len(values)


@pytest.mark.skip(reason="no way of currently testing this")
def test_unrotate_pole():
    ncs = [local_path(TESTDATA['cordex_tasmax_2006_nc']),
           local_path(TESTDATA['cordex_tasmax_2007_nc'])]
    lats, lons = nc_utils.unrotate_pole(ncs)
    assert lats.shape == (103, 106)


def test_get_index_lat():
    ncs = [local_path(TESTDATA['cordex_tasmax_2006_nc']),
           local_path(TESTDATA['cordex_tasmax_2007_nc'])]
    index = nc_utils.get_index_lat(ncs)
    assert 1 == index
    index = nc_utils.get_index_lat(ncs[0])
    assert 1 == index
    index = nc_utils.get_index_lat(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 1 == index


def test_get_coordinates():
    ncs = [local_path(TESTDATA['cordex_tasmax_2006_nc']),
           local_path(TESTDATA['cordex_tasmax_2007_nc'])]

    lats, lons = nc_utils.get_coordinates(ncs, unrotate=False)

    assert 1 == len(lats.shape)

    lats, lons = nc_utils.get_coordinates(ncs)
    assert 103 == len(lats)
    assert 106 == len(lons)


def test_get_variable():
    variable = ocg_utils.get_variable(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    assert 'tasmax' == variable
    variable = ocg_utils.get_variable(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert 'tasmax' == variable


def test_sort_by_time():
    result = nc_utils.sort_by_time([local_path(TESTDATA['cmip5_tasmax_2007_nc']),
                                 local_path(TESTDATA['cmip5_tasmax_2006_nc'])])
    assert '200601' in result[0]
    assert '200701' in result[1]

# def test_get_timestamps():
#     start,end = nc_utils.get_timestamps(local_path(TESTDATA['cmip5_tasmax_2006_nc']))
#     assert "20060116" == start
#     assert "20061216" == end


def test_get_timerange():
    start, end = nc_utils.get_timerange(local_path(TESTDATA['cmip5_tasmax_2006_nc']))
    assert "20060116" == start
    assert "20061216" == end

    start, end = nc_utils.get_timerange(local_path(TESTDATA['cordex_tasmax_2007_nc']))
    assert "20070116" == start
    assert "20071216" == end

    start, end = nc_utils.get_timerange([local_path(TESTDATA['cordex_tasmax_2006_nc']),
                                     local_path(TESTDATA['cordex_tasmax_2007_nc'])])
    assert "20060215" == start
    assert "20071216" == end


def test_drs_filename():
    # cordex
    filename = nc_utils.drs_filename(local_path(TESTDATA['cordex_tasmax_2006_nc']), skip_timestamp=False)
    assert filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_20060215-20061216.nc"

    # cordex ... skip timestamp
    filename = nc_utils.drs_filename(local_path(TESTDATA['cordex_tasmax_2006_nc']), skip_timestamp=True)
    assert filename == "tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon.nc"

    # cmip5
    filename = nc_utils.drs_filename(local_path(TESTDATA['cmip5_tasmax_2006_nc']), skip_timestamp=False)
    assert filename == "tasmax_MPI-ESM-MR_RCP4.5_r1i1p1_20060116-20061216.nc"


def test_aggregations():
    nc_files = []
    nc_files.append(local_path(TESTDATA['cmip5_tasmax_2007_nc']))
    nc_files.append(local_path(TESTDATA['cmip5_tasmax_2006_nc']))

    aggs = nc_utils.aggregations(nc_files)

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


def get_coordinates(resource, variable=None, unrotate=False):
    """
    reads out the coordinates of a variable
    :param resource: netCDF resource file
    :param variable: variable name
    :param unrotate: If True the coordinates will be returned for unrotated pole
    :returns list, list: latitudes , longitudes
    """
    if type(resource) != list:
        resource = [resource]

    if variable is None:
        variable = get_variable(resource)

    if unrotate is False:
        try:
            if len(resource) > 1:
                ds = MFDataset(resource)
            else:
                ds = Dataset(resource[0])

            var = ds.variables[variable]
            dims = list(var.dimensions)
            if 'time' in dims: dims.remove('time')
            # TODO: find position of lat and long in list and replace dims[0] dims[1]
            lats = ds.variables[dims[0]][:]
            lons = ds.variables[dims[1]][:]
            ds.close()
            LOGGER.info('got coordinates without pole rotation')
        except Exception:
            msg = 'failed to extract coordinates'
            LOGGER.exception(msg)
    else:
        lats, lons = unrotate_pole(resource)
        LOGGER.info('got coordinates with pole rotation')
    return lats, lons


def test_has_variable():
    assert ocg_utils.has_variable(
        local_path(TESTDATA['cmip5_tasmax_2006_nc']), 'tasmax') is True


def test_calc_grouping():
    assert ocg_utils.calc_grouping('year') == ['year']
    assert ocg_utils.calc_grouping('month') == ['month']
    assert ocg_utils.calc_grouping('sem') == [
        [12, 1, 2], [3, 4, 5], [6, 7, 8], [9, 10, 11], 'unique']

    # check invalid value: should raise an exception
    with pytest.raises(Exception) as e_info:
        indices.calc_grouping('unknown') == ['year']
