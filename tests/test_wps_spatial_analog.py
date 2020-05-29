import pytest
import os

from pywps import Service
from pywps.tests import assert_response_success

import numpy as np
import datetime as dt
from shapely.geometry import Point

import ocgis
from ocgis import OcgOperations
from ocgis.collection.field import Field
from ocgis.variable.base import Variable
from ocgis.spatial.grid import Grid
from ocgis.variable.temporal import TemporalVariable
from ocgis.base import get_variable_names
from ocgis.test.base import TestBase

from flyingpigeon.utils import local_path
from flyingpigeon.processes import SpatialAnalogProcess, PlotSpatialAnalogProcess
from .common import TESTDATA, client_for, CFG_FILE


@pytest.mark.skip("race condition")
class TestDissimilarity(TestBase):
    """Simple auto-generated test field."""
    def setUp(self):
        super(self.__class__, self).setUp()

    def tearDown(self):
        super(self.__class__, self).tearDown()

    def write_field_data(self, variable_name, path='a', nrow=2, ncol=2):
        path = self.get_temporary_file_path('{0}_{1}.nc'.format(
            path, variable_name))
        field = self.get_field(variable_name=variable_name, ntime=365,
                               nrow=nrow, ncol=ncol)
        field.write(path)
        return path

    def get_field(self, ntime=2, variable_name='foo', nrow=2, ncol=2):
        """Create random field where mean varies with radius and std with the
        angle around the center of the grid.
        """
        np.random.seed(1)

        row = Variable(value=np.arange(nrow) - nrow / 2., name='row', dimensions='row')
        col = Variable(value=np.arange(ncol) - ncol / 2., name='col', dimensions='col')

        grid = Grid(col, row)
        x, y = grid.get_value_stacked()

        start = dt.datetime(2000, 1, 1)
        delta = dt.timedelta(days=1)
        value_temporal = [start + i * delta for i in range(ntime)]

        temporal = TemporalVariable(value=value_temporal, dimensions='time',
                                    name='time')

        nlevel = 1
        level = None

        nrlz = 1
        realization = None
        value = np.random.rand(nrlz, ntime, nlevel, nrow, ncol) * np.arctan2(x, y).clip(.1) + np.hypot(x, y)
        variable = Variable(name=variable_name,
                            value=value,
                            dimensions=['realization', 'time', 'level', 'row',
                                        'col'])
        field = Field(grid=grid, time=temporal, is_data=variable, level=level,
                      realization=realization)

        return field

    def test1d(self):
        p1 = self.write_field_data('v1', ncol=1, nrow=1)
        p3 = self.write_field_data('v1', path='b')

        ref_range = [dt.datetime(2000, 3, 1), dt.datetime(2000, 3, 31)]
        reference = ocgis.RequestDataset(p1, time_range=ref_range).get()

        cand_range = [dt.datetime(2000, 8, 1), dt.datetime(2000, 8, 31)]
        candidate = ocgis.RequestDataset(p3, time_range=cand_range)

        calc = [{'func': 'dissimilarity',
                 'name': 'output_1d',
                 'kwds': {'target': reference,
                          'candidate': ('v1',)}}]

        ops = OcgOperations(dataset=candidate, calc=calc)
        ret = ops.execute()
        actual_field = ret.get_element()
        actual_variables = get_variable_names(actual_field.data_variables)
        self.assertEqual(actual_variables[0], 'dissimilarity')
        dist = actual_field['dissimilarity']
        self.assertEqual(dist.shape, (1, 1, 2, 2))

    def test_full(self):
        """Compute the dissimilarity will all metrics."""
        from flyingpigeon import dissimilarity
        from matplotlib import pyplot as plt
        import flyingpigeon as fp
        from flyingpigeon.config import Paths
        paths = Paths(fp)

        p1 = self.write_field_data('v1', ncol=1, nrow=1)
        p2 = self.write_field_data('v2', ncol=1, nrow=1)
        p3 = self.write_field_data('v1', ncol=11, nrow=10, path='c')
        p4 = self.write_field_data('v2', ncol=11, nrow=10, path='c')

        ref_range = [dt.datetime(2000, 3, 1), dt.datetime(2000, 3, 31)]
        ref = [ocgis.RequestDataset(p, time_range=ref_range) for p in [p1, p2]]
        reference = ocgis.MultiRequestDataset(ref)
        reference = reference.get()

        cand_range = [dt.datetime(2000, 8, 1), dt.datetime(2000, 8, 31)]
        can = [ocgis.RequestDataset(p, time_range=cand_range) for p in [p3,
                                                                        p4]]
        candidate = ocgis.MultiRequestDataset(can)

        fig, axes = plt.subplots(2, 3)
        for i, dist in enumerate(dissimilarity.__all__):

            calc = [{'func': 'dissimilarity',
                     'name': 'output_mfpf',
                     'kwds': {'target': reference,
                              'candidate': ('v1', 'v2'),
                              'dist': dist}}]

            ops = OcgOperations(dataset=candidate, calc=calc)
            ret = ops.execute()
            out_field = ret.get_element()
            var_name = get_variable_names(out_field.data_variables)[0]
            out = out_field[var_name].get_value()[0, 0]
            axes.flat[i].imshow(out)
            axes.flat[i].set_title(dist)

        path = os.path.join(paths.outputpath, 'test_spatial_analog_metrics.png')
        fig.savefig(path)
        plt.close()

    def test_simple(self):
        p1 = self.write_field_data('v1', ncol=1, nrow=1)
        p2 = self.write_field_data('v2', ncol=1, nrow=1)
        p3 = self.write_field_data('v1', path='b')
        p4 = self.write_field_data('v2', path='b')

        ref_range = [dt.datetime(2000, 3, 1), dt.datetime(2000, 3, 31)]
        ref = [ocgis.RequestDataset(p, time_range=ref_range) for p in [p1, p2]]
        reference = ocgis.MultiRequestDataset(ref)
        reference = reference.get()

        cand_range = [dt.datetime(2000, 8, 1), dt.datetime(2000, 8, 31)]
        can = [ocgis.RequestDataset(p, time_range=cand_range) for p in [p3, p4]]
        candidate = ocgis.MultiRequestDataset(can)

        calc = [{'func': 'dissimilarity',
                 'name': 'output_mfpf',
                 'kwds': {'target': reference,
                          'candidate': ('v1', 'v2')}}]

        ops = OcgOperations(dataset=candidate, calc=calc)
        ret = ops.execute()
        actual_field = ret.get_element()
        actual_variables = get_variable_names(actual_field.data_variables)
        self.assertEqual(actual_variables[0], 'dissimilarity')
        dist = actual_field['dissimilarity']
        self.assertEqual(dist.shape, (1, 1, 2, 2))


def test_dissimilarity_op():
    """Test with a real file."""
    lon, lat = -72, 46
    g = Point(lon, lat)

    cfn = local_path(TESTDATA['indicators_small_nc'])
    tfn = local_path(TESTDATA['indicators_medium_nc'])

    indices = ['meantemp', 'totalpr']

    # Candidate fields
    candidate = ocgis.RequestDataset(cfn,
                                     variable=indices,
                                     time_range=[dt.datetime(1970, 1, 1), dt.datetime(2000, 1, 1)],
                                     )

    # The indicators_small dataset is just a subset of the indicators_medium
    # dataset. Below is the code to create the small dataset.
    # Running the test with the full file takes about 2 minutes, so we'll
    # crop the data to 4 grid cells.

    # op = ocgis.OcgOperations(dataset=crd, geom=g,
    #                          select_nearest=False, search_radius_mult=1.75,
    #                          output_format='nc',
    #                          output_format_options={'data_model': 'NETCDF4'},
    #                          dir_output='/tmp', prefix='indicators_small'
    #                          )
    # res = op.execute()

    # Target fields
    # Extract values from one grid cell
    trd = ocgis.RequestDataset(tfn,
                               variable=indices,
                               time_range=[dt.datetime(1970, 1, 1),
                                           dt.datetime(2000, 1, 1)],
                               )

    op = ocgis.OcgOperations(dataset=trd, geom=g,
                             search_radius_mult=1.75, select_nearest=True)
    target = op.execute().get_element()

    ops = ocgis.OcgOperations(
        calc=[{'func': 'dissimilarity', 'name': 'spatial_analog',
               'kwds': {'dist': 'seuclidean', 'target': target,
                        'candidate': indices}}],
        dataset=candidate
    )

    res = ops.execute()
    out = res.get_element()
    val = out['dissimilarity'].get_value()
    i = np.argmin(np.abs(out['lon'].get_value() - lon))
    j = np.argmin(np.abs(out['lat'].get_value() - lat))
    np.testing.assert_almost_equal(val[j, i], 0, 6)
    np.testing.assert_array_equal(val > 0, True)


def test_wps_spatial_analog_process_small_sample():
    client = client_for(Service(processes=[SpatialAnalogProcess()]))
    datainputs = "candidate=files@xlink:href={c};" \
                 "target=files@xlink:href={t};" \
                 "location={lon},{lat};" \
                 "indices={i1};indices={i2};" \
                 "dist={dist};" \
                 "dateStartCandidate={start};" \
                 "dateEndCandidate={end};" \
                 "dateStartTarget={start};" \
                 "dateEndTarget={end}"\
        .format(c=TESTDATA['indicators_small_nc'],
                t=TESTDATA['indicators_medium_nc'],
                lon=-72,
                lat=46,
                i1='meantemp',
                i2='totalpr',
                dist='seuclidean',
                start=dt.datetime(1970, 1, 1),
                end=dt.datetime(1990, 1, 1))

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='spatial_analog',
        datainputs=datainputs)
    assert_response_success(resp)


def test_wps_plot_spatial_analog():
    client = client_for(
        Service(processes=[PlotSpatialAnalogProcess()], cfgfiles=CFG_FILE))
    datainputs = (
        "resource=files@xlink:href={0};"
        "fmt={1};fmt={2};fmt={3};fmt={4};"
        "title={5}"
    ).format(TESTDATA['dissimilarity_nc'], 'png', 'pdf', 'svg', 'eps', "Spatial Analog Example")

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='plot_spatial_analog',
        datainputs=datainputs)

    assert_response_success(resp)


@pytest.mark.skip("slow")
def test_wps_spatial_analog_process():
    client = client_for(Service(processes=[SpatialAnalogProcess()]))
    datainputs = "candidate=files@xlink:href={c};" \
                 "target=files@xlink:href={t};" \
                 "location={lon},{lat};" \
                 "indices={i1};indices={i2};" \
                 "dist={dist};" \
                 "dateStartCandidate={start};" \
                 "dateEndCandidate={end};" \
                 "dateStartTarget={start};" \
                 "dateEndTarget={end}"\
        .format(t=TESTDATA['indicators_small_nc'],
                c=TESTDATA['indicators_medium_nc'],
                lon=-72,
                lat=46,
                i1='meantemp',
                i2='totalpr',
                dist='seuclidean',
                start=dt.datetime(1970, 1, 1),
                end=dt.datetime(1990, 1, 1))

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='spatial_analog',
        datainputs=datainputs)
    assert_response_success(resp)
