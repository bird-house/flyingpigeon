import pytest

from pywps import Service
from pywps.tests import assert_response_success

from .common import TESTDATA
import ocgis
import numpy as np

from ocgis import RequestDataset, OcgOperations
from ocgis.collection.field import Field
from ocgis.variable.base import Variable
import datetime as dt
from ocgis.test.base import TestBase
from shapely.geometry import Point
from ocgis.spatial.grid import Grid
from ocgis.variable.temporal import TemporalVariable

from flyingpigeon.utils import local_path

class TestDissimilarity(TestBase):
    """Simple auto-generated test field."""
    def setUp(self):
        super(self.__class__, self).setUp()

    def tearDown(self):
        super(self.__class__, self).tearDown()

    def write_field_data(self, variable_name, dir='a', nrow=2, ncol=2):
        path = self.get_temporary_file_path('{0}_{1}.nc'.format(
            dir, variable_name))
        field = self.get_field(variable_name=variable_name, ntime=365,
                               nrow=nrow, ncol=ncol)
        field.write(path)
        return path

    def get_field(self, ntime=2, variable_name='foo', nrow=2, ncol=2):
        np.random.seed(1)

        row = Variable(value=range(4, 4+nrow), name='row', dimensions='row')
        col = Variable(value=range(40, 40+ncol), name='col', dimensions='col')

        grid = Grid(col, row)

        start = dt.datetime(2000, 1, 1)
        delta = dt.timedelta(days=1)
        value_temporal = [start + i*delta for i in range(ntime)]

        temporal = TemporalVariable(value=value_temporal, dimensions='time',
                                    name='time')

        nlevel = 1
        level = None

        nrlz = 1
        realization = None

        variable = Variable(name=variable_name,
                            value=np.random.rand(nrlz, ntime, nlevel, nrow,
                                                 ncol),
                            dimensions=['realization', 'time', 'level', 'row',
                                        'col'])
        field = Field(grid=grid, time=temporal, is_data=variable, level=level,
                         realization=realization)

        return field

    def test(self):
        from ocgis.base import get_variable_names

        p1 = self.write_field_data('v1', ncol=1, nrow=1)
        p2 = self.write_field_data('v2', ncol=1, nrow=1)
        p3 = self.write_field_data('v1', dir='b')
        p4 = self.write_field_data('v2', dir='b')

        ref_range = [dt.datetime(2000, 3, 1), dt.datetime(2000, 3, 31)]
        ref = [ocgis.RequestDataset(p, time_range=ref_range) for p in [p1,
                                                                   p2]]
        reference = ocgis.MultiRequestDataset(ref)
        reference = reference.get()

        cand_range = [dt.datetime(2000, 8, 1), dt.datetime(2000, 8, 31)]
        can = [ocgis.RequestDataset(p, time_range=cand_range) for p in [p3,
                                                                       p4]]
        candidate = ocgis.MultiRequestDataset(can)

        calc = [{'func': 'dissimilarity',
                 'name': 'output_mfpf',
                 'kwds': {'target': reference,
                          'candidate': ('v1', 'v2')}}]

        ops = OcgOperations(dataset=candidate, calc=calc)
        ret = ops.execute()
        actual_field = ret.get_element()
        actual_variables = get_variable_names(actual_field.data_variables)
        self.assertEqual(actual_variables[0],
                         ('dissimilarity_seuclidean'))
        dist = actual_field['dissimilarity_seuclidean']
        self.assertEqual(dist.shape, (1, 1, 2, 2))


def test_dissimilarity_op():
    """Test with a real file."""
    import datetime as dt
    lon, lat = -72, 46
    g = Point(-72, 46)

    rfn = local_path(TESTDATA['reference_indicators.nc'] )

    indices = ['meantemp', 'totalpr']

    # Candidate fields
    crd = ocgis.RequestDataset(rfn,
                variable=indices,
                time_range=[dt.datetime(1970, 1, 1), dt.datetime(2000, 1, 1)],
                )
    # Running the test with the full file takes about 2 minutes, so we'll
    # crop the data to 4 grid cells.
    op = ocgis.OcgOperations(dataset=crd, geom=g, select_nearest=False)
    res = op.execute()
    candidate = res.get_element()

    # Reference fields
    # Extract values from one grid cell
    op = ocgis.OcgOperations(dataset=crd, geom=g, select_nearest=True)
    target = op.execute().get_element()

    ops = ocgis.OcgOperations(
        calc=[{'func': 'dissimilarity', 'name': 'spatial_analog',
               'kwds': {'dist': 'seuclidean', 'target': target,
                        'candidate': indices}}],
                dataset=candidate)

    res = ops.execute()
    out = res.get_element()
    i = np.argmin(np.abs(out['lon'].value-lon))
    j = np.argmin(np.abs(out['lat'].value-lat))
    np.testing.assert_almost_equal( out['dissimilarity_seuclidean'].value[
        j,i], 0, 6)


@pytest.mark.online
@pytest.mark.skip(reason="no way of currently testing this")
def test_wps_spatial_analog():
    client = client_for(Service(processes=[SpatialAnalogProcess()]))
    datainputs = "[candidate={0};target={" \
                 "0};location={1},{2};indices={3};indices={4}]"\
        .format(TESTDATA['reference_indicators'], -72, 46, 'meantemp',
                'totalpr')

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='spatial_analog',
        datainputs=datainputs)
    assert_response_success(resp)
