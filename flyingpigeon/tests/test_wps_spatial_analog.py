from flyingpigeon.tests.common import WpsTestClient, TESTDATA, \
    assert_response_success
import ocgis
import numpy as np
from flyingpigeon.processes import wps_spatial_analog as sa

import pytest
from ocgis.calc.base import AbstractMultivariateFunction, \
    AbstractParameterizedFunction, AbstractFieldFunction
from ocgis.test.base import TestBase, AbstractTestField
from ocgis.collection.field import OcgField
from ocgis import RequestDataset, OcgOperations, FunctionRegistry
from ocgis.variable.base import Variable
from ocgis.variable.temporal import TemporalVariable
import datetime as dt
from ocgis.test.base import TestBase



class TestDissimilarity(TestBase):
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
        from ocgis.spatial.grid import GridXY
        from ocgis.variable.temporal import TemporalVariable
        np.random.seed(1)

        row = Variable(value=range(4, 4+nrow), name='row', dimensions='row')
        col = Variable(value=range(40, 40+ncol), name='col', dimensions='col')


        # grid = SpatialGridDimension(row=row, col=col)
        # sdim = SpatialDimension(grid=grid, crs=crs)
        grid = GridXY(col, row)

        value_temporal = []
        start = dt.datetime(2000, 1, 1)
        delta = dt.timedelta(days=1)
        ctr = 0
        while ctr < ntime:
            value_temporal.append(start)
            start += delta
            ctr += 1

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
        field = OcgField(grid=grid, time=temporal, is_data=variable, level=level,
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
        #rmean = OcgOperations(aggregate=True, geom=(40,4),dataset=reference).execute().get_element()

        cand_range = [dt.datetime(2000, 8, 1), dt.datetime(2000, 8, 31)]
        can = [ocgis.RequestDataset(p, time_range=cand_range) for p in [p3,
                                                                       p4]]
        candidate = ocgis.MultiRequestDataset(can)

        calc = [{'func': 'dissimilarity',
                 'name': 'output_mfpf',
                 'kwds': {'reference': reference,
                          'candidate': ('v1', 'v2')}}]
        ops = OcgOperations(dataset=candidate, calc=calc)
        ret = ops.execute()
        actual_field = ret.get_element()
        actual_variables = get_variable_names(actual_field.data_variables)
        self.assertEqual(actual_variables[0],
                         ('dissimilarity_seuclidean'))

def test_dissimilarity_op():
    import json, os
    import datetime as dt



    ocgis.env.DIR_DATA = '/home/david/projects/PAVICS/birdhouse/flyingpigeon/flyingpigeon/tests/testdata/spatial_analog/'
    rfn = 'reference_indicators.nc'  # TESTDATA['reference_indicators']
    tfn = 'target_indicators.nc'
    tfjson = 'target_indicators.json'

    indices = ['meantemp', 'totalpr']
    # Candidate fields
    crd = ocgis.RequestDataset(rfn,
                variable=indices,
                time_range=[dt.datetime(1960, 1, 1), dt.datetime(2000, 1, 1)],
                )

    # Reference fields
    rrd = ocgis.RequestDataset(tfn,
                variable=indices,
                time_range=[dt.datetime(1970, 1, 1), dt.datetime(2000, 1, 1)],
                )
    reference = rrd.get()
    #tarr = json.load(open(os.path.join(ocgis.env.DIR_DATA, tfjson)))
    #reference = np.array([tarr[ind] for ind in indices]).T

    ops = ocgis.OcgOperations(
        calc=[{'func': 'dissimilarity', 'name': 'spatial_analog',
               'kwds': {'algo': 'seuclidean', 'reference': reference,
                        'candidate': indices}}],
        geom=(-72, 46),
        time_range=[dt.datetime(1960, 1, 1), dt.datetime(2000, 1, 1)],
        dataset=crd)

    res = ops.execute()


@pytest.mark.skip()
def test_wps_spatial_analog():

    wps = WpsTestClient()

    datainputs = "[reference_nc={0};target_json={1};indices=meantemp;indices=totalpr]".format(TESTDATA['reference_indicators'], TESTDATA['target_indicators'])

    resp = wps.get(service='wps', request='execute', version='1.0.0', identifier='spatial_analog',
                   datainputs=datainputs)
    assert_response_success(resp)