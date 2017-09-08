import os
from pywps.tests import WpsClient, WpsTestResponse

# SERVICE = "http://localhost:8093/wps"

TESTS_HOME = os.path.abspath(os.path.dirname(__file__))
CFG_FILE = os.path.join(TESTS_HOME, 'test.cfg')

TESTDATA = {
    'cmip5_tasmax_2006_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip5',
        'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc')),
    'cmip5_tasmax_2007_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip5',
        'tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200701-200712.nc')),
    'cordex_tasmax_2006_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cordex',
        'tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200602-200612.nc')),
    'cordex_tasmax_2007_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cordex',
        'tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200701-200712.nc')),
    'indicators_medium.nc': "file://{0}".format(os.path.join(
        TESTS_HOME, 'testdata',
        'spatial_analog',
        'indicators_medium.nc')),
    'indicators_small.nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'spatial_analog',
        'indicators_small.nc')),
    'dissimilarity.nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'spatial_analog',
        'dissimilarity.nc')),
}


class WpsTestClient(WpsClient):

    def get(self, *args, **kwargs):
        query = "?"
        for key, value in kwargs.iteritems():
            query += "{0}={1}&".format(key, value)
        return super(WpsTestClient, self).get(query)


def client_for(service):
    return WpsTestClient(service, WpsTestResponse)
