import os
from pywps.tests import WpsClient, WpsTestResponse
from pywps.app.basic import xpath_ns

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
    'cmip3_tas_sresb1_da_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip3',
        'tas.sresb1.giss_model_e_r.run1.atm.da.nc')),
    'cmip3_tas_sresa2_da_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip3',
        'tas.sresa2.miub_echo_g.run1.atm.da.nc')),
    'cmip3_tasmin_sresa2_da_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip3',
        'tasmin.sresa2.miub_echo_g.run1.atm.da.nc')),
    'cmip3_tasmax_sresa2_da_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip3',
        'tasmax.sresa2.miub_echo_g.run1.atm.da.nc')),
    'cmip3_pr_sresa2_da_nc': "file://{0}".format(os.path.join(
        TESTS_HOME,
        'testdata',
        'cmip3',
        'pr.sresa2.miub_echo_g.run1.atm.da.nc')),
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


def get_output(xml):
    """Return a dictionary of output values from the WPS response xml."""
    output = {}
    for output_el in xpath_ns(xml, '/wps:ExecuteResponse'
                                   '/wps:ProcessOutputs/wps:Output'):
        [identifier_el] = xpath_ns(output_el, './ows:Identifier')
        [value_el] = xpath_ns(output_el, './wps:Reference')
        if value_el is not None:
            output[identifier_el.text] = value_el.attrib["{http://www.w3.org/1999/xlink}href"]
        else:
            [value_el] = xpath_ns(output_el, './wps:Data/wps:LiteralData')
            output[identifier_el.text] = value_el.text

    return output
