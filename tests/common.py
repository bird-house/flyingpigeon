from pywps import get_ElementMakerForVersion
from pywps.app.basic import get_xpath_ns
from pywps.tests import WpsClient, WpsTestResponse
import os

VERSION = "1.0.0"
WPS, OWS = get_ElementMakerForVersion(VERSION)
xpath_ns = get_xpath_ns(VERSION)

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
}


class WpsTestClient(WpsClient):

    def get(self, *args, **kwargs):
        query = "?"
        for key, value in kwargs.items():
            query += "{0}={1}&".format(key, value)
        return super(WpsTestClient, self).get(query)


def client_for(service):
    return WpsTestClient(service, WpsTestResponse)


def get_output(doc):
    """Copied from pywps/tests/test_execute.py.
    TODO: make this helper method public in pywps."""
    output = {}
    for output_el in xpath_ns(doc, '/wps:ExecuteResponse'
                                   '/wps:ProcessOutputs/wps:Output'):
        [identifier_el] = xpath_ns(output_el, './ows:Identifier')

        lit_el = xpath_ns(output_el, './wps:Data/wps:LiteralData')
        if lit_el != []:
            output[identifier_el.text] = lit_el[0].text

        ref_el = xpath_ns(output_el, './wps:Reference')
        if ref_el != []:
            output[identifier_el.text] = ref_el[0].attrib['href']

        data_el = xpath_ns(output_el, './wps:Data/wps:ComplexData')
        if data_el != []:
            output[identifier_el.text] = data_el[0].text

    return output
