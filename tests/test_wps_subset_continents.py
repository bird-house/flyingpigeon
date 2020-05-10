from pywps import Service
from pywps.tests import assert_response_success

from flyingpigeon.processes import SubsetcontinentProcess
from .common import TESTDATA, client_for, get_output, CFG_FILE


datainputs_fmt = (
    "resource=files@xlink:href={0};"
    "region={1};"
)


def test_wps_subset_continents():
    client = client_for(Service(processes=[SubsetcontinentProcess()], cfgfiles=CFG_FILE))

    datainputs = datainputs_fmt.format(
        TESTDATA['cmip5_tasmax_2006_nc'],
        'Africa',
        )

    # resp = client.get(
    #     service='wps', request='execute', version='1.0.0',
    #     identifier='subset_continents',
    #     datainputs=datainputs)

    resp = client.get(
       "?service=WPS&request=Execute&version=1.0.0&identifier=subset_continents&datainputs={}".format(datainputs))

    assert_response_success(resp)

    # Check output file size is smaller than input.
    out = get_output(resp.xml)
    assert 'output' in out.keys()
    assert 'metalink' in out
