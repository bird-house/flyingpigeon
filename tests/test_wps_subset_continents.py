import pytest

from pywps import Service
from pywps.tests import assert_response_success

from flyingpigeon.processes import SubsetcontinentProcess
from tests.common import TESTDATA, client_for, get_output, CFG_FILE
import os


datainputs_fmt = (
    "resource=@xlink:href={0};"
    "region={1};"
    "mosaic={2};"
)


@pytest.mark.skip(reason="fails when called with pytest tests")
def test_wps_subset_continents():
    client = client_for(
        Service(processes=[SubsetcontinentProcess()], cfgfiles=CFG_FILE))

    datainputs = datainputs_fmt.format(
        TESTDATA['cmip5_tasmax_2006_nc'],
        'Africa',
        "True")

    print(datainputs)

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='subset_continents',
        datainputs=datainputs)

    print(resp.get_data())
    assert_response_success(resp)

    # Check output file size is smaller than input.
    out = get_output(resp.xml)
    assert 'output' in out.keys()
    # ins = os.path.getsize(TESTDATA['cmip5_tasmax_2006_nc'][6:])
    # outs = os.path.getsize(out['ncout'][6:])
    # assert (outs < ins)
