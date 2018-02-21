import pytest
import os

from pywps import Service
from pywps.tests import assert_response_success

from flyingpigeon.processes import ESMFRegridProcess
from flyingpigeon.tests.common import TESTDATA, client_for, CFG_FILE
from flyingpigeon.config import test_output_path

datainputs_fmt = (
        "resource=files@xlink:href={0};"
        "dest=files@xlink:href={1};"
        "method={2};"
        "snippet={3};"
    )

def test_wps_esmfregrid_process():
    client = client_for(
        Service(processes=[ESMFRegridProcess()], cfgfiles=CFG_FILE))

    datainputs = datainputs_fmt.format(
        TESTDATA['cmip5_tasmax_2006_nc'],
        TESTDATA['cordex_tasmax_2006_nc'],
        "bilinear",
        "True",)

    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='esmf_regrid',
        datainputs=datainputs)

    print(resp.response[0])
    print(datainputs)
    assert_response_success(resp)
