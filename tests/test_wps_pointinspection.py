from pywps import Service
from pywps.tests import assert_response_success

from flyingpigeon.processes import PointinspectionProcess
from .common import TESTDATA, client_for  # ,CFG_FILE
import os


datainputs_fmt = (
    "resource=files@xlink:href={0};"
    "coords={1};"
)


def test_wps_pointinspection():
    client = client_for(
        Service(processes=[PointinspectionProcess()]))  # ,cfgfiles=CFG_FILE
    datainputs = datainputs_fmt.format(
        TESTDATA['cmip5_tasmax_2006_nc'],
        "2.356138, 48.846450",)
    resp = client.get(
        service='wps', request='execute', version='1.0.0',
        identifier='pointinspection',
        datainputs=datainputs)
    assert_response_success(resp)
