from pywps import Service
from pywps.tests import assert_response_success

from flyingpigeon.processes import ESMFRegridProcess
from flyingpigeon.tests.common import TESTDATA, client_for, CFG_FILE

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

    #print(resp.response[0])
    #print(datainputs)
    assert_response_success(resp)


#http://localhost:8093/wps?service=WPS&version=1.0.0&request=execute&identifier=esmf_regrid&datainputs=resource=files@xlink:href=file:///home/david/src/flyingpigeon/flyingpigeon/tests/testdata/cmip5/tasmax_Amon_MPI-ESM-MR_rcp45_r1i1p1_200601-200612.nc;dest=files@xlink:href=file:///home/david/src/flyingpigeon/flyingpigeon/tests/testdata/cordex/tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp45_r1i1p1_MPI-CSC-REMO2009_v1_mon_200602-200612.nc;method=bilinear;snippet=True;
