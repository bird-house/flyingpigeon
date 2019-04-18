import pytest

from pywps import Service
from pywps.tests import client_for, assert_response_success

from .common import get_output, CFG_FILE, TESTDATA
from flyingpigeon.processes import SubsetBboxProcess
import netCDF4 as nc


def test_wps_subset_bbox():
    client = client_for(Service(processes=[SubsetBboxProcess()], cfgfiles=CFG_FILE))

    datainputs = "resource=files@xlink:href={fn};" \
                 "lat0={lat0};" \
                 "lon0={lon0};" \
                 "lat1={lat1};" \
                 "lon1={lon1};" \
                 .format(fn=TESTDATA['cmip5_tasmax_2006_nc'], lat0=2., lon0=3., lat1=4., lon1=5.)

    resp = client.get(
        "?service=WPS&request=Execute&version=1.0.0&identifier=subset-bbox&datainputs={}".format(
            datainputs))

    assert_response_success(resp)

    out = get_output(resp.xml)
    ds = nc.Dataset(out['output'][7:])
    check_bnds(ds['lat_bnds'], 2, 4)
    check_bnds(ds['lon_bnds'], 3, 5)

    assert 'metalink' in out


def check_bnds(bnds, low, high):
    """Assert that bounds are at least partially within low and high."""
    for b in bnds:
        assert b[1] > low
        assert b[0] < high
