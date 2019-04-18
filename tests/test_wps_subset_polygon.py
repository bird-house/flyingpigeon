import pytest

from pywps import Service
from pywps.tests import client_for, assert_response_success

from .common import get_output, CFG_FILE
from flyingpigeon.processes import SubsetPolygonProcess
import xarray as xr
import numpy as np


def test_wps_xclim_indices(tas_data_set):
    client = client_for(Service(processes=[SubsetPolygonProcess()], cfgfiles=CFG_FILE))

    datainputs = "resource=files@xlink:href=file://{fn};" \
                 "lat0={lat0};" \
                 "lon0={lon0};" \
                 "lat1={lat1};" \
                 "lon1={lon1};" \
                 "y0={y0};" \
                 "y1={y1};".format(fn=tas_data_set, lat0=2., lon0=3., lat1=4, lon1=5, y0=2000, y1=2003)

    resp = client.get(
        "?service=WPS&request=Execute&version=1.0.0&identifier=subset_bbox&datainputs={}".format(
            datainputs))

    assert_response_success(resp)
    out = get_output(resp.xml)
    ds = xr.open_dataset(out['output'][6:])
    np.testing.assert_array_equal(ds.lat, [2, 3, 4])
    np.testing.assert_array_equal(ds.lon, [3, 4])
