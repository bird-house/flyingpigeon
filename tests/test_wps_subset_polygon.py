from pywps import Service
from pywps.tests import client_for, assert_response_success

from .common import get_output, TESTDATA, CFG_FILE
from flyingpigeon.processes import SubsetWFSPolygonProcess
import datetime as dt
import netCDF4 as nc

def test_wps_xclim_indices():
    client = client_for(Service(processes=[SubsetWFSPolygonProcess()], cfgfiles=CFG_FILE))

    datainputs = "resource=files@xlink:href=file://{fn};" \
                 "typename=public:{tn};" \
                 "featureids={tn}.{fid};" \
                 "geoserver={geoserver};" \
                 "start={start};" \
                 "end={end};" \
        .format(fn=TESTDATA['cmip5_tasmax_2006_nc'],
                tn='global_admin_boundaries',
                fid=1,
                geoserver='https://pavics.ouranos.ca/geoserver/wfs',
                start=dt.datetime(2006, 1, 1),
                end=dt.datetime(2006, 6, 1))


    resp = client.get(
        "?service=WPS&request=Execute&version=1.0.0&identifier=subset-wfs-polygon&datainputs={}".format(
            datainputs))

    assert_response_success(resp)
    out = get_output(resp.xml)
    ds = nc.Dataset(out['output'][6:])

