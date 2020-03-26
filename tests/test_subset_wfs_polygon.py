from pywps import Service
from pywps.tests import client_for, assert_response_success

from .common import get_output
from flyingpigeon.processes.wps_subset_wfs_polygon import SubsetWFSPolygonProcess


# def test_wps_subset():
#     client = client_for(Service(processes=[SubsetWFSPolygonProcess()]))
#
#     datainputs = "resource=@xlink:href={nc};" \
#                  "typename={typename};" \
#                  "geoserver={geoserver};" \
#                  "featureids={fid1};" \
#                  "featureids={fid2};" \
#                  "mosaic={mosaic}"
#
#     inputs = dict(nc="https://pavics.ouranos.ca/twitcher/ows/proxy/thredds/dodsC/birdhouse/nrcan"
#                      "/nrcan_northamerica_monthly"
#                      "/tasmax/nrcan_northamerica_monthly_2015_tasmax.nc",
#                   typename="public:USGS_HydroBASINS_lake_na_lev12",
#                   geoserver="https://pavics.ouranos.ca/geoserver/wfs",
#                   fid1="USGS_HydroBASINS_lake_na_lev12.67061",
#                   fid2="USGS_HydroBASINS_lake_na_lev12.67088",
#                   mosaic=False)
#
#     resp = client.get(
#         "?service=WPS&request=Execute&version=1.0.0&identifier=subset-wfs-polygon&datainputs={}".format(
#             datainputs.format(**inputs)))
#     assert_response_success(resp)
#
#     out = get_output(resp.xml)
#     assert 'metalink' in out
#
#     inputs['mosaic'] = True
#     resp = client.get(
#         "?service=WPS&request=Execute&version=1.0.0&identifier=subset-wfs-polygon&datainputs={}".format(
#             datainputs.format(**inputs)))
#     assert_response_success(resp)
