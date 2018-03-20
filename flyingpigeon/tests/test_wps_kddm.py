import pytest

from pywps import Service
from pywps.tests import assert_response_success

from common import client_for
from flyingpigeon.processes.wps_kddm_bc import KDDM_BC_Process

import os

CLIMOD_DIR  = 'file:///home/david/src/climod/tests/raw/'
TESTS_HOME = os.path.abspath(os.path.dirname(__file__))
CFG_FILE = os.path.join(TESTS_HOME, 'test.cfg')


def get_output(doc):
    from pywps.app.basic import xpath_ns
    output = {}
    for output_el in xpath_ns(doc, '/wps:ExecuteResponse'
                                   '/wps:ProcessOutputs/wps:Output'):
        [identifier_el] = xpath_ns(output_el, './ows:Identifier')
        try:
            [value_el] = xpath_ns(output_el, './wps:Data/wps:LiteralData')
        except:
            [value] = xpath_ns(output_el, './wps:Reference')
            value_el = value.get('{http://www.w3.org/1999/xlink}href')
        output[identifier_el.text] = value_el
    return output

#@pytest.mark.skip
def test_wps_kddm():
    obs = os.path.join(CLIMOD_DIR, 'tmax.obs.nc')
    cur = os.path.join(CLIMOD_DIR, 'tmax.cur.nc')
    fut = os.path.join(CLIMOD_DIR, 'tmax.fut.nc')

    client = client_for(Service(processes=[KDDM_BC_Process()], cfgfiles=CFG_FILE))
    datainputs = (
        "obs=files@xlink:href={0};"
        "ref=files@xlink:href={1};"
        "fut=files@xlink:href={2}"
    ).format(obs,cur, fut)

    resp = client.get(
        service='WPS', request='Execute', version='1.0.0', datainputs=datainputs,
        identifier='kddm_bc')
    #print(resp.response[0])
    assert_response_success(resp)

    out = get_output(resp.xml)
    compare_bc_output(obs[6:], cur[6:], fut[6:], out['output_netcdf_ref'][6:], out['output_netcdf_fut'][6:])

    return resp



def compare_bc_output(obs, cur, fut, out_cur, out_fut):
    import numpy as np

    mc_obs = monthly_clim(obs)
    mc_cur = monthly_clim(cur)
    mc_fut = monthly_clim(fut)
    mc_out_cur = monthly_clim(out_cur)
    mc_out_fut = monthly_clim(out_fut)

    bc = mc_obs - mc_cur
    di = np.abs(bc)
    df = np.abs(mc_obs - mc_out_cur)

    bc_cur = mc_out_cur - mc_cur
    bc_fut = mc_out_fut - mc_fut
    np.testing.assert_almost_equal(bc, bc_cur, 1)
    np.testing.assert_almost_equal(bc, bc_fut, 1)
    np.testing.assert_almost_equal(bc_cur, bc_fut, 1)

    np.testing.assert_equal(df < di) | (di < di.mean()/50, True)


def monthly_clim(ncfile):
    import ocgis
    rd = ocgis.RequestDataset(ncfile)
    ops = ocgis.OcgOperations(rd, calc=[{'func':'mean', 'name':'monthly_mean'}], calc_grouping=['month'])
    sc = ops.execute()
    return sc.get_element()['monthly_mean'].get_value()

#inpath = '/home/david/src/climod/tests/raw'
#outpath = '/tmp/5b5744d2-0c31-11e8-81f5-b052162515fb/'
def offline(inpath, outpath, var):

    obs = os.path.join(inpath, '{}.obs.nc'.format(var))
    cur = os.path.join(inpath, '{}.cur.nc'.format(var))
    fut = os.path.join(inpath, '{}.fut.nc'.format(var))

    out_cur = os.path.join(outpath, '{}.cur_kddm-bc.nc'.format(var))
    out_fut = os.path.join(outpath, '{}.fut_kddm-bc.nc'.format(var))

    test_bc_output(obs, cur, fut, out_cur, out_fut)
