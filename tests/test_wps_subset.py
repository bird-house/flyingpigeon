import os
import sys
import unittest
import configparser
import json

from pywps import Service
from pywps.tests import client_for
import numpy.ma as ma
import netCDF4

try:
    from tests import test_wps_utils
except ImportError:
    import test_wps_utils


class TestSubset(unittest.TestCase):

    def setUp(self):
        self.config = configparser.rawconfigparser()
        if os.path.isfile('configtests.cfg'):
            self.config.read('configtests.cfg')
        else:
            self.config.read('flyingpigeon/tests/configtests.cfg')
        sys.path.append('/'.join(os.getcwd().split('/')[:-1]))
        from flyingpigeon.processes import SubsetProcess
        self.client = client_for(Service(processes=[SubsetProcess()]))

    def test_getcapabilities(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)
        wps_tests_utils.get_capabilities(config_dict['wps_host'], self.client)

    def test_getcapabilities_repeat(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        for i in range(10):
            wps_tests_utils.get_capabilities(config_dict['wps_host'],
                                             self.client)

    def test_process_exists(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        wps = wps_tests_utils.get_capabilities(config_dict['wps_host'],
                                               self.client)
        self.assertIn('subset', [x.identifier for x in wps.processes])

    def test_describeprocess(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        process = wps_tests_utils.describe_process(
            'subset', config_dict['wps_host'], self.client)
        self.assertIn('resource',
                      [x.identifier for x in process.dataInputs])
        self.assertIn('initial_datetime',
                      [x.identifier for x in process.dataInputs])
        self.assertIn('output',
                      [x.identifier for x in process.processOutputs])

    def test_subset_wfs_opendap_01(self):
        # pairing0.1.1-montreal_circles0.1.1
        config_dict = wps_tests_utils.config_is_available(
            'pairing0.1.1-montreal_circles0.1.1',
            ['opendap_path', 'fileserver_path', 'geoserver'],
            self.config, set_wps_host=True)

        # let's start with one example...
        resource = os.path.join(
            config_dict['opendap_path'],
            'pairing_day_global-reg-grid_360_720_nobounds_ref180.nc')

        execution = wps_tests_utils.execute(
            'subset', inputs=[
                ('resource', resource),
                ('typename', 'testgeom:montreal_circles'),
                ('featureids', 'montreal_circles.43'),
                ('geoserver', config_dict['geoserver'])],
            wps_host=config_dict['wps_host'], wps_client=self.client)

        #if outputs['status'] == 'ProcessFailed':
        #    raise RuntimeError(wps_request)

        while execution.status == 'ProcessAccepted':
            execution.checkStatus(sleepSecs=1)
        output_json = execution.processOutputs[0].reference
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = json.loads(wps_tests_utils.get_wps_xlink(output_json))
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            tmp_output_netcdf = output_netcdf[7:]
        else:
            tmp_output_netcdf = '/tmp/testtmp.nc'
            f1 = open(tmp_output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()

        nc = netCDF4.Dataset(tmp_output_netcdf, 'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['pairing']
        self.assertEqual(nclon.shape, (21,))
        self.assertEqual(nclat.shape, (21,))
        self.assertEqual(ncvar.shape, (365, 21, 21))
        self.assertTrue(ncvar[0,0,0] is ma.masked)
        self.assertEqual(ncvar[0,10,10], 271213.0)
        self.assertEqual(nc.subset_typename, 'testgeom:montreal_circles')
        self.assertEqual(nc.subset_featureid, 'montreal_circles.43')
        nc.close()

    def test_subset_wfs_opendap_01_default_geoserver(self):
        # pairing0.1.1-montreal_circles0.1.1
        config_dict = wps_tests_utils.config_is_available(
            'pairing0.1.1-montreal_circles0.1.1',
            ['opendap_path', 'fileserver_path', 'test_default_geoserver'],
            self.config, set_wps_host=True)

        # let's start with one example...
        resource = os.path.join(
            config_dict['opendap_path'],
            'pairing_day_global-reg-grid_360_720_nobounds_ref180.nc')

        execution = wps_tests_utils.execute(
            'subset', inputs=[
                ('resource', resource),
                ('typename', 'testgeom:montreal_circles'),
                ('featureids', 'montreal_circles.43')],
            wps_host=config_dict['wps_host'], wps_client=self.client)

        #if outputs['status'] == 'ProcessFailed':
        #    raise RuntimeError(wps_request)

        while execution.status == 'ProcessAccepted':
            execution.checkStatus(sleepSecs=1)
        output_json = execution.processOutputs[0].reference
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = json.loads(wps_tests_utils.get_wps_xlink(output_json))
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            tmp_output_netcdf = output_netcdf[7:]
        else:
            tmp_output_netcdf = '/tmp/testtmp.nc'
            f1 = open(tmp_output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()

        nc = netCDF4.Dataset(tmp_output_netcdf, 'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['pairing']
        self.assertEqual(nclon.shape, (21,))
        self.assertEqual(nclat.shape, (21,))
        self.assertEqual(ncvar.shape, (365, 21, 21))
        self.assertTrue(ncvar[0,0,0] is ma.masked)
        self.assertEqual(ncvar[0,10,10], 271213.0)
        self.assertEqual(nc.subset_typename, 'testgeom:montreal_circles')
        self.assertEqual(nc.subset_featureid, 'montreal_circles.43')
        nc.close()

    def test_subset_wfs_fileserver_01(self):
        # pairing0.1.1-montreal_circles0.1.1
        config_dict = wps_tests_utils.config_is_available(
            'pairing0.1.1-montreal_circles0.1.1',
            ['opendap_path', 'fileserver_path', 'geoserver'],
            self.config, set_wps_host=True)

        # let's start with one example...
        resource = os.path.join(
            config_dict['fileserver_path'],
            'pairing_day_global-reg-grid_360_720_nobounds_ref180.nc')

        execution = wps_tests_utils.execute(
            'subset', inputs=[
                ('resource', resource),
                ('typename', 'testgeom:montreal_circles'),
                ('featureids', 'montreal_circles.43'),
                ('geoserver', config_dict['geoserver'])],
            wps_host=config_dict['wps_host'], wps_client=self.client)

        #if outputs['status'] == 'ProcessFailed':
        #    raise RuntimeError(wps_request)

        while execution.status == 'ProcessAccepted':
            execution.checkStatus(sleepSecs=1)
        output_json = execution.processOutputs[0].reference
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = json.loads(wps_tests_utils.get_wps_xlink(output_json))
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            tmp_output_netcdf = output_netcdf[7:]
        else:
            tmp_output_netcdf = '/tmp/testtmp.nc'
            f1 = open(tmp_output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()

        nc = netCDF4.Dataset(tmp_output_netcdf, 'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['pairing']
        self.assertEqual(nclon.shape, (21,))
        self.assertEqual(nclat.shape, (21,))
        self.assertEqual(ncvar.shape, (365, 21, 21))
        self.assertTrue(ncvar[0,0,0] is ma.masked)
        self.assertEqual(ncvar[0,10,10], 271213.0)
        self.assertEqual(nc.subset_typename, 'testgeom:montreal_circles')
        self.assertEqual(nc.subset_featureid, 'montreal_circles.43')
        nc.close()

    def test_subset_wfs_opendap_multi_inputs_01(self):
        # pairing0.1.1-montreal_circles0.1.1
        config_dict = wps_tests_utils.config_is_available(
            'pairing0.1.1-montreal_circles0.1.1',
            ['opendap_path', 'fileserver_path', 'geoserver'],
            self.config, set_wps_host=True)

        # let's start with one example...
        resource1 = os.path.join(
            config_dict['opendap_path'],
            'pairing_day_global-reg-grid_360_720_nobounds_ref180.nc')
        resource2 = os.path.join(
            config_dict['opendap_path'],
            'pairing_day_global-reg-grid_360_720_bounds_ref180.nc')

        execution = wps_tests_utils.execute(
            'subset', inputs=[
                ('resource', resource1),
                ('resource', resource2),
                ('typename', 'testgeom:montreal_circles'),
                ('featureids', 'montreal_circles.43'),
                ('featureids', 'montreal_circles.45'),
                ('geoserver', config_dict['geoserver'])],
            wps_host=config_dict['wps_host'], wps_client=self.client)

        #if outputs['status'] == 'ProcessFailed':
        #    raise RuntimeError(wps_request)

        while execution.status == 'ProcessAccepted':
            execution.checkStatus(sleepSecs=1)
        output_json = execution.processOutputs[0].reference
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = json.loads(wps_tests_utils.get_wps_xlink(output_json))
        self.assertEqual(len(json_data), 4)
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            tmp_output_netcdf = output_netcdf[7:]
        else:
            tmp_output_netcdf = '/tmp/testtmp.nc'
            f1 = open(tmp_output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()

        nc = netCDF4.Dataset(tmp_output_netcdf, 'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['pairing']
        self.assertEqual(nclon.shape, (21,))
        self.assertEqual(nclat.shape, (21,))
        self.assertEqual(ncvar.shape, (365, 21, 21))
        self.assertTrue(ncvar[0,0,0] is ma.masked)
        self.assertEqual(ncvar[0,10,10], 271213.0)
        self.assertEqual(nc.subset_typename, 'testgeom:montreal_circles')
        self.assertEqual(nc.subset_featureid, 'montreal_circles.43')
        nc.close()


suite = unittest.TestLoader().loadTestsFromTestCase(TestSubset)

if __name__ == '__main__':
    run_result = unittest.TextTestRunner(verbosity=2).run(suite)
    sys.exit(not run_result.wasSuccessful())
