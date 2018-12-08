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


class TestSubsetWFS(unittest.TestCase):

    def setUp(self):
        self.config = ConfigParser.RawConfigParser()
        if os.path.isfile('configtests.cfg'):
            self.config.read('configtests.cfg')
        else:
            self.config.read('flyingpigeon/tests/configtests.cfg')
        sys.path.append('/'.join(os.getcwd().split('/')[:-1]))
        from flyingpigeon.processes import SubsetWFSProcess
        self.client = client_for(Service(processes=[SubsetWFSProcess()]))

    def test_getcapabilities(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'],
            '?service=WPS&request=GetCapabilities&version=1.0.0',
            self.client)
        self.assertTrue(html_response)

    def test_getcapabilities_repeat(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        for i in range(10):
            html_response = wps_tests_utils.wps_response(
                config_dict['wps_host'],
                '?service=WPS&request=GetCapabilities&version=1.0.0',
                self.client)
            self.assertTrue(html_response)

    def test_process_exists(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'],
            '?service=WPS&request=GetCapabilities&version=1.0.0',
            self.client)
        processes = wps_tests_utils.parse_getcapabilities(html_response)
        self.assertTrue('subset_WFS' in processes)

    def test_describeprocess(self):
        config_dict = wps_tests_utils.config_is_available(
            'subsetwfs', [], self.config, set_wps_host=True)

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'],
            ('?service=WPS&request=DescribeProcess&version=1.0.0&'
             'identifier=subset_WFS'),
            self.client)
        describe_process = wps_tests_utils.parse_describeprocess(html_response)
        self.assertTrue('mosaic' in describe_process[0]['inputs'])
        self.assertTrue('output' in describe_process[0]['outputs'])

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

        wps_request = (
            '?service=WPS&request=execute&version=1.0.0&'
            'identifier=subset_WFS&DataInputs=resource={0};'
            'typename={1};featureids={2};geoserver={3}').format(
                resource,
                'testgeom:montreal_circles',
                'montreal_circles.43',
                config_dict['geoserver'])

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'], wps_request, self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        if outputs['status'] == 'ProcessFailed':
            raise RuntimeError(wps_request)
        output_json = outputs['outputs']['output']
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
            ['opendap_path', 'fileserver_path', 'geoserver',
             'test_default_geoserver'],
            self.config, set_wps_host=True)

        # let's start with one example...
        resource = os.path.join(
            config_dict['opendap_path'],
            'pairing_day_global-reg-grid_360_720_nobounds_ref180.nc')

        wps_request = (
            '?service=WPS&request=execute&version=1.0.0&'
            'identifier=subset_WFS&DataInputs=resource={0};'
            'typename={1};featureids={2};geoserver={3}').format(
                resource,
                'testgeom:montreal_circles',
                'montreal_circles.43',
                config_dict['geoserver'])

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'], wps_request, self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        if outputs['status'] == 'ProcessFailed':
            raise RuntimeError(wps_request)
        output_json = outputs['outputs']['output']
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

        wps_request = (
            '?service=WPS&request=execute&version=1.0.0&'
            'identifier=subset_WFS&DataInputs=resource={0};'
            'typename={1};featureids={2};geoserver={3}').format(
                resource,
                'testgeom:montreal_circles',
                'montreal_circles.43',
                config_dict['geoserver'])

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'], wps_request, self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        if outputs['status'] == 'ProcessFailed':
            raise RuntimeError(wps_request)
        output_json = outputs['outputs']['output']
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

        wps_request = (
            '?service=WPS&request=execute&version=1.0.0&'
            'identifier=subset_WFS&DataInputs=resource={0};resource={1};'
            'typename={2};featureids={3};featureids={4};geoserver={5}').format(
                resource1, resource2,
                'testgeom:montreal_circles',
                'montreal_circles.43', 'montreal_circles.45',
                config_dict['geoserver'])

        html_response = wps_tests_utils.wps_response(
            config_dict['wps_host'], wps_request, self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        if outputs['status'] == 'ProcessFailed':
            raise RuntimeError(wps_request)
        output_json = outputs['outputs']['output']
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

suite = unittest.TestLoader().loadTestsFromTestCase(TestSubsetWFS)

if __name__ == '__main__':
    run_result = unittest.TextTestRunner(verbosity=2).run(suite)
    sys.exit(not run_result.wasSuccessful())
