import os
import sys
import unittest
import ConfigParser
import json

from pywps import Service
from pywps.tests import client_for
import netCDF4

try:
    from flyingpigeon.tests import wps_tests_utils
except ImportError:
    import wps_tests_utils


class TestAveragerWFS(unittest.TestCase):

    def setUp(self):
        self.config = ConfigParser.RawConfigParser()
        if os.path.isfile('configtests.cfg'):
            self.config.read('configtests.cfg')
        else:
            self.config.read('flyingpigeon/tests/configtests.cfg')
        self.config_dict = dict(self.config.items('averagerwfs'))
        sys.path.append('/'.join(os.getcwd().split('/')[:-1]))
        from flyingpigeon.processes import AveragerWFS
        self.client = client_for(Service(processes=[AveragerWFS()]))
        self.wps_host = None

    def test_getcapabilities(self):
        html_response = wps_tests_utils.wps_response(
            self.wps_host,
            '?service=WPS&request=GetCapabilities&version=1.0.0',
            self.client)
        self.assertTrue(html_response)

    def test_getcapabilities_repeat(self):
        for i in range(10):
            html_response = wps_tests_utils.wps_response(
                self.wps_host,
                '?service=WPS&request=GetCapabilities&version=1.0.0',
                self.client)
            self.assertTrue(html_response)

    def test_process_exists(self):
        html_response = wps_tests_utils.wps_response(
            self.wps_host,
            '?service=WPS&request=GetCapabilities&version=1.0.0',
            self.client)
        processes = wps_tests_utils.parse_getcapabilities(html_response)
        self.assertTrue('averager_WFS' in processes)

    def test_describeprocess(self):
        html_response = wps_tests_utils.wps_response(
            self.wps_host,
            ('?service=WPS&request=DescribeProcess&version=1.0.0&'
             'identifier=averager_WFS'),
            self.client)
        describe_process = wps_tests_utils.parse_describeprocess(html_response)
        self.assertTrue('union' in describe_process[0]['inputs'])
        self.assertTrue('output' in describe_process[0]['outputs'])

    def test_averager_wfs_opendap(self):
        wps_tests_utils.config_is_available(
            ['netcdf_file_opendap', 'typename_opendap', 'featureids_opendap'],
            self.config_dict)
        if self.config_dict['geoserver_opendap']:
            html_response = wps_tests_utils.wps_response(
                self.wps_host,
                ('?service=WPS&request=execute&version=1.0.0&'
                 'identifier=averager_WFS&DataInputs=resource={0};'
                 'typename={1};featureids={2};geoserver={3}').format(
                    self.config_dict['netcdf_file_opendap'],
                    self.config_dict['typename_opendap'],
                    self.config_dict['featureids_opendap'],
                    self.config_dict['geoserver_opendap']),
                self.client)
        # the default geoserver does not work yet, also might want to
        # move this to a separate test...
        else:
            html_response = wps_tests_utils.wps_response(
                self.wps_host,
                ('?service=WPS&request=execute&version=1.0.0&'
                 'identifier=averager_WFS&DataInputs=resource={0};'
                 'typename={1};featureids={2}').format(
                    self.config_dict['netcdf_file_opendap'],
                    self.config_dict['typename_opendap'],
                    self.config_dict['featureids_opendap']),
                self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        output_json = outputs['outputs']['output']
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = wps_tests_utils.get_wps_xlink(output_json)
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            output_netcdf = output_netcdf[7:]
        else:
            output_netcdf = '/tmp/testtmp.nc'
            f1 = open(output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()
        nc = netCDF4.Dataset(output_netcdf,'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['dummy']
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['dummy']
        self.assertEqual(nclon.size, 1)
        self.assertEqual(nclat.size, 1)
        self.assertEqual(nclon[0], 19)
        self.assertEqual(nclat[0], 13)
        self.assertEqual(ncvar.shape, (1,))
        self.assertAlmostEqual(ncvar[0], 21.4, delta=0.2)

    def test_averager_wfs_fileserver(self):
        wps_tests_utils.config_is_available(
            ['netcdf_file_fileserver', 'typename_fileserver',
             'featureids_fileserver', 'geoserver_fileserver'],
            self.config_dict)
        html_response = wps_tests_utils.wps_response(
            self.wps_host,
            ('?service=WPS&request=execute&version=1.0.0&'
             'identifier=averager_WFS&DataInputs=resource={0};'
             'typename={1};featureids={2};geoserver={3}').format(
                self.config_dict['netcdf_file_fileserver'],
                self.config_dict['typename_fileserver'],
                self.config_dict['featureids_fileserver'],
                self.config_dict['geoserver_fileserver']),
            self.client)
        outputs = wps_tests_utils.parse_execute_response(html_response)
        output_json = outputs['outputs']['output']
        if output_json[:7] == 'file://':
            output_json = output_json[7:]
            f1 = open(output_json, 'r')
            json_data = json.loads(f1.read())
            f1.close()
        else:
            json_data = wps_tests_utils.get_wps_xlink(output_json)
        output_netcdf = json_data[0]
        if output_netcdf[:7] == 'file://':
            output_netcdf = output_netcdf[7:]
        else:
            output_netcdf = '/tmp/testtmp.nc'
            f1 = open(output_netcdf, 'w')
            f1.write(wps_tests_utils.get_wps_xlink(output_netcdf))
            f1.close()
        nc = netCDF4.Dataset(output_netcdf,'r')
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['dummy']
        nclon = nc.variables['lon']
        nclat = nc.variables['lat']
        ncvar = nc.variables['dummy']
        self.assertEqual(nclon.size, 1)
        self.assertEqual(nclat.size, 1)
        self.assertEqual(nclon[0], 19)
        self.assertEqual(nclat[0], 13)
        self.assertEqual(ncvar.shape, (1,))
        self.assertAlmostEqual(ncvar[0], 21.4, delta=0.2)

suite = unittest.TestLoader().loadTestsFromTestCase(TestAveragerWFS)

if __name__ == '__main__':
    run_result = unittest.TextTestRunner(verbosity=2).run(suite)
    sys.exit(not run_result.wasSuccessful())
