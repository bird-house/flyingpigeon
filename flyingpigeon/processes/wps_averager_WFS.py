import os
import shutil
import time
import json
import tempfile
import traceback

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexOutput
from pywps import get_format, configuration
import owslib
from owslib.wfs import WebFeatureService
import ocgis
import netCDF4
from shapely.geometry import shape

from flyingpigeon.utils import guess_main_variable, opendap_or_download

json_format = get_format('JSON')


class AveragerWFS(Process):
    def __init__(self):
        inputs = [
            LiteralInput('resource',
                         'NetCDF resource',
                         abstract='NetCDF files, can be OPEnDAP urls.',
                         data_type='string',
                         max_occurs=1000),
            LiteralInput('typename',
                         'TypeName',
                         abstract='Name of the layer in GeoServer.',
                         data_type='string'),
            LiteralInput('featureids',
                         'Feature Ids',
                         abstract='fid(s) of the feature in the layer.',
                         data_type='string',
                         max_occurs=1000),
            LiteralInput('geoserver',
                         'Geoserver',
                         abstract=('Typically of the form '
                                   'http://host:port/geoserver/wfs'),
                         data_type='string',
                         min_occurs=0),
            LiteralInput('union',
                         'Union of Feature Ids',
                         abstract=('If True, selected regions will be '
                                   'merged into a single geometry.'),
                         data_type='boolean',
                         min_occurs=0,
                         default=False),
            LiteralInput('variable',
                         'Variable',
                         abstract=('Name of the variable in the NetCDF file.'
                                   'Will be guessed if not provided.'),
                         data_type='string',
                         min_occurs=0)]

        outputs = [
            ComplexOutput('output',
                          'JSON file with link to NetCDF outputs',
                          abstract='JSON file with link to NetCDF outputs.',
                          as_reference=True,
                          supported_formats=[json_format])]

        super(AveragerWFS, self).__init__(
            self._handler,
            identifier='averager_WFS',
            title='Averager WFS',
            version='0.1',
            abstract=('Return the data with weighted average of grid cells '
                      'intersecting the selected polygon for each input '
                      'dataset.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        outputpath = configuration.get_config_value('server', 'outputpath')
        outputurl = configuration.get_config_value('server', 'outputurl')

        list_of_files = []
        for one_resource in request.inputs['resource']:
            # Download if not opendap
            try:
                nc_file = opendap_or_download(one_resource.data, '/tmp')
            except:
                raise Exception(traceback.format_exc())
            list_of_files.append(nc_file)

        if 'union' in request.inputs:
            union = request.inputs['union'][0].data
        else:
            union = False

        features = [f.data for f in request.inputs['featureids']]
        typename = request.inputs['typename'][0].data
        if 'geoserver' in request.inputs:
            geoserver = request.inputs['geoserver'][0].data
        else:
            geoserver = configuration._get_default_config_files_location()
            #geoserver = configuration.get_config_value(
            #    'extra', 'geoserver')

        try:
            conn = WebFeatureService(url=geoserver, version='2.0.0')
            resp = conn.getfeature([typename], featureid=features,
                                   outputFormat='application/json')
            feature = json.loads(resp.read())
            crs_code = owslib.crs.Crs(
                feature['crs']['properties']['name']).code
            crs = ocgis.CoordinateReferenceSystem(epsg=crs_code)
            geom = [
                {'geom': shape(f['geometry']), 'crs': crs,
                 'properties': f['properties']}
                for f in feature['features']]
        except Exception as e:
            msg = ('Failed to fetch features.\ngeoserver: {0} \n'
                   'typename: {1}\nfeatures {2}\n{3}').format(
                geoserver, typename, features, e)
            raise Exception(msg)

        if union:
            new_geom = geom[0]
            for merge_geom in geom[1:]:
                new_geom['geom'] = new_geom['geom'].union(merge_geom['geom'])
            new_geom['properties'] = {'bbox': feature['bbox']}
            geom = new_geom

        try:
            output_files = []
            output_urls = []
            for one_file in list_of_files:
                file_name = os.path.basename(one_file)
                if file_name[-3:] == '.nc':
                    file_prefix = file_name[:-3]
                else:
                    file_prefix = file_name
                ocgis.env.DIR_OUTPUT = os.getcwd()
                nc = netCDF4.Dataset(one_file, 'r')
                var_name = guess_main_variable(nc)
                nc.close()
                rd = ocgis.RequestDataset(one_file, var_name)
                for i, one_geom in enumerate(geom):
                    # Here with aggregate=True and output_format=region-nc,
                    # can't pass the whole one_geom dictionary, is this
                    # a sign that this does not support multipolygon?
                    ops = ocgis.OcgOperations(dataset=rd,
                                              geom=one_geom['geom'],
                                              spatial_operation='clip',
                                              aggregate=True,
                                              snippet=False,
                                              output_format='region-nc',
                                              interpolate_spatial_bounds=True,
                                              prefix=file_prefix).execute()
                    mv_dir = tempfile.mkdtemp(dir=outputpath)
                    mv_name = '{0}_{1}.nc'.format(
                        os.path.basename(ops)[:-3], features[i])
                    mv_file = os.path.join(mv_dir, mv_name)
                    shutil.move(ops, mv_file)
                    output_files.append(mv_file)
                if outputurl == 'file:///tmp':
                    disk_file = 'file:///' + mv_file.lstrip('/')
                    output_urls.append(disk_file)
                else:
                    url_file = os.path.join(
                        outputurl, os.path.basename(mv_dir),
                        os.path.basename(mv_file))
                    output_urls.append(url_file)
        except:
            raise Exception(traceback.format_exc())

        # Here we construct a unique filename
        time_str = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        output_file_name = "result_%s_.json" % (time_str,)
        output_file = os.path.join(outputpath, output_file_name)
        f1 = open(output_file, 'w')
        f1.write(json.dumps(output_urls))
        f1.close()
        response.outputs['output'].file = output_file
        response.outputs['output'].output_format = json_format
        response.update_status("done", 100)
        return response
