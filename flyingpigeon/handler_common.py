import os
import shutil
import time
import json
import tempfile
import traceback

from pywps import get_format, configuration
import owslib
from owslib.wfs import WebFeatureService
import ocgis
import netCDF4
from shapely.geometry import shape

from flyingpigeon.utils import guess_main_variable, opendap_or_download

json_format = get_format('JSON')


def wfs_common(request, response, mode):
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

    if 'mosaic' in request.inputs:
        mosaic = request.inputs['mosaic'][0].data
    else:
        mosaic = False

    features = [f.data for f in request.inputs['featureids']]
    typename = request.inputs['typename'][0].data
    if 'geoserver' in request.inputs:
        geoserver = request.inputs['geoserver'][0].data
    else:
        geoserver = configuration.get_config_value(
            'extra', 'geoserver')

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

    if mosaic:
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
                if mode == 'averager':
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
                elif mode == 'subsetter':
                    ops = ocgis.OcgOperations(dataset=rd, geom=[one_geom],
                                              snippet=False,
                                              output_format='nc',
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
