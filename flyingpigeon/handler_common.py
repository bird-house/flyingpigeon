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
from ocgis.exc import ExtentError
import netCDF4
from shapely.geometry import shape

from flyingpigeon.utils import guess_main_variable, opendap_or_download

json_format = get_format('JSON')


def wfs_common(request, response, mode):
    """Common part of wps process for wfs operations.

    :param request: request for wps process handler
    :param response: response for wps process handler
    :param mode: 'subsetter' or 'averager'

    :return response: wps process response

    """

    outputpath = configuration.get_config_value('server', 'outputpath')
    outputurl = configuration.get_config_value('server', 'outputurl')

    list_of_files = []
    for one_resource in request.inputs['resource']:
        # Download if not opendap
        # Adding a maximum file size from a server config file would
        # be possible here...
        try:
            nc_file = opendap_or_download(one_resource.data, '/tmp')
        except:
            raise Exception(traceback.format_exc())
        list_of_files.append(nc_file)

    if ('typename' in request.inputs) and ('featureids' in request.inputs):
        typename = request.inputs['typename'][0].data
        features = [f.data for f in request.inputs['featureids']]
        if 'geoserver' in request.inputs:
            geoserver = request.inputs['geoserver'][0].data
        else:
            geoserver = configuration.get_config_value('extra', 'geoserver')
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False
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
    else:
        geom = [None]

    if ('initial_datetime' in request.inputs) and \
       ('final_datetime' in request.inputs):
        tr = [request.inputs['initial_datetime'][0].data,
              request.inputs['final_datetime'][0].data]
    else:
        tr = None

    try:
        output_files = []
        output_urls = []
        mv_dir = tempfile.mkdtemp(dir=outputpath)

        for one_file in list_of_files:
            file_name = os.path.basename(one_file)
            if file_name[-3:] == '.nc':
                file_prefix = file_name[:-3]
            else:
                file_prefix = file_name
            ocgis.env.DIR_OUTPUT = os.getcwd()
            ocgis.env.OVERWRITE = True
            nc = netCDF4.Dataset(one_file, 'r')
            var_name = guess_main_variable(nc)
            nc.close()
            rd = ocgis.RequestDataset(one_file, var_name)
            for i, one_geom in enumerate(geom):
                if one_geom is None:
                    ocgis_geom = None
                else:
                    ocgis_geom = one_geom['geom']
                if mode == 'averager':
                    # Extent errors are ignored
                    try:
                        # Here with aggregate=True, can't pass the whole
                        # one_geom dictionary, is this a sign that this does
                        # not support multipolygon?
                        ops = ocgis.OcgOperations(
                            dataset=rd, geom=ocgis_geom,
                            spatial_operation='clip', aggregate=True,
                            time_range=tr, output_format='nc',
                            interpolate_spatial_bounds=True,
                            prefix=file_prefix).execute()
                    except ExtentError:
                        continue
                elif mode == 'subsetter':
                    # Extent errors are ignored
                    try:
                        # Still having problem with the geometry, previously
                        # was passing geom=[one_geom]
                        ops = ocgis.OcgOperations(
                            dataset=rd, geom=ocgis_geom, time_range=tr,
                            output_format='nc',
                            interpolate_spatial_bounds=True,
                            prefix=file_prefix).execute()
                    except ExtentError:
                        continue
                # Here, the global attribute 'subset_typename' and
                # 'subset_featureid' are added to the NetCDF file to keep
                # track of the feature used.
                if geom != [None]:
                    with netCDF4.Dataset(ops, 'a') as nc:
                        nc.subset_typename = typename
                        nc.subset_featureid = features[i]

                if geom == [None]:
                    mv_name = '{0}_{1}.nc'.format(
                        os.path.basename(ops)[:-3], 'subset')
                else:
                    mv_name = '{0}_{1}.nc'.format(
                        os.path.basename(ops)[:-3], features[i])
                mv_file = os.path.join(mv_dir, mv_name)
                shutil.move(ops, mv_file)
                output_files.append(mv_file)

                # Cover the case of an online wps server and the offline
                # mode for tests.
                if outputurl == 'file:///tmp':
                    disk_file = 'file:///' + mv_file.lstrip('/')
                    output_urls.append(disk_file)
                else:
                    url_file = os.path.join(
                        outputurl, os.path.basename(mv_dir), mv_name)
                    output_urls.append(url_file)
    except:
        raise Exception(traceback.format_exc())

    # If only ExtentError occured, the output_urls will be empty...
    if not output_urls:
        raise ExtentError(message="All ocgis calls returned ExtentError.")

    time_str = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
    output_file_name = "result_%s_.json" % (time_str,)
    output_file = os.path.join('/tmp', output_file_name)
    f1 = open(output_file, 'w')
    f1.write(json.dumps(output_urls))
    f1.close()
    response.outputs['output'].file = output_file
    response.outputs['output'].output_format = json_format
    response.update_status("done", 100)
    return response
