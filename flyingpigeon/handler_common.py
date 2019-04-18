import os
import shutil
import time
import json
import tempfile
import traceback

from pywps import Process, get_format, configuration, FORMATS
from pywps.inout.outputs import MetaFile, MetaLink4
import owslib
from owslib.wfs import WebFeatureService
import ocgis
from ocgis.exc import ExtentError
import netCDF4 as nc
from shapely.geometry import shape
import requests
from pathlib import Path
from eggshell.nc.nc_utils import get_variable, opendap_or_download


def get_feature(url, typename, features):
    """Return geometry for WFS server."""
    wfs = WebFeatureService(url, version='2.0.0')
    resp = wfs.getfeature([typename], featureid=features,
                           outputFormat='application/json')
    return json.loads(resp.read())


def make_geoms(feature, mosaic=False):
    crs_code = owslib.crs.Crs(
        feature['crs']['properties']['name']).code
    crs = ocgis.CoordinateReferenceSystem(epsg=crs_code)
    geoms = [
        {'geom': shape(f['geometry']), 'crs': crs,
         'properties': f['properties']}
        for f in feature['features']]

    if mosaic:
        new_geom = geoms[0]
        for merge_geom in geoms[1:]:
            new_geom['geom'] = new_geom['geom'].union(merge_geom['geom'])
        new_geom['properties'] = {'bbox': feature['bbox']}
        return new_geom

    return geoms


def opendap_or_netcdf_path(inputs):
    """Return a generator returning for all input values an OPeNDAP url of the file path.

    :param pywps.ComplexInput input: NetCDF resource.
    :return: path to dataset.
    """

    for input in inputs:
        url = input.url
        if url and not url.startswith("file"):
            r = requests.get(url + ".dds")
            if r.status_code == 200 and r.content.decode().startswith("Dataset"):
                path = url
        else:
            # Accessing the file property loads the data in the data property
            # and writes it to disk
            path = input.file

            # We need to cleanup the data property, otherwise it will be
            # written in the database and to the output status xml file
            # and it can get too large.
            input._data = ""

        yield path


class Subsetter(Process):

    def parse_resources(self, input):
        """Return a generator opening netCDF files either through OPeNDAP or downloading an external file."""
        for r in input:
            yield try_opendap(r)



def wfs_common(request, response, mode, workdir, spatial_mode='wfs'):
    """Common part of wps process for wfs operations.

    :param request: request for wps process handler
    :param response: response for wps process handler
    :param mode: 'subsetter' or 'averager'

    :return response: wps process response

    """

    #outputpath = configuration.get_config_value('server', 'outputpath')
    #outputurl = configuration.get_config_value('server', 'outputurl')


    #for one_resource in request.inputs['resource']:
        # Download if not opendap
        # Adding a maximum file size from a server config file would
        # be possible here...
        #try:
        #    nc_file = opendap_or_download(
        #        one_resource.data,
        #        auth_tkt_cookie=request.http_request.cookies,
        #        output_path='/tmp')
        #except Exception:
        #    raise Exception(traceback.format_exc())
        #list_of_files.append(nc_file)


    if ('typename' in request.inputs) and ('featureids' in request.inputs):
        typename = request.inputs['typename'][0].data
        features = [f.data for f in request.inputs['featureids']]
        if 'geoserver' in request.inputs:
            geoserver = request.inputs['geoserver'][0].data
        else:
            geoserver = configuration.get_config_value('extra', 'geoserver')

        mosaic = request.inputs['mosaic'][0].data

        try:
            feature = get_feature(geoserver, typename, features)
            geoms = make_geoms(feature, mosaic)

        except Exception as e:
            msg = ('Failed to fetch features.\ngeoserver: {0} \n'
                   'typename: {1}\nfeatures {2}\n{3}').format(
                geoserver, typename, features, e)
            raise Exception(msg)

    elif spatial_mode == 'bbox':
        geoms = [[request.inputs['lon0'][0].data,
                 request.inputs['lat0'][0].data,
                 request.inputs['lon1'][0].data,
                 request.inputs['lat1'][0].data]]
    else:
        geoms = [None]

    if ('initial_datetime' in request.inputs) and \
       ('final_datetime' in request.inputs):
        tr = [request.inputs['initial_datetime'][0].data,
              request.inputs['final_datetime'][0].data]
    else:
        tr = None

    try:
        files = []
        # os.chmod(mv_dir, 0o755)

        for path in opendap_or_netcdf_path(request.inputs['resource']):
            p = Path(path)

            ocgis.env.DIR_OUTPUT = tempfile.mkdtemp(dir=workdir)
            ocgis.env.OVERWRITE = True
            if 'variable' in request.inputs:
                var_names = [v.data for v in request.inputs['variable']]
            else:
                ds = nc.Dataset(path)
                var_names = get_variable(ds)
                ds.close()

            rd = ocgis.RequestDataset(path, var_names)

            for i, one_geom in enumerate(geoms):
                if one_geom is None:
                    ocgis_geom = None
                elif spatial_mode == 'bbox':
                    ocgis_geom = one_geom
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
                            prefix=p.stem).execute()
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
                            prefix=p.stem).execute()
                    except ExtentError:
                        continue
                # Here, the global attribute 'subset_typename' and
                # 'subset_featureid' are added to the NetCDF file to keep
                # track of the feature used.
                if (geoms != [None]) and (spatial_mode == 'wfs'):
                    with nc.Dataset(ops, 'a') as ds:
                        ds.subset_typename = typename
                        ds.subset_featureid = features[i]

                if (spatial_mode == 'wfs') and \
                   ('featureids' in request.inputs):
                    mv_name = '{0}_{1}.nc'.format(
                        os.path.basename(ops)[:-3], features[i])
                else:
                    mv_name = '{0}_{1}.nc'.format(
                        os.path.basename(ops)[:-3], 'subset')

                mv_file = os.path.join(ocgis.env.DIR_OUTPUT, mv_name)
                shutil.move(ops, mv_file)
                mf = MetaFile(mv_name, fmt=FORMATS.NETCDF)
                mf.file = mv_file
                files.append(mf)
                # shutil.rmtree(ocgis.env.DIR_OUTPUT)


    except Exception:
        raise Exception(traceback.format_exc())

    # If only ExtentError occured, the output_files will be empty...
    if not files:
        raise ExtentError(message="All ocgis calls returned ExtentError.")

    ml = MetaLink4('subset', workdir=workdir, files=files)
    response.outputs['output'].file = ml.files[0].file
    response.outputs['metalink'].data = ml.xml
    response.update_status("Completed", 100)
    return response
