import json
from pywps import LiteralInput, ComplexInput, ComplexOutput
from pywps import configuration, FORMATS
import owslib
from owslib.wfs import WebFeatureService
import ocgis
import netCDF4 as nc
from shapely.geometry import shape
import requests

from eggshell.nc.nc_utils import get_variable

resource = ComplexInput('resource',
                        'NetCDF resource',
                        abstract='NetCDF files, can be OPEnDAP urls.',
                        supported_formats=[FORMATS.NETCDF, FORMATS.DODS],
                        max_occurs=1000)

start = LiteralInput('start',
                     'Initial datetime',
                     abstract='Initial datetime for temporal subsetting.',
                     data_type='dateTime',
                     min_occurs=0,
                     max_occurs=1)

end = LiteralInput('end',
                   abstract='Final datetime for temporal subsetting.',
                   data_type='dateTime',
                   min_occurs=0,
                   max_occurs=1)

variable = LiteralInput('variable',
                        'Variable',
                        abstract=('Name of the variable in the NetCDF file.'
                                  'Will be guessed if not provided.'),
                        data_type='string',
                        min_occurs=0)

output = ComplexOutput('output',
                       'NetCDF output for first resource file.',
                       as_reference=True,
                       supported_formats=[FORMATS.NETCDF])

metalink = ComplexOutput('metalink',
                         'Metalink file with links to all NetCDF outputs.',
                         as_reference=True,
                         supported_formats=[FORMATS.META4])


def get_feature(url, typename, features):
    """Return geometry for WFS server."""
    wfs = WebFeatureService(url, version='2.0.0')
    resp = wfs.getfeature([typename], featureid=features,
                          outputFormat='application/json')
    return json.loads(resp.read())


def make_geoms(feature, mosaic=False):
    """Return list of feature dictionaries."""

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


class Subsetter:

    def parse_resources(self, request):
        """Return a generator returning for all input values an OPeNDAP url of the file path.

        :param [pywps.ComplexInput,] resources: List of NetCDF resources.
        :return: path to dataset.
        """

        for input in request.inputs['resource']:
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

    def parse_feature(self, request):
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
        else:
            geoms = [None, ]

        return geoms

    def parse_daterange(self, request):
        """Return [start, end] or None."""
        if ('start' in request.inputs) and ('end' in request.inputs):
            tr = [request.inputs['start'][0].data,
                  request.inputs['end'][0].data]
        else:
            tr = None
        return tr

    def parse_bbox(self, request):
        return [request.inputs['lon0'][0].data,
                request.inputs['lat0'][0].data,
                request.inputs['lon1'][0].data,
                request.inputs['lat1'][0].data]

    def parse_variable(self, request, path):
        """Parse variables specified in request and confirm they are present in file.
        If no variable is specified in the request, guess the variables from the file content.

        :return: List of variable names.
        """
        ds = nc.Dataset(path)

        if 'variable' in request.inputs:
            var_names = [v.data for v in request.inputs['variable']]
            for var in var_names:
                if var not in ds.variables:
                    raise ValueError("{} not in {}".format(var, path))

        else:
            var_names = get_variable(ds)

        ds.close()
        return var_names
