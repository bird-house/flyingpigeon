import json
from pywps import configuration
import owslib
from owslib.wfs import WebFeatureService
import ocgis
import netCDF4 as nc
from shapely.geometry import shape
import requests

from flyingpigeon.nc_utils import get_variable


def get_feature(url, typename, features):
    """Return geometry from WFS server."""
    wfs = WebFeatureService(url, version='2.0.0')
    resp = wfs.getfeature([typename], featureid=features,
                          outputFormat='application/json',
                          method="Post")
    return json.loads(resp.read())


def is_opendap_url(url):
    """
    Check if a provided url is an OpenDAP url.
    The DAP Standard specifies that a specific tag must be included in the
    Content-Description header of every request. This tag is one of:
        "dods-dds" | "dods-das" | "dods-data" | "dods-error"
    So we can check if the header starts with `dods`.
    Even then, some OpenDAP servers seem to not include the specified header...
    So we need to let the netCDF4 library actually open the file.
    """
    from requests.exceptions import MissingSchema, InvalidSchema
    from requests.exceptions import ConnectionError as reqCE

    try:
        content_description = requests.head(url, timeout=5).headers.get("Content-Description")
    except (ConnectionError, reqCE, MissingSchema, InvalidSchema):
        return False

    if content_description:
        return content_description.lower().startswith("dods")
    else:
        try:
            dataset = nc.Dataset(url)
        except OSError:
            return False
        return dataset.disk_format in ('DAP2', 'DAP4')


def make_geoms(feature, union=False):
    """Return list of feature dictionaries.

    Parameters
    ----------
    feature : list
      List of features.
    union : bool
      If True return the union of all geometries.
    """

    crs_code = owslib.crs.Crs(
        feature['crs']['properties']['name']).code
    crs = ocgis.CoordinateReferenceSystem(epsg=crs_code)
    geoms = [
        {'geom': shape(f['geometry']), 'crs': crs,
         'properties': f['properties']}
        for f in feature['features']]

    if union:
        new_geom = geoms[0]
        for merge_geom in geoms[1:]:
            new_geom['geom'] = new_geom['geom'].union(merge_geom['geom'])
        new_geom['properties'] = {'bbox': feature['bbox']}
        return [new_geom, ]

    return geoms


class Subsetter:

    def parse_resources(self, request):
        """Return a generator returning for all input values an OPeNDAP url of the file path.

        :param request: WPS request object.
        :return: path to dataset.
        """

        for input in request.inputs['resource']:
            url = input.url
            if is_opendap_url(url):
                yield url
            else:
                # Accessing the file property loads the data in the data property
                # and writes it to disk
                path = input.file

                # We need to cleanup the data property, otherwise it will be
                # written in the database and to the output status xml file
                # and it can get too large.
                input._data = ""

                yield path

    def parse_feature(self, request, union=False):
        """Parse individual features and aggregate them if mosaic is True.

        Parameters
        ----------
        request : PyWPS.WPSRequest
          Execution request.
        union : bool
          If True return the union of all geometries.

        Returns
        -------
        out : dict
          Geometries keyed by feature id. If mosaic is true, the key
          is 'mosaic'.
        """

        typename = request.inputs['typename'][0].data
        featureids = [f.data for f in request.inputs['featureids']]

        if 'geoserver' in request.inputs:
            geoserver = request.inputs['geoserver'][0].data
        else:
            geoserver = configuration.get_config_value('extra', 'geoserver')

        try:
            feature = get_feature(geoserver, typename, featureids)
            geoms = make_geoms(feature, union=union)

        except Exception as e:
            msg = ('Failed to fetch features.\ngeoserver: {0} \n'
                   'typename: {1}\nfeatures {2}\n{3}').format(
                geoserver, typename, featureids, e)
            raise Exception(msg) from e

        return geoms, featureids

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
