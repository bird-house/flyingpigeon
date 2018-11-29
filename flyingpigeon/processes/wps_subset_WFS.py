import traceback
from urlparse import urlparse
import logging
from flyingpigeon.handler_common import wfs_common
from flyingpigeon.utils import CookieNetCDFTransfer
from pywps import Process, LiteralInput, ComplexOutput, get_format

LOGGER = logging.getLogger("PYWPS")

json_format = get_format('JSON')


class SubsetWFSProcess(Process):
    """Subset a NetCDF file using WFS geometry."""

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
            LiteralInput('mosaic',
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

        super(SubsetWFSProcess, self).__init__(
            self._handler,
            identifier='subset_WFS',
            title='Subset WFS',
            version='0.1',
            abstract=('Return the data for which grid cells intersect the '
                      'selected polygon for each input dataset.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        try:
            opendap_hostnames = [
                urlparse(r.data).hostname for r in request.inputs['resource']]
            with CookieNetCDFTransfer(request, opendap_hostnames):
                result = wfs_common(request, response, mode='subsetter')
            return result
        except Exception as ex:
            msg = 'Connection to OPeNDAP failed: {}'.format(ex)
            LOGGER.exception(msg)
            raise Exception(traceback.format_exc())
