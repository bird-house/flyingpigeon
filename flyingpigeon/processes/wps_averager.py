import traceback

from pywps import Process, LiteralInput, ComplexOutput, get_format

from flyingpigeon.handler_common import wfs_common

json_format = get_format('JSON')


class AveragerProcess(Process):
    """Weighted spatial average of a NetCDF file using a WFS geometry."""

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
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1),
            LiteralInput('featureids',
                         'Feature Ids',
                         abstract='fid(s) of the feature in the layer.',
                         data_type='string',
                         min_occurs=0,
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
            LiteralInput('initial_datetime',
                         'Initial datetime',
                         abstract='Initial datetime for temporal subsetting.',
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1),
            LiteralInput('final_datetime',
                         'Final datetime',
                         abstract='Final datetime for temporal subsetting.',
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1),
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

        super(AveragerProcess, self).__init__(
            self._handler,
            identifier='averager',
            title='Averager',
            version='0.1',
            abstract=('Return the data with weighted average of grid cells '
                      'intersecting the selected polygon for each input '
                      'dataset as well as the time range selected.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        try:
            return wfs_common(request, response, mode='averager')
        except:
            raise Exception(traceback.format_exc())
