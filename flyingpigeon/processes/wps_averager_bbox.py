import traceback

from urlparse import urlparse
from pywps import Process, LiteralInput, ComplexOutput, get_format
from flyingpigeon.utils import CookieNetCDFTransfer

from flyingpigeon.handler_common import wfs_common

json_format = get_format('JSON')


class AveragerBboxProcess(Process):
    """Weighted spatial average of NetCDF file using bounding box geometry."""

    def __init__(self):
        inputs = [
            LiteralInput('resource',
                         'NetCDF resource',
                         abstract='NetCDF files, can be OPEnDAP urls.',
                         data_type='string',
                         max_occurs=1000),
            LiteralInput('lon0',
                         'Minimum longitude',
                         abstract='Minimum longitude.',
                         data_type='float'),
            LiteralInput('lon1',
                         'Maximum longitude',
                         abstract='Maximum longitude.',
                         data_type='float'),
            LiteralInput('lat0',
                         'Minimum latitude',
                         abstract='Minimum latitude.',
                         data_type='float'),
            LiteralInput('lat1',
                         'Maximum latitude',
                         abstract='Maximum latitude.',
                         data_type='float'),
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

        super(AveragerBboxProcess, self).__init__(
            self._handler,
            identifier='averager_bbox',
            title='Averager',
            version='0.1',
            abstract=('Return the data with weighted average of grid cells '
                      'intersecting the bounding box for each input '
                      'dataset as well as the time range selected.'),
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
                result = wfs_common(request, response, mode='averager',
                                    spatial_mode='bbox')
            return result
        except:
            raise Exception(traceback.format_exc())
