import logging
import traceback
# from urlparse import urlparse
from urllib.parse import urlparse

from pywps import Process, LiteralInput, ComplexInput, ComplexOutput, get_format, FORMATS

from flyingpigeon.handler_common import wfs_common
from eggshell.nc.nc_utils import CookieNetCDFTransfer

LOGGER = logging.getLogger("PYWPS")

json_format = get_format('JSON')


class SubsetBboxProcess(Process):
    """Subset a NetCDF file using bounding box geometry."""

    def __init__(self):
        inputs = [
            ComplexInput('resource',
                         'NetCDF resource',
                         abstract='NetCDF files, can be OPEnDAP urls.',
                         supported_formats=[FORMATS.NETCDF, FORMATS.DODS],
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
                          'NetCDF output for first resource file.',
                          as_reference=True,
                          supported_formats=[FORMATS.NETCDF]),
            ComplexOutput('metalink',
                          'Metalink file with links to all NetCDF outputs.',
                          as_reference=True,
                          supported_formats=[FORMATS.META4])
        ]

        super(SubsetBboxProcess, self).__init__(
            self._handler,
            identifier='subset-bbox',
            title='Subset BBox',
            version='0.2',
            abstract=('Return the data for which grid cells intersect the '
                      'bounding box for each input dataset as well as'
                      'the time range selected. This implies that the centroid of'
                      'a grid cell can be outside the bounding box.'),
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        try:
            #opendap_hostnames = [
            #    urlparse(r.url).hostname for r in request.inputs['resource']]
            #with CookieNetCDFTransfer(request, opendap_hostnames):
            result = wfs_common(request, response, mode='subsetter', workdir=self.workdir,
                                spatial_mode='bbox')
            return result
        except Exception as ex:
            msg = 'Connection to OPeNDAP failed: {}'.format(ex)
            LOGGER.exception(msg)
            raise Exception(traceback.format_exc())
