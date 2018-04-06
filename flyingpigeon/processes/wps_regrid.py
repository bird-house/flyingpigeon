from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
import ocgis
import ESMF

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, configuration, get_format
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger

import os
import logging
LOGGER = logging.getLogger("PYWPS")

json_format = get_format('JSON')

# Supported interpolation methods
methods = list(map(str.lower, ESMF.RegridMethod.__members__.keys()))

def actual_output_path(fn):
    """Return the path to an output file, adjusting for whether or not the server is active or not.

    Example
    -------
    On a local server it would yield something like::

       http://localhost:8090/wpsoutputs/flyingpigeon/af06fb/af06fb.nc

    While in test mode it would yield::

       file:///tmp/af06fb/af06fb.nc

    """
    outputurl = configuration.get_config_value('server', 'outputurl')
    outputpath = configuration.get_config_value('server', 'outputpath')

    return os.path.join(outputurl, os.path.relpath(fn, outputpath))


class ESMFRegridProcess(Process):
    """
    Notes
    -----

    Bilinear
        Destination value is a linear combination of the
        source values in the cell which contains the destination point. The weights
        for the linear combination are based on the distance of the destination
        point from each source value.

    Patch
        Higher-order patch recovery interpolation. Destination value is a weighted
        average of 2D polynomial patches constructed from cells surrounding the
        source cell which contains the destination point. This method typically
        results in better approximations to values and derivatives than bilinear.
        However, because of its larger stencil, it also results in a much larger
        interpolation matrix than the bilinear method.

    Conserve
        First order conservative interpolation. Value of a destination cell is the
        weighted sum of the values of the source cells that it overlaps. The
        weights are determined by the amount the source cell overlaps the
        destination cell. This method will typically give less accurate
        approximations to values than the other interpolation methods, however, it
        will do a much better job preserving the integral of the value between the
        source and destination. This method requires the corner coordinate values
        to be provided in the Grid, and it currently only works for Fields created
        on the Grid center stagger (or the Mesh element location).

    Nearest_STOD
        In this version of nearest neighbor interpolation each destination point is
        mapped to the closest source point. A given source point may go to multiple
        destination points, but no destination point will receive input from more
        than one source point.

    Nearest_DTOS
        In this version of nearest neighbor interpolation each source point is
        mapped to the closest destination point. A given destination point may
        receive input from multiple source points, but no source point will go to
        more than one destination point.
    """

    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files or archive (tar/zip) containing NetCDF files.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            ComplexInput('dest', 'Grid destination',
                         abstract='NetCDF file whose grid defines the interpolation target.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput("method", "Regridding method",
                         abstract="Regridding method. Note that `conserve` requires grid corners to be defined.",
                         default="bilinear",
                         allowed_values=methods,
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("snippet", "Snippet",
                         abstract="Run process only for first time step.",
                         default="False",
                         data_type="boolean",
                         min_occurs=0,
                         max_occurs=1)
                         ]
        outputs = [
            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),

            ComplexOutput('output', 'Links to regridded dataset',
                          abstract="JSON file listing the regridded netCDF URLs.",
                          as_reference=True,
                          supported_formats=[json_format]
                          ),

            ComplexOutput('output_netcdf', 'NetCDF file',
                          abstract="First NetCDF file generated by process.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),
        ]

        super(ESMFRegridProcess, self).__init__(
            self._handler,
            identifier="esmf_regrid",
            title="ESMF regridding",
            abstract='Regrid netCDF files to a destination grid.',
            version="0.10",
            metadata=[
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        import uuid
        import time
        import json
        outputpath = configuration.get_config_value('server', 'outputpath')
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        # -------------- #
        # Input handling #
        # -------------- #
        resource = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        LOGGER.info("resource: %s " % resource)

        dest = archiveextract(
            resource=rename_complexinputs(request.inputs['dest']))
        LOGGER.info("dest: %s " % dest)

        method = request.inputs['method'][0].data
        LOGGER.info("method: %s " % method)

        snippet = request.inputs['snippet'][0].data
        LOGGER.info("snippet: %s " % snippet)

        # -------------------- #
        # Regridding operation #
        # -------------------- #
        d = ocgis.RequestDataset(dest)
        m = getattr(ESMF.RegridMethod, method.upper())
        LOGGER.info('Start ocgis module call function')

        # Prepare the environment
        ocgis.env.OVERWRITE = True
        prefix = str(uuid.uuid1())
        ocgis.env.PREFIX = prefix

        outputs = []
        for source in resource:
            s = ocgis.RequestDataset(source)
            ops = ocgis.OcgOperations(dataset=s, regrid_destination=d, regrid_options={'regrid_method': m},
                                      snippet=snippet,
                                      dir_output=outputpath, output_format='nc', prefix=prefix
                                      )
            outputs.append(ops.execute())

        response.outputs['output_netcdf'].file = outputs[0]

        time_str = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        output_json = "esmf_regrid_results_{}.json".format(time_str)
        with open(output_json, 'w') as f:
            f.write(json.dumps([actual_output_path(o) for o in outputs]))

        response.outputs['output'].file = output_json
        response.outputs['output'].output_format = json_format
        return response
