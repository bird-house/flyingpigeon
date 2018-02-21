from flyingpigeon.utils import archive, archiveextract
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

# TODO: Add ESMF to environment.

# Supported interpolation methods
methods = list(map(str.lower, ESMF.RegridMethod.__members__.keys()))


def extract_doc():
    """Format the documentation about the ESMF regridding methods."""
    import inspect
    import re

    source = inspect.getsource(ESMF.RegridMethod)
    doc = source.replace('"""', '')

    def title(match):
        [name] = match.groups()
        n = len(name)
        return '\n    ' + name + '\n    ' + n*'~'

    doc = re.sub('(\w+) = \d', title, doc)
    lines = doc.splitlines()[3:]
    lines.insert(0, '    Notes')
    lines.insert(1, '    -----')

    return '\n'.join(lines)

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
    __doc__ = extract_doc()

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
            outputs.append(actual_output_path(ops.execute()))

        time_str = time.strftime("%Y-%m-%dT%H:%M:%SZ", time.gmtime())
        output_file = "esmf_regrid_results_{}.json".format(time_str)
        with open(output_file, 'w') as f:
            f.write(json.dumps(outputs))

        response.outputs['output'].file = output_file
        response.outputs['output'].output_format = json_format
        return response
