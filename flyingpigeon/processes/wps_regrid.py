from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
import ocgis
import ESMF

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")

# TODO: Add ESMF to environment.

# Supported interpolation methods
methods = map(str.lower, ESMF.RegridMethod.__members__.keys())


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
                         abstract="Regridding method",
                         default="auto",
                         allowed_values=methods,
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),
                         ]
        outputs = [
            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),

            ComplexOutput('tarout', 'Regridded datasets',
                          abstract="Tar archive containing the regridded netCDF files.",
                          as_reference=True,
                          supported_formats=[Format('application/x-tar')]
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
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        # -------------- #
        # Input handling #
        # -------------- #
        source = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        LOGGER.info("source: %s " % source)

        dest = archiveextract(
            resource=rename_complexinputs(request.inputs['dest']))
        LOGGER.info("dest: %s " % dest)

        method = request.inputs['method'].data[0]
        LOGGER.info("method: %s " % method)

        # -------------------- #
        # Regridding operation #
        # -------------------- #
        sources = ocgis.RequestDataset(source)
        d = ocgis.RequestDataset(dest)
        m = getattr(ESMF.RegridMethod, method.upper())

        outputs = []
        for s in sources:
            ops = ocgis.OcgOperations(dataset=s, regrid_destination=d, output_format='nc',
                                      regrid_options={'regrid_method': m})
            outputs.append(ops.execute())

        tarout_file = archive(outputs)

        response.outputs['tarout'].file = tarout_file
        return response
