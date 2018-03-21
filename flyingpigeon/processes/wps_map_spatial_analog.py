from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.visualisation import map_spatial_analog, fig2plot

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from datetime import datetime as dt
from matplotlib import pyplot as plt

import logging
LOGGER = logging.getLogger("PYWPS")


class MapSpatialAnalogProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'netCDF dataset',
                         abstract="Dissimilarity between target at selected "
                                  "location and candidate distributions over the entire grid.",
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip')]),

            LiteralInput('fmt', 'format',
                         abstract="Figure format",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=5,
                         default='png',
                         allowed_values=['png', 'pdf', 'svg', 'ps', 'eps']),

            LiteralInput('title', 'Title',
                         abstract="Figure title.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='')
        ]

        outputs = [

            ComplexOutput('output_figure', 'Dissimilarity map',
                          abstract="Map of dissimilarity values.",
                          as_reference=True,
                          supported_formats=[Format('image/png'),
                                             Format('application/pdf'),
                                             Format('image/svg+xml'),
                                             Format('application/postscript'), ],
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]

        super(MapSpatialAnalogProcess, self).__init__(
            self._handler,
            identifier="map_spatial_analog",
            title="Map of dissimilarity values calculated by the spatial_analog process.",
            abstract="Produce map showing the dissimilarity values computed by the "
                     "spatial_analog process as well as indicating by a marker the location of the target site.",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        tic = dt.now()
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        LOGGER.info('Start process')
        response.update_status('Execution started at : {}'.format(tic), 1)

        ######################################
        # Read inputs
        ######################################
        try:
            resource = archiveextract(resource=rename_complexinputs(
                request.inputs['resource']))[0]
            fmts = [e.data for e in request.inputs['fmt']]
            title = request.inputs['title'][0].data

        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        try:
            fig = map_spatial_analog(resource, title=title)
            output = []

            for fmt in fmts:
                output.append(fig2plot(fig, fmt))

        except Exception as e:
            msg = "Failed to create figure: {}".format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        finally:
            plt.close()

        if len(fmts) == 1:
            output = output[0]
        else:
            output = archive(output)

        response.outputs['output_figure'].file = output
        response.update_status("done", 100)
        return response
