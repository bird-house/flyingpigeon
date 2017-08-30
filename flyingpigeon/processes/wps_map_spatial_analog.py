from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.visualisation import map_spatial_analog

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from datetime import datetime as dt
import os
from matplotlib import pyplot as plt

from ocgis import FunctionRegistry, RequestDataset, OcgOperations
from flyingpigeon.ocgisDissimilarity import Dissimilarity, metrics

FunctionRegistry.append(Dissimilarity)

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
                         max_occurs=1,
                         default='png',
                         allowed_values=['png', 'pdf', 'svg']),

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
                          supported_formats=[Format('application/pdf'),
                                             Format('image/svg'),
                                             Format('image/png'),
                                             ],
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
            abstract="Map showing the dissimilarity values computed by the spatial_analog process as well as indicating by a marker the location of the target site.",
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
        from tempfile import mkstemp

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
            fmt = request.inputs['fmt'][0].data
            title = request.inputs['title'][0].data

        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        try:
            fig = map_spatial_analog(resource, title=title)
            o1, graphic = mkstemp(dir='.', suffix='.'+fmt)
            fig.savefig(graphic)

        except Exception as e:
            msg = "Failed to create figure: {}".format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        finally:
            plt.close()

        response.outputs['output_figure'].file = graphic
        response.update_status("done", 100)
        return response
