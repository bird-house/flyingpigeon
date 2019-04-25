from datetime import datetime as dt

# from eggshell.log import init_process_logger
from matplotlib import pyplot as plt
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from eggshell.utils import archive, extract_archive
# from eggshell.utils import rename_complexinputs
from eggshell.plot.plt_utils import fig2plot
from eggshell.plot.plt_ncdata import plot_spatial_analog

import logging
LOGGER = logging.getLogger("PYWPS")


class PlotSpatialAnalogProcess(Process):
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

            # ComplexOutput('output_log', 'Logging information',
            #               abstract="Collected logs during process run.",
            #               as_reference=True,
            #               supported_formats=[Format('text/plain')]
            #               ),
        ]

        super(PlotSpatialAnalogProcess, self).__init__(
            self._handler,
            identifier="plot_spatial_analog",
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
        # init_process_logger('log.txt')
        # response.outputs['output_log'].file = 'log.txt'

        LOGGER.info('Start process')
        response.update_status('Execution started at : {}'.format(tic), 1)

        ######################################
        # Read inputs
        ######################################
        try:
            resource = extract_archive(
                resources=[inpt.file for inpt in request.inputs['resource']],
                dir_output=self.workdir)[0]
            fmts = [e.data for e in request.inputs['fmt']]
            title = request.inputs['title'][0].data

        except Exception as ex:
            msg = 'Failed to read input parameter {}'.format(ex)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        try:
            fig = plot_spatial_analog(resource, title=title)
            output = []

            for fmt in fmts:
                output.append(fig2plot(fig, fmt, dir_output=self.workdir))

        except Exception as ex:
            msg = "Failed to create figure: {}".format(ex)
            LOGGER.error(msg)
            raise Exception(msg)

        finally:
            plt.close()

        if len(fmts) == 1:
            output = output[0]
        else:
            output = archive(output, dir_output=self.workdir)

        response.outputs['output_figure'].file = output
        response.update_status("done", 100)
        return response
