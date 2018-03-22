from flyingpigeon import visualisation as vs
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import get_variable

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.inout.literaltypes import AllowedValue
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class PlottimeseriesProcess(Process):
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

            LiteralInput("variable", "Variable",
                         abstract="Variable to be expected in the input files (variable will be detected if not set)",
                         default=None,
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

            ComplexOutput("plotout_spagetti", "Visualisation, Spaghetti plot",
                          abstract="Visualisation of single variables as a spaghetti plot",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          ),

            ComplexOutput("plotout_uncertainty", "Visualisation Uncertainty plot",
                          abstract="Visualisation of single variables ensemble mean with uncertainty",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          )
        ]

        super(PlottimeseriesProcess, self).__init__(
            self._handler,
            identifier="plot_timeseries",
            title="Graphics (timeseries)",
            version="0.10",
            metadata=[
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            abstract="Outputs some timeseries of the file field means. Spaghetti and uncertainty plot",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        ncfiles = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))

        if 'variable' in request.inputs:
            var = request.inputs['variable'][0].data
        else:
            var = get_variable(ncfiles[0])
            #  var = ncfiles[0].split("_")[0]

        response.update_status('plotting variable %s' % var, 10)

        try:
            plotout_spagetti_file = vs.spaghetti(ncfiles,
                                                 variable=var,
                                                 title='Fieldmean of %s ' % (var),
                                                 )
            LOGGER.info("spagetti plot done")
            response.update_status('Spagetti plot for %s %s files done' % (len(ncfiles), var), 50)
            response.outputs['plotout_spagetti'].file = plotout_spagetti_file
        except Exception:
            raise Exception("spagetti plot failed")

        try:
            plotout_uncertainty_file = vs.uncertainty(ncfiles,
                                                      variable=var,
                                                      title='Ensemble uncertainty for %s ' % (var),
                                                      )

            response.update_status('Uncertainty plot for %s %s files done' % (len(ncfiles), var), 90)
            response.outputs['plotout_uncertainty'].file = plotout_uncertainty_file
            LOGGER.info("uncertainty plot done")
        except Exception as err:
            raise Exception("uncertainty plot failed %s" % err.message)

        response.update_status('visualisation done', 100)
        return response
