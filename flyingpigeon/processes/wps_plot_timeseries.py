import logging

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from eggshell.plot import plt_ncdata
from eggshell.utils import extract_archive
from eggshell.nc.nc_utils import get_variable
# from eggshell.utils import rename_complexinputs
# from eggshell.log import init_process_logger

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
            # ComplexOutput('output_log', 'Logging information',
            #               abstract="Collected logs during process run.",
            #               as_reference=True,
            #               supported_formats=[Format('text/plain')]
            #               ),

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
            version="0.11",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Outputs some timeseries of the file field means. Spaghetti and uncertainty plot",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        # init_process_logger('log.txt')
        # response.outputs['output_log'].file = 'log.txt'

        ncfiles = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource']],
            dir_output=self.workdir)

        if 'variable' in request.inputs:
            var = request.inputs['variable'][0].data
        else:
            var = get_variable(ncfiles[0])
            #  var = ncfiles[0].split("_")[0]

        response.update_status('plotting variable {}'.format(var), 10)

        try:
            plotout_spagetti_file = plt_ncdata.spaghetti(ncfiles,
                                                         variable=var,
                                                         title='Field mean of {}'.format(var),
                                                         dir_output=self.workdir,
                                                         )
            LOGGER.info("spagetti plot done")
            response.update_status('Spagetti plot for %s %s files done' % (len(ncfiles), var), 50)
            response.outputs['plotout_spagetti'].file = plotout_spagetti_file
        except Exception as e:
            raise Exception("spagetti plot failed : {}".format(e))

        try:
            plotout_uncertainty_file = plt_ncdata.uncertainty(ncfiles,
                                                              variable=var,
                                                              title='Ensemble uncertainty for {}'.format(var),
                                                              dir_output=self.workdir,
                                                              )

            response.update_status('Uncertainty plot for {} {} files done'.format(len(ncfiles), var), 90)
            response.outputs['plotout_uncertainty'].file = plotout_uncertainty_file
            LOGGER.info("uncertainty plot done")
        except Exception as err:
            raise Exception("uncertainty plot failed {}".format(err.message))

        response.update_status('visualisation done', 100)
        return response
