import logging

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from flyingpigeon.utils import extract_archive
from flyingpigeon.nc_utils import get_variable
from flyingpigeon.calculation import robustness_stats
# from flyingpigeon.utils import rename_complexinputs
# from flyingpigeon.log import init_process_logger

LOGGER = logging.getLogger("PYWPS")


class RobustnesstatisticProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files (with one variable) or archive (tar/zip) containing NetCDF files.',
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

            LiteralInput('dateStart', 'Start date for time period',
                         abstract="Beginning of period (YYYY-MM-DD). "
                                  "If not set, the first timestep will be condiderd as start.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),

            LiteralInput('dateEnd', 'End date for time period',
                         abstract="End of period (YYYY-MM-DD)."
                                  "If not set, the last timestep will be condiderd as end.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),
        ]

        outputs = [
            ComplexOutput("output_ensmean", "Ensemble Mean",
                          abstract="netCDF file containing the ensemble median",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput("output_ensstd", "Ensemble Standard Deviation",
                          abstract="netCDF file containing the ensemble standard deviation",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),
            ]

        super(RobustnesstatisticProcess, self).__init__(
            self._handler,
            identifier="robustness_statistic",
            title="Ensemble Statistic",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Calculates median and percentils"
                     "of an ensemble over a given timeperiod",
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

        response.update_status('ensemble variable {}'.format(var), 10)

        dateStart = request.inputs['dateStart'][0].data
        dateEnd = request.inputs['dateEnd'][0].data

        from datetime import datetime as dt
        LOGGER.debug('time region set to {}-{}'.format(dt.strftime(dateStart, '%Y-%m-%d'),
                                                       dt.strftime(dateEnd, '%Y-%m-%d')))
        #
        # dateStart = dt.strptime(dateStart_str, '%Y-%m-%d'),
        # dateEnd = dt.strptime(dateStart_str, '%Y-%m-%d'),

        try:
            output_ensmean, output_ensstd = robustness_stats(ncfiles,
                                                             time_range=[dateStart, dateEnd],
                                                             dir_output=self.workdir)

            LOGGER.info("Ensemble Statistic calculated ")
            response.update_status('Ensemble Statistic calculated', 50)
            response.outputs['output_ensmean'].file = output_ensmean
            response.outputs['output_ensstd'].file = output_ensstd

        except Exception as e:
            raise Exception("Ensemble Statistic calculation failed : {}".format(e))

        response.update_status('Ensemble Statistic calculation done', 100)
        return response
