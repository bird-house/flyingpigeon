import logging

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from datetime import datetime as dt

from pywps.app.Common import Metadata

from flyingpigeon.utils import extract_archive
from flyingpigeon.nc_utils import get_variable
from flyingpigeon.nc_statistic import robustness_stats
from flyingpigeon.nc_statistic import robustness_cc_signal
from flyingpigeon.plt_ncdata import plot_map_ccsignal
# from flyingpigeon.utils import rename_complexinputs
# from flyingpigeon.log import init_process_logger

LOGGER = logging.getLogger("PYWPS")


class ClimatechangesignalProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource_ref', 'Resource Refernce',
                         abstract='NetCDF Files (with one variable) or archive (tar/zip) containing NetCDF files.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            ComplexInput('resource_proj', 'Resource Projection',
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

            LiteralInput('datestart_ref', 'Start date for time period',
                         abstract="Beginning of period (YYYY-MM-DD). "
                                  "If not set, the first timestep will be considerd as start.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),

            LiteralInput('dateend_ref', 'End date for time period',
                         abstract="End of period (YYYY-MM-DD)."
                                  "If not set, the last timestep will be considerd as end.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),


            LiteralInput('datestart_proj', 'Start date for time period',
                         abstract="Beginning of period (YYYY-MM-DD). "
                                  "If not set, the first timestep will be considerd as start.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),

            LiteralInput('dateend_proj', 'End date for time period',
                         abstract="End of period (YYYY-MM-DD)."
                                  "If not set, the last timestep will be considerd as end.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),

            LiteralInput('vmin', 'vmin',
                         abstract='Minimum limit of colorbar.',
                         data_type='float',
                         default=None,
                         min_occurs=0,
                         max_occurs=1,),


            LiteralInput('vmax', 'vmax',
                         abstract='Maximum limit of colorbar.',
                         data_type='float',
                         default=None,
                         min_occurs=0,
                         max_occurs=1,),


            LiteralInput("cmap", "Colormap",
                         abstract="Colormap according to python matplotlib.",
                         default='RdYlBu_r',
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),


            LiteralInput("title", "Title",
                         abstract="Title to be written over the graphic",
                         default=None,
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),
        ]

        outputs = [
            ComplexOutput("output_cc_signal", "Climate Change Signal",
                          abstract="netCDF file containing the ensemble median",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput("output_ensstd", "Mean Standard Deviation",
                          abstract="netCDF file containing the ensemble standard deviation",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput("plot_cc_signal", "Visualisation, spatial climate signal plot",
                          abstract="Visualisation of map realized with cartopy.",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          ),

            ]

        super(ClimatechangesignalProcess, self).__init__(
            self._handler,
            identifier="climatechange_signal",
            title="Climate Change Signal",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Calculates the Climate Change signal "
                     "of an ensemble between two given timeperiods",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        # init_process_logger('log.txt')
        # response.outputs['output_log'].file = 'log.txt'

        ncfiles_ref = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource_ref']],
            dir_output=self.workdir)

        ncfiles_proj = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource_proj']],
            dir_output=self.workdir)

        if 'variable' in request.inputs:
            var = request.inputs['variable'][0].data
        else:
            var = get_variable(ncfiles_ref[0])
            #  var = ncfiles[0].split("_")[0]

        response.update_status('ensemble variable {}'.format(var), 10)

        if 'datestart_ref' in request.inputs:
            datestart_ref = request.inputs['datestart_ref'][0].data
        else:
            datestart_ref = None

        if 'dateend_ref' in request.inputs:
            dateend_ref = request.inputs['dateend_ref'][0].data
        else:
            dateend_ref = None

        if 'datestart_proj' in request.inputs:
            datestart_proj = request.inputs['datestart_proj'][0].data
        else:
            datestart_proj = None

        if 'dateend_proj' in request.inputs:
            dateend_proj = request.inputs['dateend_proj'][0].data
        else:
            dateend_proj = None

        cmap = request.inputs['cmap'][0].data

        if 'vmin' in request.inputs:
            vmin = request.inputs['vmin'][0].data
        else:
            vmin = None

        if 'vmax' in request.inputs:
            vmax = request.inputs['vmax'][0].data
        else:
            vmax = None

        if 'title' in request.inputs:
            title = request.inputs['title'][0].data
        else:
            title = None

        LOGGER.debug('time region set to {}-{}'.format(
                                                dt.strftime(datestart_ref, '%Y-%m-%d'),
                                                dt.strftime(dateend_ref, '%Y-%m-%d')))
        #
        # dateStart = dt.strptime(dateStart_str, '%Y-%m-%d'),
        # dateEnd = dt.strptime(dateStart_str, '%Y-%m-%d'),

        try:
            output_ensmean_ref, output_ensstd_ref = robustness_stats(ncfiles_ref,
                                                                     time_range=[datestart_ref, dateend_ref],
                                                                     dir_output=self.workdir)
            LOGGER.info("Ensemble Statistic calculated for reference periode ")
            response.update_status('Ensemble Statistic calculated for reference periode', 30)
        except Exception as e:
            raise Exception("Ensemble Statistic calculation failed for reference periode: {}".format(e))

        try:
            output_ensmean_proj, output_ensstd_proj = robustness_stats(ncfiles_proj,
                                                                       time_range=[datestart_proj, dateend_proj],
                                                                       dir_output=self.workdir)

            LOGGER.info("Ensemble Statistic calculated for projection periode ")
            response.update_status('Ensemble Statistic calculated for projection periode', 60)
        except Exception as e:
            raise Exception("Ensemble Statistic calculation failed for projection periode: {}".format(e))

        try:
            out_cc_signal, out_mean_std = robustness_cc_signal(variable_mean=[output_ensmean_ref, output_ensmean_proj],
                                                               standard_deviation=[output_ensstd_ref,
                                                                                   output_ensstd_proj],
                                                               dir_output=self.workdir)
            LOGGER.info("Climate Change signal calculated")
            response.update_status('Climate Change signal calculated', 90)
        except Exception as e:
            raise Exception("Climate Change signal calculation failed: {}".format(e))

        try:
            out_graphic = plot_map_ccsignal(signal=out_cc_signal,
                                            robustness=None,
                                            vmin=vmin, vmax=vmax,
                                            title=title,
                                            cmap=cmap)
            LOGGER.info("Climate Change signal plotted")
            response.update_status('Climate Change signal graphic plotted', 95)
        except Exception as e:
            raise Exception("Climate Change signal plotting failed: {}".format(e))

        response.outputs['output_cc_signal'].file = out_cc_signal
        response.outputs['output_ensstd'].file = out_mean_std
        response.outputs['plot_cc_signal'].file = out_graphic

        response.update_status('Ensemble Statistic calculation done', 100)
        return response
