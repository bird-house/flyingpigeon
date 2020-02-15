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


class PlottimemeanProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files (with one variable) or archive (tar/zip) containing NetCDF files.'\
                                  'if multiple files are provided, a mean over all will be displayed',
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

            LiteralInput("title", "Title",
                         abstract="Title to be written over the graphic",
                         default=None,
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('dateStart', 'Start date for time period',
                         abstract="Beginning of period (YYYY-MM-DD). "
                                  "If not set, the first timestep will be considerd as start.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),

            LiteralInput('dateEnd', 'End date for time period',
                         abstract="End of period (YYYY-MM-DD)."
                                  "If not set, the last timestep will be considerd as end.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         default=None,
                         ),


            LiteralInput("cmap", "Colormap",
                         abstract="Colormap according to python matplotlib.",
                         default='RdYlBu_r',
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

          LiteralInput('delta', 'Delta',
                       abstract='To set an offset for the values.\
                        e.g. -273.15 to transform Kelvin to Celsius',
                       data_type='float',
                       default=0,),

        ]

        outputs = [
            ComplexOutput("plotout_map", "Visualisation, spatial map plot",
                          abstract="Visualisation of map realized with cartopy.",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          ),
            ]

        super(PlottimemeanProcess, self).__init__(
            self._handler,
            identifier="plot_map_timemean",
            title="Plot Map",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Visualisation of map realized with cartopy as a mean over timestepps"\
                     "If multiple files are provided, an ensemble mean over the means of the single files will be calculated",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        # init_process_logger('log.txt')
        # response.outputs['output_log'].file = 'log.txt'

        LOGGER.info('Collecting Arguments for the process')

        ncfiles = extract_archive(
            resources=[inpt.file for inpt in request.inputs['resource']],
            dir_output=self.workdir)

        if 'variable' in request.inputs:
            var = request.inputs['variable'][0].data
        else:
            var = get_variable(ncfiles[0])
            #  var = ncfiles[0].split("_")[0]

        cmap = request.inputs['cmap'][0].data
        delta = request.inputs['delta'][0].data

        if 'dateStart' in request.inputs:
            dateStart = request.inputs['dateStart'][0].data
        else:
            dateStart = None
        if 'dateEnd' in request.inputs:
            dateEnd = request.inputs['dateEnd'][0].data
        else:
            dateEnd = None

        if 'title' in request.inputs:
            title = request.inputs['title'][0].data
        else:
            title = None

        response.update_status('Variable for the plot: {}'.format(var), 10)

        try:
            LOGGER.info('Start ploting map')
            response.update_status('Start ploting map', 20)

            plotout_map_file = plt_ncdata.plot_map_timemean(ncfiles,
                                                            variable=var,
                                                            title=title,
                                                            dir_output=self.workdir,
                                                            cmap=cmap,
                                                            delta=delta,
                                                            time_range=[dateStart, dateEnd]
                                                            )
            LOGGER.info("plot map done")
            response.update_status('plot map done', 50)

            response.outputs['plotout_map'].file = plotout_map_file
        except Exception as e:
            raise Exception("Plot Map failed : {}".format(e))

        response.update_status('visualisation plot map done', 100)
        return response
