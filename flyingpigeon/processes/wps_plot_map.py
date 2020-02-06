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


class PlotmapProcess(Process):
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

            LiteralInput("title", "Title",
                         abstract="Title to be written over the graphic",
                         default=None,
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
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

        super(PlotmapProcess, self).__init__(
            self._handler,
            identifier="plot_map",
            title="Plot Map",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Visualisation of map realized with cartopy as a mean over timestepps",
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

        if 'title' in request.inputs:
            title = request.inputs['title'][0].data
        else:
            title = None

        response.update_status('Variable for the plot: {}'.format(var), 10)

        try:
            LOGGER.info('Start ploting map')
            response.update_status('Start ploting map', 20)
            for ncfile in ncfiles:
                plotout_map_file = plt_ncdata.plot_map(ncfile,
                                                       variable=var,
                                                       title=title,
                                                       dir_output=self.workdir,
                                                       cmap=cmap,
                                                       delta=delta,
                                                       )
            LOGGER.info("plot map done")
            response.update_status('plot map done', 50)

            response.outputs['plotout_map'].file = plotout_map_file
        except Exception as e:
            raise Exception("Plot Map failed : {}".format(e))

        response.update_status('visualisation plot map done', 100)
        return response
