import logging

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from flyingpigeon import plt_ncdata
from flyingpigeon.utils import extract_archive
from flyingpigeon.nc_utils import get_variable
# from flyingpigeon.utils import rename_complexinputs
# from flyingpigeon.log import init_process_logger

LOGGER = logging.getLogger("PYWPS")


class PlotuncertaintyRcpProcess(Process):
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

            LiteralInput('delta', 'Delta',
                         abstract='To set an offset for the values.'
                                  'e.g. -273.15 to transform Kelvin to Celsius',
                         data_type='float',
                         default=0,
                         ),

            LiteralInput('ymin', 'ymin',
                         abstract='Minimum limit of colorbar.',
                         data_type='float',
                         default=None,
                         min_occurs=0,
                         max_occurs=1,),

            LiteralInput('ymax', 'ymax',
                         abstract='Maximum limit of colorbar.',
                         data_type='float',
                         default=None,
                         min_occurs=0,
                         max_occurs=1,),


            LiteralInput('figsize', 'Figure Size',   data_type='string',
                         abstract='Give two numbers (as a string:"7,10") to define the size of the graphic',
                         default='10,10',
                         ),

        ]

        outputs = [
            # ComplexOutput('output_log', 'Logging information',
            #               abstract="Collected logs during process run.",
            #               as_reference=True,
            #               supported_formats=[Format('text/plain')]
            #               ),
            #
            # ComplexOutput("plotout_spaghetti", "Visualisation, Spaghetti plot",
            #               abstract="Visualisation of single variables as a spaghetti plot",
            #               supported_formats=[Format("image/png")],
            #               as_reference=True,
            #               ),

            ComplexOutput("plotout_uncertainty", "Visualisation Uncertainty plot",
                          abstract="Visualisation of single variables ensemble running mean with uncertainty",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          )
            # TODO: include window for running mean
        ]

        super(PlotuncertaintyRcpProcess, self).__init__(
            self._handler,
            identifier="plot_uncertaintyrcp",
            title="Timeseries as uncertainty plot with Rcp differenciated",
            version="0.1",
            metadata=[
                Metadata('Doc',
                         'https://flyingpigeon.readthedocs.io/en/latest/processes_des.html#data-visualization'),
            ],
            abstract="Outputs timeseries of all inputfiles with the ensemble running mean and correponding bandwidth."
                     "medians are caluclated seperately for each RCP, the uncertainty over all files."
                     "The single lines are colorcoded according to the IPCC graphic guidelines.",
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
        if 'delta' in request.inputs:
            delta = request.inputs['delta'][0].data
        else:
            delta = 0

        if 'title' in request.inputs:
            title = request.inputs['title'][0].data
        else:
            title = None

        if 'ymin' in request.inputs:
            ymin = request.inputs['ymin'][0].data
        else:
            ymin = None

        if 'ymax' in request.inputs:
            ymax = request.inputs['ymax'][0].data
        else:
            ymax = None

        f_str = request.inputs['figsize'][0].data
        figsize = tuple([float(f) for f in f_str.split(',')])
        response.update_status('plotting variable {}'.format(var), 10)

        try:
            plotout_file = plt_ncdata.plot_ts_uncertaintyrcp(ncfiles,
                                                             variable=var,
                                                             title=title,
                                                             delta=delta,
                                                             figsize=figsize,
                                                             ylim=(ymin, ymax),
                                                             dir_output=self.workdir)
            LOGGER.info("uncertainty plot done")
            response.update_status('Uncertainty plot for %s %s files done' % (len(ncfiles), var), 50)
            response.outputs['plotout_uncertainty'].file = plotout_file
        except Exception as e:
            raise Exception("Uncertainty plot failed : {}".format(e))

        response.update_status('visualisation done', 100)
        return response
