from flyingpigeon.utils import archiveextract
from flyingpigeon import robustness as erob
from tempfile import mkstemp
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.datafetch import write_fileinfo

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging

LOGGER = logging.getLogger("PYWPS")


class RobustnessProcess(Process):
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

            LiteralInput("method", "Method of robustness calculation",
                         abstract="Detailed information about the methods can be found in the documentation",
                         data_type='string',
                         default='signal_noise_ratio',
                         min_occurs=0,
                         max_occurs=1,
                         allowed_values=['signal_noise_ratio', 'Method_B', 'Method_C']
                         ),

            LiteralInput("start", "Start Year",
                         abstract="Beginn of the analysed period (e.g 19710101; if not set, the first consistend \
                                  year of the ensemble will be taken)",
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         # default='1971'
                         # allowedValues=range(1900,2200)
                         ),

            LiteralInput('end', "End Year",
                         abstract="End of the analysed period (e.g. 20501231 if not set, \
                                   the last consistend year of the ensemble will be taken)",
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         # default='2000',
                         ),

            LiteralInput("timeslice", "Time slice",
                         abstract="Time slice (in days) for robustness reference e.g. 3650",
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         # default='3560'
                         # allowedValues=range(1,50)
                         ),

            # self.variableIn = self.addLiteralInput(
            #   identifier="variable",
            #   title="Variable",
            #   abstract="Variable to be expected in the input files (Variable will be detected if not set, )",
            #   # default=None,
            #   type=type(''),
            #   minOccurs=0,
            #   maxOccurs=1,
            #   )

        ]

        outputs = [
            ComplexOutput('output_high', 'Mask for areas with high agreement',
                          abstract="netCDF file containing calculated robustness mask",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput('output_low', 'Mask for areas with low agreement',
                          abstract="netCDF file containing calculated robustness mask",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput('output_signal', 'Signal',
                          abstract="netCDF file containing calculated change of mean over the timeperiod and ensemble",
                          supported_formats=[Format('application/x-netcdf')],
                          as_reference=True,
                          ),

            ComplexOutput("output_graphic", "Graphic",
                          abstract="Graphic showing the signal difference with high and low ensemble agreement",
                          supported_formats=[Format("image/png")],
                          as_reference=True,
                          ),

            ComplexOutput("output_text", "Sourcefiles",
                          abstract="text file with a list of the used input data sets",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          )
        ]

        super(RobustnessProcess, self).__init__(
            self._handler,
            identifier="robustness",
            title="Ensemble robustness",
            version="0.5",
            metadata=[
                Metadata("LSCE", "http://www.lsce.ipsl.fr/")
            ],
            abstract="Calculates the robustness as the ratio of noise to signal in an ensemle of timeseries",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True
        )

    def _handler(self, request, response):
        response.update_status('starting uncertainty process', 0)

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        try:
            ncfiles = archiveextract(resource=rename_complexinputs(request.inputs['resource']))
            method = request.inputs['method'][0].data

            if 'start' in request.inputs:
                start = request.inputs['start'][0].data
            else:
                start = None

            if 'end' in request.inputs:
                end = request.inputs['end'][0].data
            else:
                end = None

            if 'timeslice' in request.inputs:
                timeslice = request.inputs['timeslice'][0].data
            else:
                timeslice = None

            response.update_status('arguments read', 5)
            LOGGER.info('Successfully read in the arguments')
        except:
            LOGGER.exception("failed to read in the arguments")
            raise

        response.outputs['output_text'].file = write_fileinfo(ncfiles)

        #  LOGGER.debug('variable set to %s' % variable)
        # if method == 'signal_noise_ratio':

        signal, low_agreement_mask, high_agreement_mask, text_src = erob.signal_noise_ratio(
            resource=ncfiles,
            start=start, end=end,
            timeslice=timeslice,
            # variable=variable
        )  # graphic,

        LOGGER.debug('Robustness calculated')

        try:
            # LOGGER.info('variable to be plotted: %s' % variable)
            from flyingpigeon.visualisation import map_robustness

            # if title is None:
            title = 'signal robustness'  # , end1, end2, start1, start2

            graphic = map_robustness(signal,
                                     high_agreement_mask,
                                     low_agreement_mask,
                                     # variable=variable,
                                     # cmap=cmap,
                                     title=title)

            LOGGER.info('graphic generated')
        except:
            msg = 'graphic generation failed'
            LOGGER.exception(msg)
            _, graphic = mkstemp(dir='.', suffix='.png')

        response.update_status('process worker done', 95)

        response.outputs['output_signal'].file = signal
        response.outputs['output_high'].file = high_agreement_mask
        response.outputs['output_low'].file = low_agreement_mask
        response.outputs['output_graphic'].file = graphic

        response.update_status('uncertainty process done', 100)
        return response
