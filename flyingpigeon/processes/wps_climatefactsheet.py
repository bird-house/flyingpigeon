from os.path import abspath
from tempfile import mkstemp

from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.subset import clipping
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import get_variable
from flyingpigeon import visualisation as vs

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class FactsheetProcess(Process):
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

            LiteralInput("region", "Region",
                         # abstract= countries_longname(), # need to handle special non-ascii char in countries.
                         data_type='string',
                         min_occurs=0,
                         max_occurs=len(countries()),
                         allowed_values=countries()),  # REGION_EUROPE #COUNTRIES
        ]

        ###########
        # OUTPUTS
        ###########
        outputs = [
            ComplexOutput('output_nc', "Subsets",
                          abstract="Tar archive containing the netCDF files",
                          as_reference=True,
                          supported_formats=[Format("application/x-tar")],
                          ),

            ComplexOutput('output_factsheet', "Climate Fact Sheet",
                          abstract="Short overview of the climatological situation of the selected countries",
                          as_reference=True,
                          supported_formats=[Format('application/pdf')],
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")]),
        ]

        super(FactsheetProcess, self).__init__(
            self._handler,
            identifier="climatefactsheet",
            title="Climate Fact Sheet Generator",
            version="0.3",
            abstract="Returns a pdf with a short overview of the climatological situation for the selected countries",
            metadata=[
                # {"title": "LSCE", "href": "http://www.lsce.ipsl.fr/en/index.php"},
                {"title": "Doc", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True
        )

    def _handler(self, request, response):

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        ncs = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))

        var = get_variable(ncs[0])
        LOGGER.info('variable to be plotted: %s' % var)

        # mosaic = self.mosaic.getValue()
        if 'region' in request.inputs:
            regions = [inp.data for inp in request.inputs['region']]
            try:
                png_region = vs.plot_polygons(regions)
            except:
                LOGGER.exception('failed to plot the polygon to world map')
                o1, png_region = mkstemp(dir='.', suffix='.png')

            # clip the demanded polygons
            subsets = clipping(
                resource=ncs,
                variable=var,
                polygons=regions,
                mosaic=True,
                spatial_wrapping='wrap',
                )
        else:
            subsets = ncs
            png_region = vs.plot_extend(ncs[0])

        response.update_status('Arguments set for subset process', 0)

        try:
            tar_subsets = archive(subsets)
        except:
            LOGGER.exception('failed to archive subsets')
            _, tar_subsets = mkstemp(dir='.', suffix='.tar')

        try:
            png_uncertainty = vs.uncertainty(subsets, variable=var)
        except:
            LOGGER.exception('failed to generate the uncertainty plot')
            _, png_uncertainty = mkstemp(dir='.', suffix='.png')

        try:
            png_spaghetti = vs.spaghetti(subsets, variable=var,)
        except:
            LOGGER.exception('failed to generate the spaghetti plot')
            _, png_spaghetti = mkstemp(dir='.', suffix='.png')

        try:
            from flyingpigeon import robustness as ro
            signal, low_agreement_mask, high_agreement_mask, text_src = ro.signal_noise_ratio(resource=subsets,
                                                                                              # start=None, end=None,
                                                                                              # timeslice=None,
                                                                                              # variable=var
                                                                                              )
            # if title is None:
            title = 'signal robustness of %s ' % (var)  # , end1, end2, start1, start2
            png_robustness = vs.map_robustness(signal,
                                               high_agreement_mask,
                                               low_agreement_mask,
                                               # cmap=cmap,
                                               #    title=title
                                               )
            LOGGER.info('robustness graphic generated')
        except:
            LOGGER.exception('failed to generate the robustness plot')
            _, png_robustness = mkstemp(dir='.', suffix='.png')

        factsheet = vs.factsheetbrewer(
            png_region=png_region,
            png_uncertainty=png_uncertainty,
            png_spaghetti=png_spaghetti,
            png_robustness=png_robustness
            )

        response.outputs['output_nc'].file = tar_subsets
        response.outputs['output_factsheet'].file = factsheet
        response.update_status("done", 100)
        return response
