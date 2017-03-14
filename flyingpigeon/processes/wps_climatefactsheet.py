import os
import tarfile

from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class FactsheetProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract="NetCDF Files or archive (tar/zip) containing netCDF files",
                         min_occurs=1,
                         max_occurs=1000,
                         #  maxmegabites=5000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput("region", "Region",
                         # abstract= countries_longname(), # need to handle special non-ascii char in countries.
                         default='DEU',
                         data_type='string',
                         min_occurs=1,
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
            version="0.2",
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
        from flyingpigeon.utils import archive, archiveextract
        from tempfile import mkstemp

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        ncs = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))

        # mosaic = self.mosaic.getValue()
        regions = [inp.data for inp in request.inputs['region']]

        response.update_status('Arguments set for subset process', 0)
        LOGGER.debug('starting: regions=%s, num_files=%s' % (len(regions), len(ncs)))

        try:
            from flyingpigeon.visualisation import plot_polygons
            png_country = plot_polygons(regions)
        except:
            LOGGER.exception('failed to plot the polygon to world map')
            o1, png_country = mkstemp(dir='.', suffix='.png')

        # clip the demanded polygons
        from flyingpigeon.subset import clipping
        subsets = clipping(resource=ncs, variable=None,
                           dimension_map=None,
                           calc=None,
                           output_format='nc',
                           calc_grouping=None,
                           time_range=None,
                           time_region=None,
                           historical_concatination=True,
                           prefix=None,
                           spatial_wrapping='wrap',
                           polygons=regions,
                           mosaic=True
                           )

        try:
            tar_subsets = archive(subsets)
        except:
            LOGGER.exception('failed to archive subsets')
            _, tar_subsets = mkstemp(dir='.', suffix='.tar')

        try:
            from flyingpigeon.visualisation import uncertainty
            png_uncertainty = uncertainty(subsets)
        except:
            LOGGER.exception('failed to generate the uncertainty plot')
            _, png_uncertainty = mkstemp(dir='.', suffix='.png')

        try:
            from flyingpigeon.visualisation import spaghetti
            png_spaghetti = spaghetti(subsets)
        except:
            LOGGER.exception('failed to generate the spaghetti plot')
            _, png_spaghetti = mkstemp(dir='.', suffix='.png')

        try:
            from flyingpigeon import robustness as erob
            from flyingpigeon.utils import get_variable
            variable = get_variable(ncs[0])

            signal, low_agreement_mask, high_agreement_mask, text_src = erob.method_A(resource=subsets,
                                                                                      # start=None, end=None,
                                                                                      # timeslice=None,
                                                                                      variable=variable
                                                                                      )
            LOGGER.info('variable to be plotted: %s' % variable)
            from flyingpigeon.visualisation import map_robustness
            # if title is None:
            title = 'signal robustness of %s ' % (variable)  # , end1, end2, start1, start2
            png_robustness = map_robustness(signal,
                                            high_agreement_mask,
                                            low_agreement_mask,
                                            # cmap=cmap,
                                            title=title)
            LOGGER.info('graphic generated')

        except:
            LOGGER.exception('failed to generate the robustness plot')
            _, png_robustness = mkstemp(dir='.', suffix='.png')

        from flyingpigeon.visualisation import factsheetbrewer
        factsheet = factsheetbrewer(png_country=png_country,
                                    png_uncertainty=png_uncertainty,
                                    png_spaghetti=png_spaghetti,
                                    png_robustness=png_robustness)

        response.outputs['output_nc'].file = tar_subsets
        response.outputs['output_factsheet'].file = factsheet
        response.update_status("done", 100)
        return response
