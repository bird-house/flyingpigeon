from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from flyingpigeon.indices import indices, indices_description
from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.indices import calc_indice_simple
from flyingpigeon.utils import GROUPING
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon import config
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class IndicessingleProcess(Process):
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

            LiteralInput("indices", "Index",
                         abstract='Select an index',
                         default='TG',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=indices()
                         ),

            LiteralInput("groupings", "Grouping",
                         abstract="Select an time grouping (time aggregation)",
                         default='yr',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=len(GROUPING),
                         allowed_values=GROUPING
                         ),

            LiteralInput('region', 'Region',
                         data_type='string',
                         # abstract= countries_longname(), # need to handle special non-ascii char in countries.
                         abstract="Country ISO-3166-3:\
                          https://en.wikipedia.org/wiki/ISO_3166-1_alpha-3#Officially_assigned_code_elements",
                         min_occurs=1,
                         max_occurs=len(countries()),
                         default='DEU',
                         allowed_values=countries()),  # REGION_EUROPE #COUNTRIES

            LiteralInput("mosaic", "Mosaic",
                         abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file",
                         default='0',
                         data_type='boolean',
                         min_occurs=0,
                         max_occurs=1,
                         ),

        ]

        outputs = [
            ComplexOutput("output_archive", "Masked Files Archive",
                          abstract="Tar file of the masked netCDF files",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            # ComplexOutput("output_example", "Example",
            #               abstract="one example file to display in the WMS",
            #               supported_formats=[Format("application/x-netcdf")],
            #               as_reference=True,
            #               ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")])
        ]

        super(IndicessingleProcess, self).__init__(
            self._handler,
            identifier="indices_single",
            title="Climate indices (Single variable)",
            version="0.10",
            abstract="Climate indices based on one single input variable",
            metadata=[
                {'title': 'Doc',
                 'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/\
                 index.html#climate-indices'},
                {"title": "ICCLIM",
                 "href": "http://icclim.readthedocs.io/en/latest/"},
                {"title": "Percentile-based indices", "href": "http://flyingpigeon.readthedocs.io/en/\
                latest/descriptions/indices.html#percentile-based-indices"},
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        resources = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        indices = request.inputs['indices'][0].data
        region = request.inputs['region'][0].data
        groupings = [inpt.data for inpt in request.inputs['groupings']]
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False

        response.update_status('starting: indices=%s, refperiod=%s, groupings=%s, num_files=%s'
                               % (indices, refperiod, groupings, len(resources)), 2)

        results = calc_indice_simple(
            resource=ncs,
            mosaic=mosaic,
            indices=indices,
            polygons=polygons,
            groupings=groupings,
            # dir_output=path.curdir,
            )

#         # if not results:
#         #     raise Exception("failed to produce results")
#         # response.update_status('num results %s' % len(results), 90)

        tarf = archive(results)

        response.outputs['output_archive'].file = tarf
#         # response.update_status('done: indice=%s, num_files=%s' % (indices, len(resources)), 100)
        response.update_status("done", 100)
        return response

    #
    # """
    # This process calculates climate indices for the given input datasets.
    # """
    # def __init__(self):
    #     WPSProcess.__init__(
    #         self,
    #         identifier="indices_simple",
    #         title="Climate indices -- Simple",
    #         version="0.9",
    #         abstract="Climate indices based on one single input variable.",
    #         metadata=[
    #             {'title': 'Documentation',
    #              'href': 'http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#climate-indices'},
    #             {"title": "ICCLIM",
    #              "href": "http://icclim.readthedocs.io/en/latest/"},
    #             {"title": "Simple Indices",
    #              "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/indices.html"}
    #             ],
    #         statusSupported=True,
    #         storeSupported=True
    #         )
    #
    #     self.resource = self.addComplexInput(
    #         identifier="resource",
    #         title="Resouce",
    #         abstract="NetCDF File",
    #         minOccurs=1,
    #         maxOccurs=100,
    #         maxmegabites=5000,
    #         formats=[{"mimeType": "application/x-netcdf"}],
    #         )
    #
    #     self.groupings = self.addLiteralInput(
    #         identifier="groupings",
    #         title="Grouping",
    #         abstract="Select an time grouping (time aggregation)",
    #         default='yr',
    #         type=type(''),
    #         minOccurs=1,
    #         maxOccurs=len(GROUPING),
    #         allowedValues=GROUPING
    #         )
    #
    #     self.indices = self.addLiteralInput(
    #         identifier="indices",
    #         title="Index",
    #         abstract=indices_description(),
    #         default='SU',
    #         type=type(''),
    #         minOccurs=1,
    #         maxOccurs=len(indices()),
    #         allowedValues=indices()
    #         )
    #
    #     self.polygons = self.addLiteralInput(
    #         identifier="polygons",
    #         title="Country subset",
    #         abstract=str(countries_longname()),
    #         type=type(''),
    #         minOccurs=0,
    #         maxOccurs=len(countries()),
    #         allowedValues=countries()
    #         )
    #
    #     self.mosaic = self.addLiteralInput(
    #         identifier="mosaic",
    #         title="Mosaic",
    #         abstract="If Mosaic is checked, selected polygons be clipped as a mosaic for each input file",
    #         default=False,
    #         type=type(False),
    #         minOccurs=0,
    #         maxOccurs=1,
    #         )
    #
    #     # complex output
    #     # -------------
    #     self.output = self.addComplexOutput(
    #         identifier="output",
    #         title="Index",
    #         abstract="Calculated index as NetCDF file",
    #         metadata=[],
    #         formats=[{"mimeType": "application/x-tar"}],
    #         asReference=True
    #         )
    #
    #     self.output_netcdf = self.addComplexOutput(
    #         title="one dataset as example",
    #         abstract="NetCDF file to be dispayed on WMS",
    #         formats=[{"mimeType": "application/x-netcdf"}],
    #         asReference=True,
    #         identifier="ncout",
    #         )
    #
    #     self.output_log = self.addComplexOutput(
    #         identifier="output_log",
    #         title="Logging information",
    #         abstract="Collected logs during process run.",
    #         formats=[{"mimeType": "text/plain"}],
    #         asReference=True,
    #         )
    #
    # def execute(self):
    #     import os
    #     from flyingpigeon.utils import archive
    #     # import tarfile
    #     from tempfile import mkstemp
    #     from os import path
    #     from numpy import squeeze
    #
    #     init_process_logger('log.txt')
    #     self.output_log.setValue('log.txt')
    #
    #     ncs = self.getInputValues(identifier='resource')
    #     indices = self.indices.getValue()
    #     polygons = self.polygons.getValue()
    #     mosaic = self.mosaic.getValue()
    #     groupings = self.groupings.getValue()
    #
    #     if polygons is None:
    #         self.status.set('No countries selected, entire domain will be calculated', 10)
    #     logger.debug('indices=%s', indices)
    #     logger.debug('groupings=%s', groupings)
    #     logger.debug('num files=%s', len(ncs))
    #     self.status.set('processing indices : %s' % indices, 12)
    #
    #     results = squeeze(calc_indice_simple(
    #         resource=ncs,
    #         mosaic=mosaic,
    #         indices=indices,
    #         polygons=polygons,
    #         groupings=groupings,
    #         dir_output=path.curdir,
    #         ))
    #     results_list = results.tolist()
    #     self.status.set('indices calculated', 90)
    #     logger.debug('results type: %s', type(results_list))
    #     logger.debug('indices files: %s ' % results_list)
    #
    #     try:
    #         archive_indices = archive(results_list)
    #         logger.info('archive prepared')
    #     except Exception as e:
    #         msg = "archive preparation failed"
    #         logger.exception(msg)
    #         raise Exception(msg)
    #     try:
    #         self.output.setValue(archive_indices)
    #         if type(results_list) == list:
    #             i = next((i for i, x in enumerate(results.tolist()) if x), None)
    #             self.output_netcdf.setValue(str(results[i]))
    #         elif type(results_list) == str:
    #             self.output_netcdf.setValue(results_list)
    #         else:
    #             logger.debug('results_list type: %s  not extractable ' % type(results_list))
    #             self.output_netcdf.setValue(None)
    #     except Exception as e:
    #         msg = "extraction of example file failed"
    #         logger.exception(msg)
    #
    #     self.status.set('done', 100)
