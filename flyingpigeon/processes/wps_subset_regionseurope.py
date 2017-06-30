from flyingpigeon.subset import clipping
# from flyingpigeon.subset import countries, countries_longname
from flyingpigeon.subset import _EUREGIONS_
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class ClipregionseuropeProcess(Process):
    """
    TODO: opendap input support, additional metadata to display region names.
    """
    def __init__(self):
        inputs = [
            LiteralInput('region', 'Region',
                         data_type='string',
                         abstract="European region code, see ISO-3166 Alpha2: https://en.wikipedia.org/wiki/ISO_3166-2 ",  # noqa
                         min_occurs=1,
                         max_occurs=len(_EUREGIONS_),
                         default='DEU',
                         allowed_values=_EUREGIONS_),

            LiteralInput('mosaic', 'Union of multiple regions',
                         data_type='boolean',
                         abstract="If True, selected regions will be merged"
                                  " into a single geometry.",
                         min_occurs=0,
                         max_occurs=1,
                         default=False),

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
        ]

        outputs = [
            ComplexOutput('output', 'Tar archive',
                          abstract="Tar archive of the subsetted netCDF files.",
                          as_reference=True,
                          supported_formats=[Format('application/x-tar')]
                          ),

            ComplexOutput('ncout', 'Example netCDF file',
                          abstract="NetCDF file with subset for one dataset.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          )
        ]

        super(ClipregionseuropeProcess, self).__init__(
            self._handler,
            identifier="subset_regionseurope",
            title="Subset (European Regions)",
            version="0.10",
            abstract="Return the data whose grid cells inteserct the selected regions for each input dataset.",
            metadata=[
                Metadata('LSCE', 'http://www.lsce.ipsl.fr/en/index.php'),
                Metadata('Documentation', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        # input files
        LOGGER.debug("url=%s, mime_type=%s",
                     request.inputs['resource'][0].url,
                     request.inputs['resource'][0].data_format.mime_type)
        ncs = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
            # mime_type=request.inputs['resource'][0].data_format.mime_type)
        # mosaic option
        # TODO: fix defaults in pywps 4.x
        if 'mosaic' in request.inputs:
            mosaic = request.inputs['mosaic'][0].data
        else:
            mosaic = False
        # regions used for subsetting
        regions = [inp.data for inp in request.inputs['region']]

        LOGGER.info('ncs = %s', ncs)
        LOGGER.info('regions = %s', regions)
        LOGGER.info('mosaic = %s', mosaic)

        response.update_status("Arguments set for subset process", 0)
        LOGGER.debug('starting: regions=%s, num_files=%s', len(regions), len(ncs))

        try:
            results = clipping(
                resource=ncs,
                polygons=regions,  # self.region.getValue(),
                mosaic=mosaic,
                spatial_wrapping='wrap',
                # variable=variable,
                # dir_output=os.path.abspath(os.curdir),
                # dimension_map=dimension_map,
            )
            LOGGER.info('results %s' % results)
        except:
            msg = 'clipping failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        if not results:
            raise Exception('no results produced.')

        # prepare tar file
        try:
            tarf = archive(results)
            LOGGER.info('Tar file prepared')
        except:
            msg = 'Tar file preparation failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output'].file = tarf

        i = next((i for i, x in enumerate(results) if x), None)
        response.outputs['ncout'].file = results[i]

        response.update_status("done", 100)
        return response
#
#
# import os
# import tarfile
#
# from flyingpigeon.subset import clipping
# from flyingpigeon.subset import _EUREGIONS_  # countries, countries_longname
# from flyingpigeon.log import init_process_logger
#
# from pywps.Process import WPSProcess
#
# import logging
# logger = logging.getLogger(__name__)
# europeanregions = _EUREGIONS_.keys()
# europeanregions.sort()
#
#
# class subset_regionseuropeProcess(WPSProcess):
#     def __init__(self):
#         WPSProcess.__init__(
#             self,
#             identifier="subset_regionseurope",
#             title="Subset European Regions",
#             version="0.9",
#             abstract="Returns the selected European administrative region defined in the GADM database (v2.5)\
#              for each input dataset.",
#             statusSupported=True,
#             storeSupported=True
#             )
#
#         self.resource = self.addComplexInput(
#             identifier="resource",
#             title="Resource",
#             abstract="NetCDF File",
#             minOccurs=1,
#             maxOccurs=1000,
#             maxmegabites=5000,
#             formats=[{"mimeType": "application/x-netcdf"}],
#             )
#
#         self.region = self.addLiteralInput(
#             identifier="region",
#             title="Region",
#             # abstract= countries_longname(), # need to handle special non-ascii char in countries.
#             default='DE.MV',
#             type=type(''),
#             minOccurs=1,
#             maxOccurs=len(europeanregions),
#             allowedValues=europeanregions
#             )
#
#         self.mosaic = self.addLiteralInput(
#             identifier="mosaic",
#             title="Mosaic",
#             abstract="If Mosaic is checked, selected polygons will be clipped as a mosaic for each input file.",
#             default=False,
#             type=type(False),
#             minOccurs=0,
#             maxOccurs=1,
#             )
#
#         # self.dimension_map = self.addLiteralInput(
#         #     identifier="dimension_map",
#         #     title="Dimension Map",
#         #     abstract= 'If not ordered in lon/lat, a dimension map has to be provided.',
#         #     type=type(''),
#         #     minOccurs=0,
#         #     maxOccurs=1
#         #     )
#
#         # self.variable = self.addLiteralInput(
#         #     identifier="variable",
#         #     title="Variable",
#         #     abstract="Variable to be expected in the input files (variable will be detected if not set).",
#         #     default=None,
#         #     type=type(''),
#         #     minOccurs=0,
#         #     maxOccurs=1,
#         #     )
#
#         self.output = self.addComplexOutput(
#             title="Subsets",
#             abstract="Tar archive containing the netCDF files",
#             formats=[{"mimeType": "application/x-tar"}],
#             asReference=True,
#             identifier="output",
#             )
#
#         self.output_log = self.addComplexOutput(
#             identifier="output_log",
#             title="Logging information",
#             abstract="Collected logs during process run.",
#             formats=[{"mimeType": "text/plain"}],
#             asReference=True,
#             )
#
#     def execute(self):
#         from ast import literal_eval
#         from flyingpigeon.utils import archive, archiveextract
#
#         init_process_logger('log.txt')
#         self.output_log.setValue('log.txt')
#
#         ncs = archiveextract(self.getInputValues(identifier='resource'))
#         mosaic = self.mosaic.getValue()
#         regions = self.region.getValue()
#         # variable = self.variable.getValue()
#
#         # logger.info('regions: %s' % regions)
#
#         # dimension_map = self.dimension_map.getValue()
#         # if dimension_map != None:
#         #     dimension_map = literal_eval(dimension_map)
#
#         logger.info('ncs = %s', ncs)
#         logger.info('regions = %s', regions)
#         logger.info('mosaic = %s', mosaic)
#         # logger.info('dimension_map = %s', dimension_map)
#
#         self.status.set('Arguments set for subset process', 0)
#
#         logger.debug('starting: regions=%s, num_files=%s' % (len(regions), len(ncs)))
#
#         try:
#             results = clipping(
#                 resource=ncs,
#                 polygons=regions,  # self.region.getValue(),
#                 mosaic=mosaic,
#                 # variable=variable,
#                 dir_output=os.path.abspath(os.curdir),
#                 #  dimension_map=dimension_map,
#                 )
#
#         except Exception as e:
#             logger.exception('clipping failed')
#             self.status.set('clipping failed')
#         # prepare tar file
#         try:
#             tarf = archive(results)
#             logger.info('Tar file prepared')
#         except Exception as e:
#             logger.exception('Tar file preparation failed')
#             raise
#
#         self.output.setValue(tarf)
#         self.status.set('done', 100)
