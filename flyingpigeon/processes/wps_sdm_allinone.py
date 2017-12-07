"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.log import init_process_logger
from flyingpigeon import sdm
from flyingpigeon.sdm import _SDMINDICES_
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.visualisation import map_gbifoccurrences
from flyingpigeon.visualisation import map_PAmask
from flyingpigeon.visualisation import pdfmerge, concat_images

import tempfile
import logging
LOGGER = logging.getLogger("PYWPS")


class SDMallinoneProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput('taxon_name', 'Taxonomic name of tree species',
                         abstract='Taxonomic name of tree species (e. g. Fagus sylvatica)',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         default='Fagus sylvatica'
                         ),


            #  self.BBox = self.addBBoxInput(
            #     identifier="BBox",
            #     title="Bounding Box",
            #     abstract="coordinates to define the region for occurence data fetch",
            #     minOccurs=1,
            #     maxOccurs=1,
            #     crss=['EPSG:4326']
            #     )

            LiteralInput("indices", "Indices",
                         abstract="Climate indices related to growth conditions \
                                    of tree species",
                         default='TG_JJA',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=10,
                         allowed_values=_SDMINDICES_
                         ),

            LiteralInput("period", "Reference period",
                         abstract="Reference period for climate conditions\
                         (all = entire timeseries)",
                         default="all",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['all', '1951-1980', '1961-1990',
                                         '1971-2000', '1981-2010']
                         ),


            LiteralInput("archive_format", "Archive format",
                         abstract="Result files will be compressed into archives.\
                                  Choose an appropriate format",
                         default="tar",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['zip', 'tar']
                         )
        ]

        outputs = [
            ComplexOutput('output_csv', 'Tree species table',
                          abstract="Extracted CSV file containing the tree species table",
                          as_reference=True,
                          supported_formats=[Format('text/csv')]
                          ),

            ComplexOutput("output_gbif", "Graphic of GBIF coordinates",
                          abstract="PNG graphic file showing the presence of tree species\
                                    according to the CSV file",
                          supported_formats=[Format('image/png')],
                          as_reference=True,
                          ),

            ComplexOutput("output_PA", "Graphic of PA mask",
                          abstract="PNG graphic file showing PA mask generated based on\
                                    netCDF spatial increment",
                          supported_formats=[Format('image/png')],
                          as_reference=True,
                          ),

            ComplexOutput("output_indices", "Climate indices for growth conditions over all timesteps",
                          abstract="Archive (tar/zip) containing calculated climate indices",
                          supported_formats=[Format('application/x-tar'),
                                             Format('application/zip')
                                             ],
                          as_reference=True,
                          ),

            ComplexOutput("output_reference", "Climate indices for growth conditions of reference period",
                          abstract="Archive (tar/zip) containing calculated climate indices",
                          supported_formats=[Format('application/x-tar'),
                                             Format('application/zip')
                                             ],
                          as_reference=True,
                          ),

            ComplexOutput("output_prediction", "predicted growth conditions",
                          abstract="Archive containing files of the predicted\
                                     growth conditions",
                          supported_formats=[Format('application/x-tar'),
                                             Format('application/zip')
                                             ],
                          as_reference=True,
                          ),

            ComplexOutput("output_info", "GAM statistics information",
                          abstract="Graphics and information of the learning statistics",
                          supported_formats=[Format("application/pdf")],
                          as_reference=True,
                          ),
            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          )
        ]

        super(SDMallinoneProcess, self).__init__(
            self._handler,
            identifier="sdm_allinone",
            title="Species distribution Model (all in one)",
            version="0.10",
            metadata=[
                Metadata("LWF", "http://www.lwf.bayern.de/"),
                Metadata(
                    "Doc",
                    "http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#species-distribution-model"),
                Metadata("paper",
                         "http://www.hindawi.com/journals/jcli/2013/787250/"),
                Metadata("Tutorial",
                         "http://flyingpigeon.readthedocs.io/en/latest/tutorials/sdm.html"),
            ],
            abstract="Indices preparation for SDM process",
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        response.update_status('Start process', 0)

        try:
            LOGGER.info('reading the arguments')
            resources = archiveextract(
                resource=rename_complexinputs(request.inputs['resource']))
            taxon_name = request.inputs['taxon_name'][0].data
            bbox = [-180, -90, 180, 90]
            # bbox_obj = self.BBox.getValue()
            # bbox = [bbox_obj.coords[0][0],
            #         bbox_obj.coords[0][1],
            #         bbox_obj.coords[1][0],
            #         bbox_obj.coords[1][1]]
            period = request.inputs['period']
            period = period[0].data
            indices = [inpt.data for inpt in request.inputs['indices']]
            archive_format = request.inputs['archive_format'][0].data
            LOGGER.exception("indices = %s for %s ", indices, taxon_name)
            LOGGER.info("bbox={0}".format(bbox))
        except:
            LOGGER.exception('failed to read in the arguments')
        LOGGER.info('indices %s ' % indices)

        try:
            response.update_status('Fetching GBIF Data', 10)
            gbifdic = sdm.get_gbif(taxon_name, bbox=bbox)
            LOGGER.info('Fetched GBIF data')
        except:
            msg = 'failed to search gbif.'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('write csv file', 70)
            gbifcsv = sdm.gbifdic2csv(gbifdic)
            LOGGER.info('GBIF data written to file')
        except:
            msg = 'failed to write csv file.'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('plot map', 80)
            latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except:
            msg = 'failed to plot occurence map.'
            LOGGER.exception(msg)
            raise Exception(msg)

        #################################
        # calculate the climate indices
        #################################

        # get the indices
        ncs_indices = None
        try:
            response.update_status('start calculation of climate indices for %s'
                            % indices, 30)
            ncs_indices = sdm.get_indices(resource=resources, indices=indices)
            LOGGER.info('indice calculation done')
        except:
            msg = 'failed to calculate indices'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            # sort indices
            indices_dic = sdm.sort_indices(ncs_indices)
            LOGGER.info('indice files sorted in dictionary')
        except:
            msg = 'failed to sort indices'
            LOGGER.exception(msg)
            indices_dic = {'dummy': []}

        ncs_references = []
        species_files = []
        stat_infos = []
        PAmask_pngs = []

        response.update_status('Start processing for %s Datasets' % len(indices_dic.keys()))

        for count, key in enumerate(indices_dic.keys()):
            try:
                staus_nr = 40 + count*10
                response.update_status('Start processing of %s' % key, staus_nr)
                ncs = indices_dic[key]
                LOGGER.info('with %s files' % len(ncs))

                try:
                    response.update_status('generating the PA mask', 20)
                    PAmask = sdm.get_PAmask(coordinates=latlon, nc=ncs[0])
                    LOGGER.info('PA mask sucessfully generated')
                except:
                    LOGGER.exception('failed to generate the PA mask')

                try:
                    response.update_status('Ploting PA mask', 25)
                    PAmask_pngs.extend([map_PAmask(PAmask)])
                except:
                    LOGGER.exception('failed to plot the PA mask')

                try:
                    ncs_reference = sdm.get_reference(ncs_indices=ncs, period=period)
                    ncs_references.extend(ncs_reference)
                    LOGGER.info('reference indice calculated %s '
                                % ncs_references)
                except:
                    msg = 'failed to calculate the reference'
                    LOGGER.exception(msg)

                try:
                    gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask)
                    stat_infos.append(gam_info)
                    response.update_status('GAM sucessfully trained', staus_nr + 5)
                except:
                    msg = 'failed to train GAM for %s' % (key)
                    LOGGER.exception(msg)

                try:
                    prediction = sdm.get_prediction(gam_model, ncs)
                    response.update_status('prediction done', staus_nr + 7)
                except:
                    msg = 'failed to predict tree occurence'
                    LOGGER.exception(msg)
                    # raise Exception(msg)

                # try:
                #     response.update_status('land sea mask for predicted data',  staus_nr + 8)
                #     from numpy import invert, isnan, nan, broadcast_arrays  # , array, zeros, linspace, meshgrid
                #     mask = invert(isnan(PAmask))
                #     mask = broadcast_arrays(prediction, mask)[1]
                #     prediction[mask is False] = nan
                # except:
                #     LOGGER.exception('failed to mask predicted data')

                try:
                    species_files.append(sdm.write_to_file(ncs[0], prediction))
                    LOGGER.info('Favourabillity written to file')
                except:
                    msg = 'failed to write species file'
                    LOGGER.exception(msg)
                    # raise Exception(msg)

            except:
                msg = 'failed to calculate reference indices'
                LOGGER.exception(msg)
                raise Exception(msg)

        try:
            archive_indices = archive(ncs_indices, format=archive_format)
            LOGGER.info('indices added to archive')
        except:
            msg = 'failed adding indices to archive'
            LOGGER.exception(msg)
            archive_indices = tempfile.mkstemp(suffix='.tar', prefix='foobar-', dir='.')

        try:
            archive_references = archive(ncs_references, format=archive_format)
            LOGGER.info('indices reference added to archive')
        except:
            msg = 'failed adding reference indices to archive'
            LOGGER.exception(msg)
            archive_references = tempfile.mkstemp(suffix='.tar', prefix='foobar-', dir='.')

        try:
            archive_prediction = archive(species_files, format=archive_format)
            LOGGER.info('species_files added to archive')
        except:
            msg = 'failed adding species_files indices to archive'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            stat_infosconcat = pdfmerge(stat_infos)
            LOGGER.debug('pngs %s' % PAmask_pngs)
            PAmask_png = concat_images(PAmask_pngs, orientation='h')
            LOGGER.info('stat infos pdfs and mask pngs merged')
        except:
            LOGGER.exception('failed to concat images')
            _, stat_infosconcat = tempfile.mkstemp(suffix='.pdf', prefix='foobar-', dir='.')
            _, PAmask_png = tempfile.mkstemp(suffix='.png', prefix='foobar-', dir='.')

        response.outputs['output_gbif'].file = occurence_map
        response.outputs['output_PA'].file = PAmask_png
        response.outputs['output_indices'].file = archive_indices
        response.outputs['output_reference'].file = archive_references
        response.outputs['output_prediction'].file = archive_prediction
        response.outputs['output_info'].file = stat_infosconcat
        response.outputs['output_csv'].file = gbifcsv

        response.update_status('done', 100)
        return response
