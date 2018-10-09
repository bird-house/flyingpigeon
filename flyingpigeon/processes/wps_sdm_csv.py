"""
Processes for Species distribution
Author: Nils Hempelmann ( info@nilshempelmann.de )
"""
import logging
import tempfile
<<<<<<< HEAD

from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata
=======
>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41

from flyingpigeon import sdm
from flyingpigeon.log import init_process_logger
from flyingpigeon.sdm import _SDMINDICES_
from flyingpigeon.utils import archive, archiveextract, download
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.visualisation import map_PAmask
from flyingpigeon.visualisation import map_gbifoccurrences
from flyingpigeon.visualisation import pdfmerge, concat_images
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

<<<<<<< HEAD
from eggshell.log import init_process_logger

import tempfile
import logging

=======
>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41
LOGGER = logging.getLogger("PYWPS")


class SDMcsvProcess(Process):
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

            LiteralInput("gbif", "GBIF csv file",
                         abstract="GBIF table (csv) with tree occurence \
                         (output of 'GBIF data fetch' process )",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         #  default='http://localhost:8090/wpsoutputs/flyingpigeon/output_csv-abe15f64-c30d-11e6-bf63-142d277ef1f3.csv'
                         ),

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

        super(SDMcsvProcess, self).__init__(
            self._handler,
            identifier="sdm_csv",
            title="Species distribution Model (GBIF-CSV table as input)",
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
            period = request.inputs['period']
            period = period[0].data
            indices = [inpt.data for inpt in request.inputs['indices']]
            archive_format = request.inputs['archive_format'][0].data
            LOGGER.info("all arguments read in nr of files in resources: {}".foirmat(len(resources)))
        except Exception as ex:
            msg = 'failed to read in the arguments: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            gbif_url = request.inputs['gbif'][0].data
            csv_file = download(gbif_url)
            LOGGER.info('CSV file fetched sucessfully: %s' % csv_file)
        except Exception as ex:
            msg = 'failed to fetch GBIF file: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('read in latlon coordinates', 10)
            latlon = sdm.latlon_gbifcsv(csv_file)
            LOGGER.info('got occurence coordinates %s ' % csv_file)
        except Exception as ex:
            msg = 'failed to extract the latlon points from file {}: {}'.format(csv_file, str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('plot map', 20)
            occurence_map = map_gbifoccurrences(latlon)
            LOGGER.info('GBIF occourence ploted')
        except Exception as ex:
            msg = 'failed to plot occurence map: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        #################################
        # calculate the climate indices
        #################################

        # get the indices
        try:
            response.update_status('start calculation of indices', 30)
            ncs_indices = sdm.get_indices(resource=resources, indices=indices)
            LOGGER.info('indice calculation done')
        except Exception as ex:
            msg = 'failed to calculate indices: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            # sort indices
            indices_dic = sdm.sort_indices(ncs_indices)
            LOGGER.info('indice files sorted in dictionary')
        except Exception as ex:
            msg = 'failed to sort indices: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)
            indices_dic = {'dummy': []}

        ncs_references = []
        species_files = []
        stat_infos = []
        PAmask_pngs = []

        response.update_status('Start processing for {} datasets'.format(len(indices_dic.keys())))
        for count, key in enumerate(indices_dic.keys()):
            try:
<<<<<<< HEAD
                status_nr = 40 + count * 10
                response.update_status('Start processing of {}'.format(key), status_nr)
=======
                staus_nr = 40 + count * 10
                response.update_status('Start processing of %s' % key, staus_nr)
>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41
                ncs = indices_dic[key]
                LOGGER.info('with {} files'.format(len(ncs)))

                try:
                    response.update_status('generating the PA mask', 20)
                    PAmask = sdm.get_PAmask(coordinates=latlon, nc=ncs[0])
                    LOGGER.info('PA mask sucessfully generated')
                except Exception as ex:
                    msg = 'failed to generate the PA mask: {}'.format(str(ex))
                    LOGGER.exception(msg)
                    raise Exception(msg)

                try:
                    response.update_status('Ploting PA mask', 25)
                    PAmask_pngs.extend([map_PAmask(PAmask)])
                except Exception as ex:
                    msg = 'failed to plot the PA mask: {}'.format(str(ex))
                    LOGGER.exception(msg)
                    raise Exception(msg)

                try:
                    ncs_reference = sdm.get_reference(ncs_indices=ncs, period=period)
                    ncs_references.extend(ncs_reference)
                    LOGGER.info('reference indice calculated {}'.format(ncs_references))
                except Exception as ex:
                    msg = 'failed to calculate the reference: {}'.format(str(ex))
                    LOGGER.exception(msg)
                    raise Exception(msg)

                try:
                    gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask)
                    stat_infos.append(gam_info)
                    response.update_status('GAM sucessfully trained', status_nr + 5)
                except Exception as ex:
                    msg = 'failed to train GAM for {}: {}'.format(key, str(ex))
                    LOGGER.debug(msg)
                    raise Exception(msg)

                try:
                    prediction = sdm.get_prediction(gam_model, ncs)
                    response.update_status('prediction done', status_nr + 7)
                except Exception as ex:
                    msg = 'failed to predict tree occurence: {}'.format(str(ex))
                    LOGGER.exception(msg)
                    raise Exception(msg)
                #
                # try:
                #     response.update_status('land sea mask for predicted data',  status_nr + 8)
                #     from numpy import invert, isnan, nan, broadcast_arrays  # , array, zeros, linspace, meshgrid
                #     mask = invert(isnan(PAmask))
                #     mask = broadcast_arrays(prediction, mask)[1]
                #     prediction[mask is False] = nan
                # except:
                #     LOGGER.debug('failed to mask predicted data')

                try:
                    species_files.append(sdm.write_to_file(ncs[0], prediction))
                    LOGGER.info('Favourability written to file')
                except Exception as ex:
                    msg = 'failed to write species file: {}'.format(str(ex))
                    LOGGER.debug(msg)
                    raise Exception(msg)

            except Exception as ex:
                msg = 'failed to process SDM chain for {} : {}'.format(key, str(ex))
                LOGGER.exception(msg)
                raise Exception(msg)

        try:
            archive_indices = archive(ncs_indices, format=archive_format)
            LOGGER.info('indices added to archive')
        except Exception as ex:
            msg = 'failed adding indices to archive: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)
            archive_indices = tempfile.mkstemp(suffix='.tar', prefix='foobar-', dir='.')

        try:
            archive_references = archive(ncs_references, format=archive_format)
            LOGGER.info('indices reference added to archive')
        except Exception as ex:
            msg = 'failed adding reference indices to archive: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)
            archive_references = tempfile.mkstemp(suffix='.tar', prefix='foobar-', dir='.')

        try:
            archive_prediction = archive(species_files, format=archive_format)
            LOGGER.info('species_files added to archive')
        except Exception as ex:
            msg = 'failed adding species_files indices to archive: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            stat_infosconcat = pdfmerge(stat_infos)
            LOGGER.debug('pngs {}'.format(PAmask_pngs))
            PAmask_png = concat_images(PAmask_pngs, orientation='h')
            LOGGER.info('stat infos pdfs and mask pngs merged')
        except Exception as ex:
            msg = 'failed to concat images: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)
            _, stat_infosconcat = tempfile.mkstemp(suffix='.pdf', prefix='foobar-', dir='.')
            _, PAmask_png = tempfile.mkstemp(suffix='.png', prefix='foobar-', dir='.')

        # self.output_csv.setValue(csv_file)
        response.outputs['output_gbif'].file = occurence_map
        response.outputs['output_PA'].file = PAmask_png
        response.outputs['output_indices'].file = archive_indices
        response.outputs['output_reference'].file = archive_references
        response.outputs['output_prediction'].file = archive_prediction
        response.outputs['output_info'].file = stat_infosconcat

        response.update_status('done', 100)
        return response
