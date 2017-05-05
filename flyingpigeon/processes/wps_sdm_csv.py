"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from flyingpigeon.sdm import _SDMINDICES_

from flyingpigeon.log import init_process_logger

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.inout.literaltypes import AllowedValue
from pywps.app.Common import Metadata


import logging
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
                         default='http://localhost:8090/wpsoutputs/flyingpigeon/output_csv-abe15f64-c30d-11e6-bf63-142d277ef1f3.csv'
                         ),

            LiteralInput("input_indices", "Indices",
                         abstract="Climate indices related to growth conditions \
                                    of tree species",
                         default=['TG_JJA', 'TNn_Jan'],
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

            # self.output_csv = self.addComplexOutput(
            #     identifier="output_csv",
            #     title="Tree species table",
            #     abstract="Extracted CSV file containing the tree species table",
            #     formats=[{"mimeType": "text/csv"}],
            #     asReference=True,
            #     )

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

        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive, archiveextract, download
        response.update_status('Start process', 0)

        try:
            LOGGER.info('reading the arguments')
            resources_raw = archiveextract(
                resource=rename_complexinputs(request.inputs['resources']))
            csv_url = request.inputs['gbif'][0].data
            period = request.inputs['period']
            period = period[0].data
            indices = request.inputs['input_indices']
            archive_format = request.inputs['archive_format']
            LOGGER.info('indices %s ' % indices)
            LOGGER.debug('csv_url %s' % csv_url)
        except Exception:
            LOGGER.exception('failed to read in the arguments')
            raise

        try:
            LOGGER.info('set up the environment')
            csv_file = download(csv_url)
            resources = archiveextract(resources_raw)
        except:
            LOGGER.exception('failed to set up the environment')
            raise

        try:
            response.update_status('read in latlon coordinates', 10)
            latlon = sdm.latlon_gbifcsv(csv_file)
            LOGGER.info('got occurence coordinates %s ' % csv_file)
        except:
            LOGGER.exception('failed to extract the latlon points from file: %s' % (csv_file))

        try:
            response.update_status('plot map', 20)
            from flyingpigeon.visualisation import map_gbifoccurrences
            # latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except:
            LOGGER.exception('failed to plot occurence map')

        #################################
        # calculate the climate indices
        #################################

        # get the indices
        ncs_indices = None
        try:
            response.update_status('start calculation of climate indices for %s'
                                   % indices, 30)
            ncs_indices = sdm.get_indices(resources=resources, indices=indices)
            LOGGER.info('indice calculation done')
        except:
            msg = 'failed to calculate indices'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('get domain', 30)
            domains = set()
            for resource in ncs_indices:
                # get_domain works only if metadata are set in a correct way
                domains = domains.union([basename(resource).split('_')[1]])
            if len(domains) == 1:
                domain = list(domains)[0]
                LOGGER.debug('Domain %s found in indices files' % domain)
            else:
                LOGGER.error('Not a single domain in indices files %s' % domains)
        except:
            LOGGER.exception('failed to get domains')

        try:
            response.update_status('generating the PA mask', 20)
            PAmask = sdm.get_PAmask(coordinates=latlon, domain=domain)
            LOGGER.info('PA mask sucessfully generated')
        except:
            LOGGER.exception('failed to generate the PA mask')

        try:
            response.update_status('Ploting PA mask', 25)
            from flyingpigeon.visualisation import map_PAmask
            PAmask_png = map_PAmask(PAmask)
        except:
            LOGGER.exception('failed to plot the PA mask')

        try:
            # sort indices
            indices_dic = None
            indices_dic = sdm.sort_indices(ncs_indices)
            LOGGER.info('indice files sorted for %s Datasets' %
                        len(indices_dic.keys()))
        except:
            msg = 'failed to sort indices'
            LOGGER.exception(msg)
            raise Exception(msg)

        ncs_references = []
        species_files = []
        stat_infos = []

        for count, key in enumerate(indices_dic.keys()):
            try:
                staus_nr = 40 + count*10
                response.update_status('Start processing of %s' % key, staus_nr)
                ncs = indices_dic[key]
                LOGGER.info('with %s files' % len(ncs))
                try:
                    ncs_reference = sdm.get_reference(ncs_indices=ncs, period=period)
                    ncs_references.extend(ncs_reference)
                    LOGGER.info('reference indice calculated %s '
                                % ncs_references)
                except:
                    msg = 'failed to calculate the reference'
                    LOGGER.exception(msg)
                    raise Exception(msg)

                try:
                    gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask)
                    stat_infos.append(gam_info)
                    response.update_status('GAM sucessfully trained', staus_nr + 5)
                except:
                    msg = 'failed to train GAM for %s' % (key)
                    LOGGER.debug(msg)

                try:
                    prediction = sdm.get_prediction(gam_model, ncs)
                    response.update_status('prediction done', staus_nr + 7)
                except:
                    msg = 'failed to predict tree occurence'
                    LOGGER.exception(msg)
                    # raise Exception(msg)

                try:
                    response.update_status('land sea mask for predicted data',  staus_nr + 8)
                    from numpy import invert, isnan, nan, broadcast_arrays  # , array, zeros, linspace, meshgrid
                    mask = invert(isnan(PAmask))
                    mask = broadcast_arrays(prediction, mask)[1]
                    prediction[mask is False] = nan
                except:
                    LOGGER.debug('failed to mask predicted data')

                try:
                    species_files.append(sdm.write_to_file(ncs[0], prediction))
                    LOGGER.info('Favourabillity written to file')
                except:
                    msg = 'failed to write species file'
                    LOGGER.debug(msg)
                    # raise Exception(msg)

            except:
                msg = 'failed to calculate reference indices.'
                LOGGER.exception(msg)
                raise Exception(msg)

        try:
            archive_indices = None
            archive_indices = archive(ncs_indices, format=archive_format)
            LOGGER.info('indices added to archive')
        except:
            msg = 'failed adding indices to archive'
            LOGGER.exception(msg)
            raise Exception(msg)

        archive_references = None
        try:
            archive_references = archive(ncs_references, format=archive_format)
            LOGGER.info('indices reference added to archive')
        except:
            msg = 'failed adding reference indices to archive'
            LOGGER.exception(msg)
            raise Exception(msg)

        archive_predicion = None
        try:
            archive_predicion = archive(species_files, format=archive_format)
            LOGGER.info('species_files added to archive')
        except:
            msg = 'failed adding species_files indices to archive'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            from flyingpigeon.visualisation import pdfmerge
            stat_infosconcat = pdfmerge(stat_infos)
            LOGGER.info('stat infos pdfs merged')
        except:
            LOGGER.exception('failed to concat images')
            _, stat_infosconcat = tempfile.mkstemp(suffix='.pdf', prefix='foobar-', dir='.')

        # self.output_csv.setValue(csv_file)
        response.outputs['output_gbif'].file = occurence_map
        response.outputs['output_PA'].file = PAmask_png
        response.outputs['output_indices'].file = archive_indices
        response.outputs['output_reference'].file = archive_references
        response.outputs['archive_predicion'].file = archive_predicion
        response.outputs['output_info'].file = stat_infosconcat

        response.update_status('done', 100)
        return response
