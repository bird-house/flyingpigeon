"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps.Process import WPSProcess
from flyingpigeon.sdm import _SDMINDICES_
from flyingpigeon.log import init_process_logger
import logging
logger = logging.getLogger(__name__)


class SDMallinoneProcess(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="sdm_allinone",
            title="SDM -- all in one ",
            version="0.9",
            metadata=[
                {"title": "LWF", "href": "http://www.lwf.bayern.de/"},
                {"title": "Doc",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/"}, ],
            abstract="Species distribution model for tree species based on\
             scientific species name and raw model data. GBIF data fetch and\
             indices calculation will be done within the process",
            statusSupported=True,
            storeSupported=True
            )

        # Literal Input Data
        # ------------------
        self.resources = self.addComplexInput(
            identifier="resources",
            title="tas/pr files",
            abstract="Raw climate model outputs as stored in netCDF files. archives (tar/zip) can also be provided",
            minOccurs=1,
            maxOccurs=500,
            maxmegabites=50000,
            formats=[{"mimeType": "application/x-netcdf"},
                     {"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            )

        self.taxon_name = self.addLiteralInput(
            identifier="taxon_name",
            title="Tree Species",
            abstract="Scientific name of tree species",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            default='Fagus sylvatica'
            )

        """
        # TODO: bbox parameter is not working
        self.BBox = self.addBBoxInput(
            identifier="BBox",
            title="Bounding Box",
            abstract="coordinates to define the region for occurence data fetch",
            minOccurs=1,
            maxOccurs=1,
            crss=['EPSG:4326']
            )
        """

        self.input_indices = self.addLiteralInput(
            identifier="input_indices",
            title="Indices",
            abstract="Climate indices related to growth\
             conditions of tree species",
            default=['TG_JJA', 'TNn_Jan'],
            type=type(''),
            minOccurs=1,
            maxOccurs=10,
            allowedValues=_SDMINDICES_
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate condition\
             (all = entire timeseries)",
            default="all",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['all', '1951-1980', '1961-1990',
                           '1971-2000', '1981-2010']
            )

        self.archive_format = self.addLiteralInput(
            identifier="archive_format",
            title="Archive format",
            abstract="Result files will be compressed into archives.\
             Choose an appropriate format",
            default="tar",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['zip', 'tar']
            )

        ###########
        # OUTPUTS
        ###########

        self.output_csv = self.addComplexOutput(
            identifier="output_csv",
            title="Tree species table",
            abstract="Extracted CSV file containing the tree species table ",
            formats=[{"mimeType": "text/csv"}],
            asReference=True,
            )

        self.output_gbif = self.addComplexOutput(
            identifier="output_gbif",
            title="Graphic of GBIF coordinates",
            abstract="PNG graphic file showing the presence of tree species",
            formats=[{"mimeType": "image/png"}],
            asReference=True,
            )

        self.output_PA = self.addComplexOutput(
            identifier="output_PA",
            title="Graphic of PA mask",
            abstract="PNG graphic file showing generated PA mask",
            formats=[{"mimeType": "image/png"}],
            asReference=True,
            )

        self.output_indices = self.addComplexOutput(
            identifier="output_indices",
            title="Climate indices for growth conditions over all timesteps",
            abstract="Archive (tar/zip) containing calculated climate indices",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

        self.output_reference = self.addComplexOutput(
            identifier="output_reference",
            title="Climate indices for growth conditions of reference period",
            abstract="Archive containing calculated climate indices",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

        self.output_prediction = self.addComplexOutput(
            identifier="output_prediction",
            title="predicted growth conditions",
            abstract="Archive containing predicted growth conditions",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

        self.output_info = self.addComplexOutput(
            identifier="output_info",
            title="GAM statistics information",
            abstract="Graphics and information of the learning statistics",
            formats=[{"mimeType": "application/pdf"}],
            asReference=True,
            )

        self.output_log = self.addComplexOutput(
            identifier="output_log",
            title="Logging information",
            abstract="Collected logs during process run.",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
            )

    def execute(self):
        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive, archiveextract

        self.status.set('Start process', 0)

        try:
            logger.info('reading the arguments')
            resources = archiveextract(self.getInputValues(identifier='resources'))
            taxon_name = self.getInputValues(identifier='taxon_name')[0]
            # bbox_obj = self.BBox.getValue()
            # bbox = [bbox_obj.coords[0][0],
            #         bbox_obj.coords[0][1],
            #         bbox_obj.coords[1][0],
            #         bbox_obj.coords[1][1]]
            bbox = [-180, -90, 180, 90]
            period = self.getInputValues(identifier='period')
            period = period[0]
            indices = self.getInputValues(identifier='input_indices')
            archive_format = self.archive_format.getValue()
            logger.debug("indices = %s for %s ", indices, taxon_name)
            logger.info("bbox={0}".format(bbox))
        except Exception as e:
            logger.error('failed to read in the arguments %s ' % e)
        logger.info('indices %s ' % indices)

        try:
            self.status.set('Fetching GBIF Data', 10)
            gbifdic = sdm.get_gbif(taxon_name, bbox=bbox)
        except Exception as e:
            msg = 'failed to search gbif.'
            logger.exception(msg)
            raise Exception(msg)

        try:
            self.status.set('write csv file', 70)
            gbifcsv = sdm.gbifdic2csv(gbifdic)
        except Exception as e:
            msg = 'failed to write csv file.'
            logger.exception(msg)
            raise Exception(msg)

        try:
            self.status.set('plot map', 80)
            from flyingpigeon.visualisation import map_gbifoccurrences
            latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except Exception as e:
            msg = 'failed to plot occurence map.'
            logger.exception(msg)
            raise Exception(msg)

        #################################
        # calculate the climate indices
        #################################

        # get the indices
        ncs_indices = None
        try:
            self.status.set('start calculation of climate indices for %s'
                            % indices, 30)
            ncs_indices = sdm.get_indices(resources=resources, indices=indices)
            logger.info('indice calculation done')
        except:
            msg = 'failed to calculate indices'
            logger.exception(msg)
            raise Exception(msg)

        try:
            self.status.set('get domain', 30)
            domains = set()
            for resource in ncs_indices:
                # get_domain works only if metadata are set in a correct way
                domains = domains.union([basename(resource).split('_')[1]])
            if len(domains) == 1:
                domain = list(domains)[0]
                logger.debug('Domain %s found in indices files' % domain)
            else:
                logger.error('Not a single domain in indices files %s' % domains)
        except Exception as e:
            logger.exception('failed to get domains %s' % e)

        try:
            self.status.set('generating the PA mask', 20)
            PAmask = sdm.get_PAmask(coordinates=latlon, domain=domain)
            logger.info('PA mask sucessfully generated')
        except Exception as e:
            logger.exception('failed to generate the PA mask: %s' % e)

        try:
            self.status.set('Ploting PA mask', 25)
            from flyingpigeon.visualisation import map_PAmask
            PAmask_png = map_PAmask(PAmask)
        except Exception as e:
            logger.exception('failed to plot the PA mask: %s' % e)

        try:
            # sort indices
            indices_dic = None
            indices_dic = sdm.sort_indices(ncs_indices)
            logger.info('indice files sorted for %s Datasets' %
                        len(indices_dic.keys()))
        except:
            msg = 'failed to sort indices'
            logger.exception(msg)
            raise Exception(msg)

        ncs_references = []
        species_files = []
        stat_infos = []

        for count, key in enumerate(indices_dic.keys()):
            try:
                staus_nr = 40 + count*10
                self.status.set('Start processing of %s' % key, staus_nr)
                ncs = indices_dic[key]
                logger.info('with %s files' % len(ncs))
                try:
                    ncs_reference = sdm.get_reference(ncs_indices=ncs, period=period)
                    ncs_references.extend(ncs_reference)
                    logger.info('reference indice calculated %s '
                                % ncs_references)
                except:
                    msg = 'failed to calculate the reference'
                    logger.exception(msg)
                    raise Exception(msg)

                try:
                    gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask)
                    stat_infos.append(gam_info)
                    self.status.set('GAM sucessfully trained', staus_nr + 5)
                except Exception as e:
                    msg = 'failed to train GAM for %s : %s' % (key, e)
                    logger.debug(msg)

                try:
                    prediction = sdm.get_prediction(gam_model, ncs)
                    self.status.set('prediction done', staus_nr + 7)
                except Exception as e:
                    msg = 'failed to predict tree occurence %s' % e
                    logger.exception(msg)
                    # raise Exception(msg)

                try:
                    self.status.set('land sea mask for predicted data',  staus_nr + 8)
                    from numpy import invert, isnan, nan, broadcast_arrays  # , array, zeros, linspace, meshgrid
                    mask = invert(isnan(PAmask))
                    mask = broadcast_arrays(prediction, mask)[1]
                    prediction[mask is False] = nan
                except Exception as e:
                    logger.debug('failed to mask predicted data: %s' % e)

                try:
                    species_files.append(sdm.write_to_file(ncs[0], prediction))
                    logger.info('Favourabillity written to file')
                except Exception as e:
                    msg = 'failed to write species file %s' % e
                    logger.debug(msg)
                    # raise Exception(msg)

            except Exception as e:
                msg = 'failed to calculate reference indices. %s ' % e
                logger.exception(msg)
                raise Exception(msg)

        try:
            archive_indices = None
            archive_indices = archive(ncs_indices, format=archive_format)
            logger.info('indices added to archive')
        except:
            msg = 'failed adding indices to archive'
            logger.exception(msg)
            raise Exception(msg)

        archive_references = None
        try:
            archive_references = archive(ncs_references, format=archive_format)
            logger.info('indices reference added to archive')
        except:
            msg = 'failed adding reference indices to archive'
            logger.exception(msg)
            raise Exception(msg)

        archive_predicion = None
        try:
            archive_predicion = archive(species_files, format=archive_format)
            logger.info('species_files added to archive')
        except:
            msg = 'failed adding species_files indices to archive'
            logger.exception(msg)
            raise Exception(msg)
        try:
            from flyingpigeon.visualisation import pdfmerge
            stat_infosconcat = pdfmerge(stat_infos)
            logger.info('stat infos pdfs merged')
        except:
            logger.exception('failed to concat images')
            _, stat_infosconcat = tempfile.mkstemp(suffix='.pdf', prefix='foobar-', dir='.')

        self.output_csv.setValue(gbifcsv)
        self.output_gbif.setValue(occurence_map)
        self.output_PA.setValue(PAmask_png)
        self.output_indices.setValue(archive_indices)
        self.output_reference.setValue(archive_references)
        self.output_prediction.setValue(archive_predicion)
        self.output_info.setValue(stat_infosconcat)

        self.status.set('done', 100)
