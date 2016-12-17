"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from pywps.Process import WPSProcess
from flyingpigeon.sdm import _SDMINDICES_
import logging
logger = logging.getLogger(__name__)


class sdmcsvProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="sdm_csv",
            title="SDM -- CSV table",
            version="0.9",
            metadata=[
                {"title": "LWF",
                 "href": "http://www.lwf.bayern.de/"},
                {"title": "Doc",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/\
                        descriptions/index.html#species-distribution-model"},
                {"title": "Paper",
                 "href": "http://www.hindawi.com/journals/jcli/2013/787250/"},
                {"title": "Tutorial",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/\
                 tutorials/sdm.html"},
                ],
            abstract="Species distribution model for tree species based on\
             GBIF presence/absence data and climate model data. Indices will\
             be calculated while processing",
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

        self.gbif = self.addLiteralInput(
            identifier="gbif",
            title="GBIF csv file",
            abstract="GBIF table (csv) with tree occurence \
            (output of 'GBIF data fetch' process )",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            default='http://localhost:8090/wpsoutputs/flyingpigeon/output_csv-abe15f64-c30d-11e6-bf63-142d277ef1f3.csv'
            # maxmegabites=50,
            # formats=[{"mimeType": "text/csv"}],
            )

        self.input_indices = self.addLiteralInput(
            identifier="input_indices",
            title="Indices",
            abstract="Climate indices related to growth conditions",
            # default=['TG_JJA', 'TNn_Jan'],
            type=type(''),
            minOccurs=1,
            maxOccurs=10,
            allowedValues=_SDMINDICES_
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate conditions\
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
        #
        # self.output_csv = self.addComplexOutput(
        #     identifier="output_csv",
        #     title="Tree species table",
        #     abstract="Extracted CSV file containing the tree species table",
        #     formats=[{"mimeType": "text/csv"}],
        #     asReference=True,
        #     )

        self.output_gbif = self.addComplexOutput(
            identifier="output_gbif",
            title="Graphic of GBIF coordinates",
            abstract="PNG graphic file showing the presence of tree species\
             according to the CSV file",
            formats=[{"mimeType": "image/png"}],
            asReference=True,
            )

        self.output_PA = self.addComplexOutput(
            identifier="output_PA",
            title="Graphic of PA mask",
            abstract="PNG graphic file showing PA mask generated based on\
             netCDF spatial increment",
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
            abstract="Archive (tar/zip) containing calculated climate indices",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

        self.output_prediction = self.addComplexOutput(
            identifier="output_prediction",
            title="predicted growth conditions",
            abstract="Archive containing files of the predicted\
             growth conditions",
            formats=[{"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
            asReference=True,
            )

        self.output_info = self.addComplexOutput(
            identifier="output_info",
            title="GAM statistics information",
            abstract="Graphics and information of the learning statistics",
            formats=[{"mimeType": "image/png"}],
            asReference=True,
            )

    def execute(self):
        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive, archiveextract, download
        self.status.set('Start process', 0)

        try:
            logger.info('reading the arguments')
            resources_raw = self.getInputValues(identifier='resources')
            csv_url = self.getInputValues(identifier='gbif')[0]
            period = self.getInputValues(identifier='period')
            period = period[0]
            indices = self.getInputValues(identifier='input_indices')
            archive_format = self.archive_format.getValue()
            logger.info('indices %s ' % indices)
            logger.debug('csv_url %s' % csv_url)
        except Exception as e:
            logger.error('failed to read in the arguments %s ' % e)
            raise

        try:
            logger.info('set up the environment')
            csv_file = download(csv_url)
            resources = archiveextract(resources_raw)
        except Exception as e:
            logger.error('failed to set up the environment %s ' % e)
            raise

        try:
            self.status.set('read in latlon coordinates', 10)
            latlon = sdm.latlon_gbifcsv(csv_file)
            logger.info('got occurence coordinates %s ' % csv_file)
        except Exception as e:
            logger.exception('failed to extract the latlon points from file: %s: %s' % (csv_file, e))

        try:
            self.status.set('plot map', 20)
            from flyingpigeon.visualisation import map_gbifoccurrences

            # latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except Exception as e:
            logger.exception('failed to plot occurence map %s' % e)

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

        # try:
        #     archive_indices = archive(ncs_indices, format=archive_format)
        #     logger.info('indices 3D added to tarfile')
        # except:
        #     msg = 'failed adding indices to tar'
        #     logger.exception(msg)
        #     raise Exception(msg)

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

        from flyingpigeon.visualisation import concat_images
        try:
            stat_infosconcat = concat_images(stat_infos, orientation='v')
        except:
            msg = 'failed to concat images'
            logger.exception(msg)
            raise Exception(msg)

        # self.output_csv.setValue(csv_file)
        self.output_gbif.setValue(occurence_map)
        self.output_PA.setValue(PAmask_png)
        self.output_indices.setValue(archive_indices)
        self.output_reference.setValue(archive_references)
        self.output_prediction.setValue(archive_predicion)
        self.output_info.setValue(stat_infosconcat)

        self.status.set('done', 100)
