"""
Processes for Species distribution
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

import tempfile

from pywps.Process import WPSProcess

from flyingpigeon.sdm import _SDMINDICES_
from flyingpigeon.log import init_process_logger

import logging
logger = logging.getLogger(__name__)


class SDMcsvindicesProcess(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier="sdm_csvindices",
            title="SDM -- CSV Table and Indices",
            version="0.9",
            metadata=[
                {"title": "LWF", "href": "http://www.lwf.bayern.de/"},
                {"title": "Doc",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/index.html#species-distribution-model"},  # NOQA
                {"title": "Paper",
                 "href": "http://www.hindawi.com/journals/jcli/2013/787250/"},
                {"title": "Tutorial",
                 "href": "http://flyingpigeon.readthedocs.io/en/latest/tutorials/sdm.html"},
            ],
            abstract="Species distribution model for tree species based on GBIF"
                     " presence/absence data and precalculated Indices",
            statusSupported=True,
            storeSupported=True
        )

        # Literal Input Data
        # ------------------

        self.input_indices = self.addComplexInput(
            identifier="input_indices",
            title="Precalculated Indices",
            abstract="Precalculated Indices as basis for the SDM calculation"
                     " (list of netCDF files or tar/zip archive)",
            minOccurs=1,
            maxOccurs=500,
            # maxmegabites=50,
            formats=[{"mimeType": "application/x-netcdf"},
                     {"mimeType": "application/x-tar"},
                     {"mimeType": "application/zip"}],
        )

        self.gbif = self.addComplexInput(
            identifier="gbif",
            title="GBIF csv file",
            abstract="GBIF table (csv) with tree occurence"
                     " (output of 'GBIF data fetch' process)",
            minOccurs=1,
            maxOccurs=1,
            # maxmegabites=50,
            formats=[{"mimeType": "text/csv"}],
        )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Reference period",
            abstract="Reference period for climate conditions"
                     " (all = entire timeseries)",
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
            abstract="Result files will be compressed into archives."
                     " Choose an appropriate format.",
            default="tar",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['zip', 'tar']
        )

        ###########
        # OUTPUTS
        ###########

        self.output_gbif = self.addComplexOutput(
            identifier="output_gbif",
            title="Graphic of GBIF coordinates",
            abstract="PNG graphic file showing the presence of tree species"
                     " according to the CSV file",
            formats=[{"mimeType": "image/png"}],
            asReference=True,
        )

        self.output_PA = self.addComplexOutput(
            identifier="output_PA",
            title="Graphic of PA mask",
            abstract="PNG graphic file showing PA mask generated based"
                     " on netCDF spatial increment.",
            formats=[{"mimeType": "image/png"}],
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
            abstract="Archive (tar/zip) containing the netCDF files of the"
                     " predicted growth conditions.",
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
        from os.path import basename
        from flyingpigeon import sdm
        from flyingpigeon.utils import archive, archiveextract  # , get_domain

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        self.status.set('Start process', 0)

        try:
            self.status.set('reading the arguments', 5)
            resources = archiveextract(self.getInputValues(identifier='input_indices'))
            csv_file = self.getInputValues(identifier='gbif')[0]
            period = self.getInputValues(identifier='period')
            period = period[0]
            archive_format = self.archive_format.getValue()
        except:
            logger.error('failed to read in the arguments')

        try:
            self.status.set('read in latlon coordinates', 10)
            latlon = sdm.latlon_gbifcsv(csv_file)
        except:
            logger.exception('failed to extract the latlon points')

        try:
            self.status.set('plot map', 20)
            from flyingpigeon.visualisation import map_gbifoccurrences
            # latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except:
            logger.exception('failed to plot occurence map')

        try:
            self.status.set('get domain', 30)
            domains = set()
            for indice in resources:
                # get_domain works only if metadata are set in a correct way
                domains = domains.union([basename(indice).split('_')[1]])
            if len(domains) == 1:
                domain = list(domains)[0]
                logger.info('Domain %s found in indices files' % domain)
            else:
                logger.warn('NOT a single domain in indices files %s' % domains)
        except:
            logger.exception('failed to get domains')

        try:
            self.status.set('generating the PA mask', 20)
            PAmask = sdm.get_PAmask(coordinates=latlon, domain=domain)
            logger.info('PA mask sucessfully generated')
        except:
            logger.exception('failed to generate the PA mask')

        try:
            self.status.set('Ploting PA mask', 25)
            from flyingpigeon.visualisation import map_PAmask
            PAmask_png = map_PAmask(PAmask)
        except:
            logger.exception('failed to plot the PA mask')

        try:
            # sort indices
            indices_dic = sdm.sort_indices(resources)
            logger.info('indice files sorted for %s Datasets' %
                        len(indices_dic.keys()))
        except:
            msg = 'failed to sort indices'
            logger.exception(msg)
            raise Exception(msg)

        ncs_references = []
        species_files = []
        stat_infos = []

        self.status.set('Start processing for %s Datasets' % len(indices_dic.keys()))
        for count, key in enumerate(indices_dic.keys()):
            try:
                staus_nr = 40 + count * 10
                self.status.set('Start processing of %s' % key, staus_nr)
                ncs = indices_dic[key]
                logger.info('with %s files' % len(ncs))
                try:
                    ncs_reference = sdm.get_reference(ncs_indices=ncs, period=period)
                    ncs_references.extend(ncs_reference)
                    logger.info('reference indice calculated %s '
                                % ncs_references)
                    self.status.set('reference indice calculated', staus_nr + 2)
                except:
                    msg = 'failed to calculate the reference'
                    logger.exception(msg)
                    # raise Exception(msg)

                try:
                    gam_model, predict_gam, gam_info = sdm.get_gam(ncs_reference, PAmask)
                    stat_infos.append(gam_info)
                    self.status.set('GAM sucessfully trained', staus_nr + 5)
                except:
                    msg = 'failed to train GAM for %s' % (key)
                    logger.exception(msg)

                try:
                    prediction = sdm.get_prediction(gam_model, ncs)
                    self.status.set('prediction done', staus_nr + 7)
                except:
                    msg = 'failed to predict tree occurence'
                    logger.exception(msg)
                    # raise Exception(msg)

                try:
                    self.status.set('land sea mask for predicted data', staus_nr + 8)
                    from numpy import invert, isnan, nan, broadcast_arrays  # , array, zeros, linspace, meshgrid
                    mask = invert(isnan(PAmask))
                    mask = broadcast_arrays(prediction, mask)[1]
                    prediction[mask is False] = nan
                except:
                    logger.exception('failed to mask predicted data')

                try:
                    species_files.append(sdm.write_to_file(ncs[0], prediction))
                    logger.info('Favourabillity written to file')
                except:
                    msg = 'failed to write species file'
                    logger.exception(msg)
                    # raise Exception(msg)
            except:
                msg = 'failed to process SDM chain for %s ' % key
                logger.exception(msg)
                # raise Exception(msg)

        try:
            archive_references = None
            archive_references = archive(ncs_references, format=archive_format)
            logger.info('indices 2D added to archive')
        except:
            msg = 'failed adding 2D indices to archive'
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

        self.output_gbif.setValue(occurence_map)
        self.output_PA.setValue(PAmask_png)
        self.output_reference.setValue(archive_references)
        self.output_prediction.setValue(archive_predicion)
        self.output_info.setValue(stat_infosconcat)
        self.status.set('done', 100)
