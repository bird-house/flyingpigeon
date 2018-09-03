"""
Processes to fetch data from GBIF data base
Author: Nils Hempelmann (info@nilshempelmann.de)
"""

<<<<<<< HEAD
from eggshell.log import init_process_logger
=======
import logging
>>>>>>> master

from pywps import ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

from flyingpigeon.log import init_process_logger

LOGGER = logging.getLogger("PYWPS")


class GBIFfetchProcess(Process):
    def __init__(self):
        inputs = [
            LiteralInput('taxon_name', 'Taxonomic name of tree species',
                         abstract='Taxonomic name of tree species (e. g. Fagus sylvatica)',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         default='Fagus sylvatica'
                         ),
        ]

        # self.BBox = self.addBBoxInput(
        # #         identifier="BBox",
        # #         title="Bounding Box",
        # #         abstract="coordinates to define the region for occurence data fetch",
        # #         minOccurs=1,
        # #         maxOccurs=1,
        # #         crss=['EPSG:4326']

        outputs = [
            ComplexOutput('output_map', 'Graphic of species occurences',
                          abstract="PNG graphic file showing the presence of tree species \
                               according to GBIF data fetch",
                          as_reference=True,
                          supported_formats=[Format('image/png')]
                          ),

            ComplexOutput('output_csv', 'Tree species table',
                          abstract="Extracted CSV file containing the tree species table",
                          as_reference=True,
                          supported_formats=[Format('text/csv')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          )
        ]

        super(GBIFfetchProcess, self).__init__(
            self._handler,
            identifier="sdm_gbiffetch",
            title="Species distribution Model (GBIF data fetch only)",
            version="0.2",
            abstract="Species occurence search in Global Biodiversity \
             Infrastructure Facillity (GBIF)",
            metadata=[
                # Metadata('LSCE', 'http://www.lsce.ipsl.fr/en/index.php'),
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
                Metadata('GBIF', 'http://gbif.org/')
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'
        #
        # init_process_logger('log.txt')
        # self.output_log.setValue('log.txt')
        #
        response.update_status('Start process', 0)
        from flyingpigeon import sdm

        try:
            LOGGER.info('reading the arguments')
            taxon_name = request.inputs['taxon_name'][0].data
            bbox = [-180, -90, 180, 90]
            # bbox_obj = self.BBox.getValue()
            # bbox = [bbox_obj.coords[0][0],
            #         bbox_obj.coords[0][1],
            #         bbox_obj.coords[1][0],
            #         bbox_obj.coords[1][1]]
            LOGGER.info("bbox={}".format(bbox))
            LOGGER.info("Taxon Name={}".format(taxon_name))
        except Exception as ex:
            msg = 'failed to read in the arguments: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('Fetching GBIF Data', 10)
            gbifdic = sdm.get_gbif(taxon_name, bbox=bbox)
        except Exception as ex:
            msg = 'failed to search gbif: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('write csv file', 70)
            gbifcsv = sdm.gbifdic2csv(gbifdic)
        except Exception as ex:
            msg = 'failed to write csv file: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            response.update_status('plot map', 80)
            from flyingpigeon.visualisation import map_gbifoccurrences
            latlon = sdm.latlon_gbifdic(gbifdic)
            occurence_map = map_gbifoccurrences(latlon)
        except Exception as ex:
            msg = 'failed to plot occurence map: {}'.format(str(ex))
            LOGGER.exception(msg)
            raise Exception(msg)

        response.outputs['output_map'].file = occurence_map
        response.outputs['output_csv'].file = gbifcsv
        response.update_status('done', 100)
        return response
        #
        # # write folder statistics:
        # import shlex
        # import subprocess
        # import os
        # import socket
        # LOGGER.debug('HOSTNAME: %s ' % socket.gethostname())
        #
        # cmd = 'stat %s/' % os.path.abspath(os.curdir)
        # args = shlex.split(cmd)
        # output, error = subprocess.Popen(
        #                 args, stdout=subprocess.PIPE,
        #                 stderr=subprocess.PIPE
        #                 ).communicate()
        #
        # LOGGER.debug('temp folder statistics: %s  ERRORS: %s' % (output, error))
