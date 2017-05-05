from os import mkdir, path, listdir
from datetime import datetime as dt

from flyingpigeon import segetalflora as sf
from flyingpigeon.subset import countries  # REGION_EUROPE

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class SegetalfloraProcess(Process):
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

            LiteralInput("climate_type", "Climate type",
                         abstract="Select climate type",
                         default='3',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=8,
                         allowed_values=["1", "2", "3", "4", "5", "6", "7", "all"]
                         ),

            LiteralInput("culture_type", "Culture type",
                         abstract="Select culture type",
                         default='fallow',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=8,
                         allowed_values=["fallow", "intensive", "extensive"]  # sem
                         ),
        ]

        outputs = [
            ComplexOutput("Yearly mean temperature", "out_tasmean",
                          abstract="Tar archive containing the netCDF EUR tas mean files",
                          supported_formats=[Format('application/x-tar')],
                          as_reference=True,
                          ),

            ComplexOutput("Segetalflora", "out_segetalflora",
                          abstract="Tar archive containing the segetalflora data ",
                          supported_formats=[Format('application/x-tar')],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]

        super(SegetalfloraProcess, self).__init__(
            self._handler,
            identifier="segetalflora",
            title="Segetal Flora",
            abstract="Species biodiversity of segetal flora. Imput files: variable:tas , \
                    domain: EUR-11 or EUR-44",
            version="0.10",
            metadata=[
                Metadata('LSCE', 'http://www.lsce.ipsl.fr/en/index.php'),
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'
        process_start_time = time.time()  # measure process execution time ...

        response.update_status('execution started at : %s ' % dt.now(), 5)

        LOGGER.debug('starting segetalflora process execution')
        response.update_status('starting calcualtion segetalflora', 5)

        ############################
        # read argments to variables
        ############################
        try:
            resource = archiveextract(resource=rename_complexinputs(request.inputs['resource']))
            climate_type = request.inputs['climate_type'][0].data
            culture_type = request.inputs['culture_type'][0].data

            LOGGER.info('urls for %s ncs found' % (len(resource)))
            LOGGER.info('culture type: %s ' % (culture_type))
        except Exception as e:
            LOGGER.debug('failed to read in the arguments: %s ' % e)

        try:
            if type(climate_type) != list:
                climate_type = list([climate_type])
            if type(culture_type) != list:
                culture_type = list([culture_type])
            LOGGER.info('arguments are lists')
        except Exception as e:
            LOGGER.debug('failed to transform arguments to lists: %s ' % e)

        #############################
        # get yearly mean temperature
        #############################

        nc_tasmean = sf.get_yrmean(resource)

        #######################################
        # main call for segetalflora processing
        #######################################

        nc_sf = sf.get_segetalflora(resource=nc_tasmean, culture_type=culture_type, climate_type=climate_type)

        ####################
        # tar file archiving
        ####################

        try:
            from flyingpigeon.utils import archive
            response.update_status('files to tar archives', 99)
            tar_sf = archive(nc_sf, format='tar', dir_output='.', mode='w')
            tar_tasmean = archive(nc_tasmean, format='tar', dir_output='.', mode='w')
            LOGGER.info('Archives prepared')
        except Exception as e:
            LOGGER.debug('failed to archive files %s' % e)

        response.outputs['out_segetalflora'] = tar_sf
        response.outputs['out_tasmean'] = tar_tasmean

        response.update_status('execution ended', 100)
        LOGGER.debug("total execution took %s seconds.", time.time() - process_start_time)
        response.update_status('preparting output', 99)
        return response
