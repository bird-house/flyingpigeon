"""
Processes for Weather Classification
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""
from flyingpigeon.datafetch import _PRESSUREDATA_
from flyingpigeon.weatherregimes import _TIMEREGIONS_
from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import BoundingBoxInput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class WeatherregimesmodelProcess(Process):
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

            # BoundingBoxInput('bbox', 'Bounding Box',
            #                  abstract='Bounding box to define the region for weather classification.'
            #                           ' Default: -80, 20, 50, 70.',
            #                  crss=['epsg:4326'],
            #                  min_occurs=0),

            LiteralInput("season", "Time region",
                         abstract="Select the months to define the time region (all == whole year will be analysed)",
                         default="DJF",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=_TIMEREGIONS_.keys()
                         ),

            LiteralInput("period", "Period for weatherregime calculation",
                         abstract="Period for analysing the dataset",
                         default="19700101-20101231",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("anualcycle", "Period for anualcycle calculation",
                         abstract="Period for anual cycle calculation",
                         default="19700101-19991231",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("kappa", "Nr of Weather regimes",
                         abstract="Set the number of clusters to be detected",
                         default='4',
                         data_type='integer',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=range(2, 11)
                         ),
        ]

        outputs = [
            ComplexOutput("Routput_graphic", "Weather Regime Pressure map",
                          abstract="Weather Classification",
                          supported_formats=[Format('image/pdf')],
                          as_reference=True,
                          ),

            ComplexOutput("output_pca", "R - datafile",
                          abstract="Principal components (PCA)",
                          supported_formats=[Format('text/plain')],
                          as_reference=True,
                          ),

            ComplexOutput("output_classification", "R - workspace",
                          abstract="Weather regime classification",
                          supported_formats=[Format("application/octet-stream")],
                          as_reference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for one dataset',
                          abstract="Prepared netCDF file as input for weatherregime calculation",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]
        super(WeatherregimesmodelProcess, self).__init__(
            self._handler,
            identifier="weatherregimes_model",
            title="Weather Regimes (based on climate model data)",
            abstract='k-mean cluster analyse of the pressure patterns. Clusters are equivalent to weather regimes',
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

        LOGGER.info('Start process')
        from datetime import datetime as dt
        from flyingpigeon import weatherregimes as wr
        from flyingpigeon.utils import archive, archiveextract
        from tempfile import mkstemp

        response.update_status('execution started at : %s ' % dt.now(), 5)

        ################################
        # reading in the input arguments
        ################################
        try:
            response.update_status('execution started at : {}'.format(dt.now()), 5)

            ################################
            # reading in the input arguments
            ################################
            LOGGER.info('read in the arguments')
            resource = archiveextract(resource=[res.file for res in request.inputs['resource']])

            # resources = self.getInputValues(identifier='resources')
            season = request.inputs['season'][0].data
            LOGGER.info('season %s', season)
            if 'bbox' in request.inputs:
                bbox = request.inputs['bbox'][0].data
                bbox = [-80, 20, 50, 70]
            else:
                bbox = [-80, 20, 50, 70]
            period = request.inputs['period'][0].data
            LOGGER.info('period %s', period)
            anualcycle = request.inputs['anualcycle'][0].data
            kappa = request.inputs['kappa'][0].data
            LOGGER.info('kappa %s', kappa)

            start = dt.strptime(period.split('-')[0], '%Y%m%d')
            end = dt.strptime(period.split('-')[1], '%Y%m%d')
            LOGGER.debug('start: %s , end: %s ', start, end)
            LOGGER.info('bbox %s', bbox)
            LOGGER.info('period %s', period)
            LOGGER.info('season %s', season)
        except Exception as e:
            msg = 'failed to read in the arguments'
            LOGGER.exception(msg)
            raise Exception(msg)

        ############################################################
        # get the required bbox and time region from resource data
        ############################################################
        response.update_status('start subsetting', 17)
        # from flyingpigeon.weatherregimes import get_level

        from flyingpigeon.ocgis_module import call
        from flyingpigeon.utils import get_variable
        time_range = [start, end]

        variable = get_variable(resource)
        model_subset = call(
            resource=resource, variable=variable,
            geom=bbox, spatial_wrapping='wrap', time_range=time_range,  # conform_units_to=conform_units_to
        )
        LOGGER.info('Dataset subset done: %s ' % model_subset)
        response.update_status('dataset subsetted', 19)

        #####################
        # computing anomalies
        #####################

        response.update_status('computing anomalies ', 19)

        cycst = anualcycle.split('-')[0]
        cycen = anualcycle.split('-')[0]
        reference = [dt.strptime(cycst, '%Y%m%d'), dt.strptime(cycen, '%Y%m%d')]
        model_anomal = wr.get_anomalies(model_subset, reference=reference)

        ###################
        # extracting season
        ####################
        model_season = wr.get_season(model_anomal, season=season)
        response.update_status('values normalized', 20)

        ####################
        # call the R scripts
        ####################
        response.update_status('Start weather regime clustering ', 25)
        import shlex
        import subprocess
        from flyingpigeon import config
        from os.path import curdir, exists, join

        try:
            rworkspace = curdir
            Rsrc = config.Rsrc_dir()
            Rfile = 'weatherregimes_model.R'

            infile = model_season  # model_subset #model_ponderate
            modelname = 'MODEL'
            yr1 = start.year
            yr2 = end.year
            ip, output_graphics = mkstemp(dir=curdir, suffix='.pdf')
            ip, file_pca = mkstemp(dir=curdir, suffix='.txt')
            ip, file_class = mkstemp(dir=curdir, suffix='.Rdat')

            args = ['Rscript', join(Rsrc, Rfile), '%s/' % curdir,
                    '%s/' % Rsrc, '%s' % infile, '%s' % variable,
                    '%s' % output_graphics, '%s' % file_pca,
                    '%s' % file_class, '%s' % season,
                    '%s' % start.year, '%s' % end.year,
                    '%s' % 'MODEL', '%s' % kappa]
            LOGGER.info('Rcall builded')
        except Exception as e:
            msg = 'failed to build the R command %s' % e
            LOGGER.error(msg)
            raise Exception(msg)
        try:
            output, error = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            # ,shell=True
            LOGGER.info('R outlog info:\n %s ' % output)
            LOGGER.debug('R outlog errors:\n %s ' % error)
            if len(output) > 0:
                response.update_status('**** weatherregime in R suceeded', 90)
            else:
                LOGGER.error('NO! output returned from R call')
        except Exception as e:
            msg = 'weatherregime in R %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Weather regime clustering done ', 90)
        ############################################
        # set the outputs
        ############################################
        response.update_status('Set the process outputs ', 95)

        response.outputs['Routput_graphic'].file = output_graphics
        response.outputs['output_pca'].file = file_pca
        response.outputs['output_classification'].file = file_class
        response.outputs['output_netcdf'].file = model_season
        response.update_status('done', 100)
        return response
