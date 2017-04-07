"""
Processes for Weather Classification
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

from flyingpigeon.datafetch import _PRESSUREDATA_
from flyingpigeon.weatherregimes import _TIMEREGIONS_
from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class WeatherregimesreanalyseProcess(Process):
    def __init__(self):
        inputs = [

            # self.BBox = self.addBBoxInput(
            #     identifier="BBox",
            #     title="Bounding Box",
            #     abstract="coordinates to define the region for weather classification ('EPSG:4326')",
            #     minOccurs=1,
            #     maxOccurs=1,
            #     crss=['EPSG:4326']
            #     )

            # Literal Input Data
            # ------------------

            # self.BBox = self.addLiteralInput(
            #     identifier="BBox",
            #     title="Region",
            #     abstract="coordinates to define the region: (minlon,maxlon,minlat,maxlat)",
            #     default='-80,22.5,50,70', #  cdo syntax: 'minlon,maxlon,minlat,maxlat' ;\
            #  ocgis syntax (minlon,minlat,maxlon,maxlat)
            #     type=type(''),
            #     minOccurs=1,
            #     maxOccurs=1,
            #     )

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

            LiteralInput("reanalyses", "Reanalyses Data",
                         abstract="Choose a reanalyses dataset for comparison",
                         default="NCEP_slp",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=_PRESSUREDATA_
                         ),

            # LiteralInput("kappa", "Nr of Weather regimes",
            #              abstract="Set the number of clusters to be detected",
            #              default='4',
            #              data_type='integer',
            #              min_occurs=1,
            #              max_occurs=1,
            #              allowed_values=['2', '3', '4', '5', '6', '7', '8', '9', '10', '11']  # int(range(2, 11))
            #              ),
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

        super(WeatherregimesreanalyseProcess, self).__init__(
            self._handler,
            identifier="weatherregimes_reanalyse",
            title="Weather Regimes (based on reanalyses data)",
            abstract='k-mean cluster analyse of the pressure patterns. Clusters are equivalent to weather regimes'
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
        from tempfile import mkstemp

        response.update_status('execution started at : %s ' % dt.now(), 5)

        ################################
        # reading in the input arguments
        ################################
        try:
            LOGGER.info('read in the arguments')
            # resources = self.getInputValues(identifier='resources')
            season = request.inputs['season'][0].data
            bbox = [-180, -90, 180, 90]
            # bbox_obj = self.BBox.getValue()
            model_var = request.inputs['reanalyses'][0].data
            model, variable = model_var.split('_')

            period = request.inputs['period'][0].data
            anualcycle = request.inputs['anualcycle'][0].data
            kappa = 4

            # kappa = request.inputs['kappa'][0].data
            LOGGER.info('period %s' % str(period))
            LOGGER.info('season %s' % str(season))
            start = dt.strptime(period.split('-')[0], '%Y%m%d')
            end = dt.strptime(period.split('-')[1], '%Y%m%d')
        except:
            LOGGER.exception('failed to read in the arguments')

        # try:
        #
        #     # if bbox_obj is not None:
        #     #     LOGGER.info("bbox_obj={0}".format(bbox_obj.coords))
        #     #     bbox = [bbox_obj.coords[0][0], bbox_obj.coords[0][1], bbox_obj.coords[1][0], bbox_obj.coords[1][1]]
        #     #     LOGGER.info("bbox={0}".format(bbox))
        #     # else:
        #     #     bbox = None
        #
        # except Exception as e:
        #     LOGGER.exception('failed to transform BBOXObject  %s ' % e)

        ###########################
        # set the environment
        ###########################

        response.update_status('fetching data from archive', 10)

        try:
            if model == 'NCEP':
                if 'z' in variable:
                    level = variable.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            elif '20CRV2' in model:
                if 'z' in variable:
                    level = variable.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            else:
                LOGGER.exception('Reanalyses dataset not known')
            LOGGER.info('environment set')
        except:
            msg = 'failed to set environment'
            LOGGER.exception(msg)
            raise Exception(msg)

        ##########################################
        # fetch Data from original data archive
        ##########################################

        from flyingpigeon.datafetch import reanalyses as rl
        try:
            model_nc = rl(start=start.year,
                          end=end.year,
                          dataset=model,
                          variable=variable)
            LOGGER.info('reanalyses data fetched')
        except:
            msg = 'failed to get reanalyses data'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.update_status('fetching data done', 15)
        ############################################################
        # get the required bbox and time region from resource data
        ############################################################

        response.update_status('subsetting region of interest', 17)
        # from flyingpigeon.weatherregimes import get_level
        from flyingpigeon.ocgis_module import call

        time_range = [start, end]
        model_subset = call(resource=model_nc, variable=variable,
                            geom=bbox, spatial_wrapping='wrap', time_range=time_range,
                            # conform_units_to=conform_units_to
                            )
        LOGGER.info('Dataset subset done: %s ' % model_subset)

        response.update_status('dataset subsetted', 19)
        ##############################################
        # computing anomalies
        ##############################################
        response.update_status('computing anomalies ', 19)

        cycst = anualcycle.split('-')[0]
        cycen = anualcycle.split('-')[0]
        reference = [dt.strptime(cycst, '%Y%m%d'), dt.strptime(cycen, '%Y%m%d')]
        LOGGER.exception('reference time: %s' % reference)
        model_anomal = wr.get_anomalies(model_subset, reference=reference)

        #####################
        # extracting season
        #####################
        response.update_status('normalizing data', 21)
        model_season = wr.get_season(model_anomal, season=season)

        response.update_status('anomalies computed and  normalized', 24)
        #######################
        # call the R scripts
        #######################
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
            modelname = model
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
                    '%s' % model_var, '%s' % kappa]
            LOGGER.info('Rcall builded')
        except:
            msg = 'failed to build the R command'
            LOGGER.exception(msg)
            raise Exception(msg)
        try:
            output, error = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            LOGGER.info('R outlog info:\n %s ' % output)
            LOGGER.exception('R outlog errors:\n %s ' % error)
            if len(output) > 0:
                response.update_status('**** weatherregime in R suceeded', 90)
            else:
                LOGGER.exception('NO! output returned from R call')
        except:
            msg = 'weatherregime in R'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.update_status('Weather regime clustering done ', 80)
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
