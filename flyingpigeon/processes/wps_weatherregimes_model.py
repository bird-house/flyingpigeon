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

from datetime import datetime as dt
from datetime import time as dt_time
from flyingpigeon import weatherregimes as wr
from flyingpigeon.utils import archive, archiveextract, get_calendar
from tempfile import mkstemp
from os import path

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

            LiteralInput('BBox', 'Bounding Box',
                         data_type='string',
                         abstract="Enter a bbox: min_lon, max_lon, min_lat, max_lat."
                            " min_lon=Western longitude,"
                            " max_lon=Eastern longitude,"
                            " min_lat=Southern or northern latitude,"
                            " max_lat=Northern or southern latitude."
                            " For example: -80,50,20,70",
                         min_occurs=1,
                         max_occurs=1,
                         default='-80,50,20,70',
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

            LiteralInput("method", "Method of annual cycle calculation",
                         abstract="Method of annual cycle calculation",
                         default="cdo",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['ocgis', 'cdo']
                         ),

            LiteralInput("sseas", "Serial or multiprocessing for annual cycle",
                         abstract="Serial or multiprocessing for annual cycle",
                         default="multi",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['serial', 'multi']
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

            # If files are from different datasets.
            # i.e. files: ...output1/slp.1999.nc and ...output2/slp.1997.nc will not be sorted with just .sort()
            # So:
            if type(resource) == list:
                resource = sorted(resource, key=lambda i: path.splitext(path.basename(i))[0])
            else:
                resource=[resource]

            # resources = self.getInputValues(identifier='resources')
            season = request.inputs['season'][0].data
            LOGGER.info('season %s', season)

            # if 'bbox' in request.inputs:
            #    bbox = request.inputs['bbox'][0].data
            #    bbox = [-80, 20, 50, 70]
            # else:
            #    bbox = [-80, 20, 50, 70]

            bbox = []
            bboxStr = request.inputs['BBox'][0].data
            bboxStr = bboxStr.split(',')
            bbox.append(float(bboxStr[0]))
            bbox.append(float(bboxStr[2]))
            bbox.append(float(bboxStr[1]))
            bbox.append(float(bboxStr[3]))
            LOGGER.debug('BBOX for ocgis: %s ' % (bbox))
            LOGGER.debug('BBOX original: %s ' % (bboxStr))

            period = request.inputs['period'][0].data
            LOGGER.info('period %s', period)
            anualcycle = request.inputs['anualcycle'][0].data
            kappa = request.inputs['kappa'][0].data
            LOGGER.info('kappa %s', kappa)

            method = request.inputs['method'][0].data
            LOGGER.info('Calc annual cycle with %s', method)

            sseas = request.inputs['sseas'][0].data
            LOGGER.info('Annual cycle calc with %s', sseas)

            start = dt.strptime(period.split('-')[0], '%Y%m%d')
            end = dt.strptime(period.split('-')[1], '%Y%m%d')

            # OCGIS for models workaround - to catch 31 of Dec
            start = dt.combine(start, dt_time(12,0))
            end = dt.combine(end, dt_time(12,0))

            cycst = anualcycle.split('-')[0]
            cycen = anualcycle.split('-')[1]
            reference = [dt.strptime(cycst, '%Y%m%d'), dt.strptime(cycen, '%Y%m%d')]
            LOGGER.debug('Reference start: %s , end: %s ' % (reference[0], reference[1]))

            reference[0] = dt.combine(reference[0],dt_time(12,0))
            reference[1] = dt.combine(reference[1],dt_time(12,0))
            LOGGER.debug('New Reference start: %s , end: %s ' % (reference[0], reference[1]))

            # Check if 360_day calendar (all months are exactly 30 days):
            try:
                if type(resource) is not list: resource=[resource]
                modcal, calunits = get_calendar(resource[0])
                if '360_day' in modcal:
                    if start.day == 31:
                        start = start.replace(day=30)
                        LOGGER.debug('Date has been changed for: %s' % (start))
                    if end.day == 31:
                        end = end.replace(day=30)
                        LOGGER.debug('Date has been changed for: %s' % (end))
                    if reference[0].day == 31:
                        reference[0] = reference[0].replace(day=30)
                        LOGGER.debug('Date has been changed for: %s' % (reference[0]))
                    if reference[1].day == 31:
                        reference[1] = reference[1].replace(day=30)
                        LOGGER.debug('Date has been changed for: %s' % (reference[1]))
            except:
                LOGGER.debug('Could not detect calendar')

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
        from flyingpigeon.utils import get_variable, get_timerange
        time_range = [start, end]

        tmp_resource = []
        for re in resource:
            s,e = get_timerange(re)
            tmpSt = dt.strptime(s,'%Y%m%d')
            tmpEn = dt.strptime(e,'%Y%m%d')
            if ((tmpSt <= end ) and (tmpEn >= start)):
                tmp_resource.append(re)
                LOGGER.debug('Selected file: %s ' % (re))
        resource = tmp_resource

        # Here start trick with z... levels and regriding...
        # Otherwise call will give memory error for hires models with geop
        # TODO: Add level and domain selection as in wps_analogs_model for 4D var.

        variable = get_variable(resource)
        model_subset = call(
            resource=resource, variable=variable,
            geom=bbox, spatial_wrapping='wrap', time_range=time_range,  # conform_units_to=conform_units_to
        )
        LOGGER.info('Dataset subset done: %s ' % model_subset)
        response.update_status('dataset subsetted', 18)

        #####################
        # computing anomalies
        #####################

        response.update_status('computing anomalies ', 19)

        model_anomal = wr.get_anomalies(model_subset, reference=reference, method=method, sseas=sseas)

        ###################
        # extracting season
        ####################
        model_season = wr.get_season(model_anomal, season=season)
        response.update_status('values normalized', 20)

        ####################
        # call the R scripts
        ####################
        response.update_status('Start weather regime clustering ', 50)
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
            LOGGER.debug('ARGS: %s'%(args))
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

        response.update_status('Weather regime clustering done ', 92)
        ############################################
        # set the outputs
        ############################################
        response.update_status('Set the process outputs ', 95)
        #bla=bla
        response.outputs['Routput_graphic'].file = output_graphics
        response.outputs['output_pca'].file = file_pca
        response.outputs['output_classification'].file = file_class
        response.outputs['output_netcdf'].file = model_season
        response.update_status('done', 100)
        return response
