"""
Processes for Weather Classification
Author: Nils Hempelmann (nils.hempelmann@lsce.ipsl.fr)
"""

import logging
from datetime import datetime as dt
from os.path import abspath
from tempfile import mkstemp

from flyingpigeon import weatherregimes as wr
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import download, get_time
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.weatherregimes import _TIMEREGIONS_
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

LOGGER = logging.getLogger("PYWPS")


class WeatherregimesprojectionProcess(Process):
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

            LiteralInput("Rdat", "R - workspace",
                         abstract="R workspace as output from weather regime reference process",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("dat", "R - datafile",
                         abstract="R datafile as output from weather regime reference process",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("netCDF", "netCDF reference",
                         abstract="netCDF file as output from weather regime reference process",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         ),

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
                         ), ]
        outputs = [
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
            ComplexOutput("output_frequency", "Frequency",
                          abstract="Weather regime frequency values per year",
                          supported_formats=[Format('text/plain')],
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

        super(WeatherregimesprojectionProcess, self).__init__(
            self._handler,
            identifier="weatherregimes_projection",
            title="Weather Regimes (Projection based on precalculated statistics)",
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

        response.update_status('execution started at : {}'.format(dt.now()), 5)

        ################################
        # reading in the input arguments
        ################################
        try:
            LOGGER.info('read in the arguments')
            # resources = self.getInputValues(identifier='resources')
            season = request.inputs['season'][0].data
            LOGGER.info('season %s', season)

            period = request.inputs['period'][0].data
            LOGGER.info('period %s', period)
            anualcycle = request.inputs['anualcycle'][0].data

            start = dt.strptime(period.split('-')[0], '%Y%m%d')
            end = dt.strptime(period.split('-')[1], '%Y%m%d')
            LOGGER.debug('start: %s , end: %s ' % (start, end))

            resource = archiveextract(resource=rename_complexinputs(request.inputs['resource']))
            # resource = archiveextract(resource=[res.file for res in request.inputs['resource']])
            url_Rdat = request.inputs['Rdat'][0].data
            url_dat = request.inputs['dat'][0].data
            url_ref_file = request.inputs['netCDF'][0].data  # can be None
            # season = self.getInputValues(identifier='season')[0]
            # period = self.getInputValues(identifier='period')[0]
            # anualcycle = self.getInputValues(identifier='anualcycle')[0]
            LOGGER.info('period %s' % str(period))
            LOGGER.info('season %s' % str(season))
            LOGGER.info('read in the arguments')
            LOGGER.info('url_ref_file: %s' % url_ref_file)
            LOGGER.info('url_Rdat: %s' % url_Rdat)
            LOGGER.info('url_dat: %s' % url_dat)
        except Exception as e:
            LOGGER.debug('failed to convert arguments %s ' % e)

        ############################
        # fetching trainging data
        ############################

        try:
            dat = abspath(download(url_dat))
            Rdat = abspath(download(url_Rdat))
            LOGGER.info('training data fetched')
        except Exception as e:
            LOGGER.error('failed to fetch training data %s' % e)

        ##########################################################
        # get the required bbox and time region from resource data
        ##########################################################
        # from flyingpigeon.weatherregimes import get_level
        try:
            from flyingpigeon.ocgis_module import call
            from flyingpigeon.utils import get_variable
            time_range = [start, end]

            variable = get_variable(resource)

            if len(url_ref_file) > 0:
                ref_file = download(url_ref_file)
                model_subset = call(
                    resource=resource, variable=variable,
                    time_range=time_range,  # conform_units_to=conform_units_to, geom=bbox, spatial_wrapping='wrap',
                    regrid_destination=ref_file, regrid_options='bil')
                LOGGER.info('Dataset subset with regridding done: %s ' % model_subset)
            else:
                model_subset = call(
                    resource=resource, variable=variable,
                    time_range=time_range,  # conform_units_to=conform_units_to, geom=bbox, spatial_wrapping='wrap',
                )
                LOGGER.info('Dataset time period extracted: %s ' % model_subset)
        except:
            LOGGER.exception('failed to make a data subset ')

        #######################
        # computing anomalies
        #######################
        try:
            cycst = anualcycle.split('-')[0]
            cycen = anualcycle.split('-')[1]
            reference = [dt.strptime(cycst, '%Y%m%d'), dt.strptime(cycen, '%Y%m%d')]
            model_anomal = wr.get_anomalies(model_subset, reference=reference, sseas='multi')

            #####################
            # extracting season
            #####################

            model_season = wr.get_season(model_anomal, season=season)
        except:
            LOGGER.exception('failed to compute anualcycle or seasons')

        #######################
        # call the R scripts
        #######################

        import subprocess
        from flyingpigeon import config
        from os.path import curdir, join

        try:
            rworkspace = curdir
            Rsrc = config.Rsrc_dir()
            Rfile = 'weatherregimes_projection.R'

            yr1 = start.year
            yr2 = end.year
            time = get_time(model_season)  # , format='%Y%m%d')

            # ip, output_graphics = mkstemp(dir=curdir ,suffix='.pdf')
            ip, file_pca = mkstemp(dir=curdir, suffix='.txt')
            ip, file_class = mkstemp(dir=curdir, suffix='.Rdat')
            ip, output_frec = mkstemp(dir=curdir, suffix='.txt')

            args = ['Rscript', join(Rsrc, Rfile), '%s/' % curdir,
                    '%s/' % Rsrc,
                    '%s' % model_season,
                    '%s' % variable,
                    '%s' % str(time).strip("[]").replace("'", "").replace(" ", ""),
                    # '%s' % output_graphics,
                    '%s' % dat,
                    '%s' % Rdat,
                    '%s' % file_pca,
                    '%s' % file_class,
                    '%s' % output_frec,
                    '%s' % season,
                    '%s' % start.year,
                    '%s' % end.year,
                    '%s' % 'MODEL']

            LOGGER.info('Rcall builded')
        except Exception as e:
            msg = 'failed to build the R command %s' % e
            LOGGER.error(msg)
            raise Exception(msg)
        try:
            output, error = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            # , shell=True
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

        #################
        # set the outputs
        #################

        response.update_status('Set the process outputs ', 95)

        response.outputs['output_pca'].file = file_pca
        response.outputs['output_classification'].file = file_class
        response.outputs['output_netcdf'].file = model_season
        response.outputs['output_frequency'].file = output_frec

        response.update_status('done', 100)
        return response
