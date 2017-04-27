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
            #     default='-80,22.5,50,70', #  cdo syntax: 'minlon,maxlon,minlat,maxlat' ;
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


        super(WeatherregimesreanalyseProcess, self).__init__(
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



        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=5000,
            formats=[{"mimeType": "application/x-netcdf"}],
            )

        self.Rdat = self.addLiteralInput(
            identifier="Rdat",
            title="R - workspace",
            abstract="R workspace as output from weather regime reference process",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
            )

        self.dat = self.addLiteralInput(
            identifier="dat",
            title="R - datafile",
            abstract="R datafile as output from weather regime reference process",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
            )

        self.netCDF = self.addLiteralInput(
            identifier="netCDF",
            title="netCDF reference",
            abstract="netCDF file as output from weather regime reference process",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            # default=' http://api.gbif.org/v1/occurrence/download/request/0013848-160118175350007.zip'
            # maxmegabites=50,
            # formats=[{"mimeType":"application/zip"}],
            )

        self.season = self.addLiteralInput(
            identifier="season",
            title="Time region",
            abstract="Select the months to define the time region (all == whole year will be analysed)",
            default="DJF",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=_TIMEREGIONS_.keys()
            )

        self.period = self.addLiteralInput(
            identifier="period",
            title="Period for weather regime calculation",
            abstract="Period for analysing the dataset",
            default="19700101-20101231",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        self.anualcycle = self.addLiteralInput(
            identifier="anualcycle",
            title="Period for annual cycle calculation",
            abstract="Period for annual cycle calculation",
            default="19700101-19991231",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            )

        ####################
        # define the outputs
        ####################

        # self.Routput_graphic = self.addComplexOutput(
        #     identifier="Routput_graphic",
        #     title="Graphics and Tables",
        #     abstract="Weather classification pressure map and frequency table",
        #     formats=[{"mimeType":"image/pdf"}],
        #     asReference=True,
        #     )

        self.output_pca = self.addComplexOutput(
            identifier="output_pca",
            title="PCA",
            abstract="Principal components",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
            )

        self.output_classification = self.addComplexOutput(
            identifier="output_classification",
            title="classification",
            abstract="Weather regime classification",
            formats=[{"mimeType": "application/octet-stream"}],
            asReference=True,
            )

        self.output_frequency = self.addComplexOutput(
            identifier="output_frequency",
            title="Frequency",
            abstract="Weather regime frequency values per year",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
            )

        self.output_netcdf = self.addComplexOutput(
            identifier="output_netcdf",
            title="netCDF file",
            abstract="Prepared netCDF file as input for weather regime calculation",
            formats=[{"mimeType": "application/x-netcdf"}],
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
        LOGGER.info('Start process')

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        from datetime import datetime as dt
        from flyingpigeon import weatherregimes as wr
        from tempfile import mkstemp

        ################################
        # reading in the input arguments
        ################################
        try:
            resource = self.getInputValues(identifier='resource')
            url_Rdat = self.getInputValues(identifier='Rdat')[0]
            url_dat = self.getInputValues(identifier='dat')[0]
            url_ref_file = self.getInputValues(identifier='netCDF')  # can be None
            season = self.getInputValues(identifier='season')[0]
            period = self.getInputValues(identifier='period')[0]
            anualcycle = self.getInputValues(identifier='anualcycle')[0]
        except Exception as e:
            LOGGER.debug('failed to read in the arguments %s ' % e)

        try:
            start = dt.strptime(period.split('-')[0], '%Y%m%d')
            end = dt.strptime(period.split('-')[1], '%Y%m%d')
            # kappa = int(self.getInputValues(identifier='kappa')[0])

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

        from flyingpigeon.utils import download, get_time
        from os.path import abspath

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

        from flyingpigeon.ocgis_module import call
        from flyingpigeon.utils import get_variable
        time_range = [start, end]

        variable = get_variable(resource)

        if len(url_ref_file) > 0:
            ref_file = download(url_ref_file[0])
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

        #######################
        # computing anomalies
        #######################

        cycst = anualcycle.split('-')[0]
        cycen = anualcycle.split('-')[0]
        reference = [dt.strptime(cycst, '%Y%m%d'), dt.strptime(cycen, '%Y%m%d')]
        model_anomal = wr.get_anomalies(model_subset, reference=reference)

        #####################
        # extracting season
        #####################

        model_season = wr.get_season(model_anomal, season=season)

        #######################
        # call the R scripts
        #######################

        import shlex
        import subprocess
        from flyingpigeon import config
        from os.path import curdir, exists, join

        try:
            rworkspace = curdir
            Rsrc = config.Rsrc_dir()
            Rfile = 'weatherregimes_projection.R'

            yr1 = start.year
            yr2 = end.year
            time = get_time(model_season, format='%Y%m%d')

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
                self.status.set('**** weatherregime in R suceeded', 90)
            else:
                LOGGER.error('NO! output returned from R call')
        except Exception as e:
            msg = 'weatherregime in R %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)

        #################
        # set the outputs
        #################

        # self.Routput_graphic.setValue( output_graphics )
        self.output_pca.setValue(file_pca)
        self.output_classification.setValue(file_class)
        self.output_netcdf.setValue(model_season)
        self.output_frequency.setValue(output_frec)
