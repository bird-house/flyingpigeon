from datetime import date
from datetime import datetime as dt
import time  # performance test
from tempfile import mkstemp
from os import path

from flyingpigeon.datafetch import _PRESSUREDATA_
from flyingpigeon.datafetch import reanalyses as rl
from flyingpigeon.ocgis_module import call
from flyingpigeon import analogs
from flyingpigeon.utils import rename_complexinputs

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class AnalogsreanalyseProcess(Process):
    def __init__(self):
        inputs = [

            LiteralInput("reanalyses", "Reanalyses Data",
                         abstract="Choose a reanalyses dataset for comparison",
                         default="NCEP_slp",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=_PRESSUREDATA_
                         ),

            # self.BBox = self.addBBoxInput(
            #   identifier="BBox",
            #   title="Bounding Box",
            #   abstract="coordinates to define the region to be analysed",
            #   minOccurs=1,
            #   maxOccurs=1,
            #   crss=['EPSG:4326']
            #   )

            LiteralInput('dateSt', 'Start date of analysis period',
                         data_type='date',
                         abstract='First day of the period to be analysed',
                         default='2013-07-15',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput('dateEn', 'End date of analysis period',
                         data_type='date',
                         abstract='Last day of the period to be analysed',
                         default='2013-12-31',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput('refSt', 'Start date of reference period',
                         data_type='date',
                         abstract='First day of the period where analogues being picked',
                         default='2013-01-01',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput('refEn', 'End date of reference period',
                         data_type='date',
                         abstract='Last day of the period where analogues being picked',
                         default='2014-12-31',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput("normalize", "normalization",
                         abstract="Normalize by subtraction of annual cycle",
                         default='base',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['None', 'base', 'sim', 'own']
                         ),

            LiteralInput("seasonwin", "Seasonal window",
                         abstract="Number of days befor and after the date to be analysed",
                         default='30',
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("nanalog", "Nr of analogues",
                         abstract="Number of analogues to be detected",
                         default='20',
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("dist", "Distance",
                         abstract="Distance function to define analogues",
                         default='euclidean',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['euclidean', 'mahalanobis', 'cosine', 'of']
                         ),

            LiteralInput("outformat", "output file format",
                         abstract="Choose the format for the analogue output file",
                         default="ascii",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['ascii', 'netCDF4']
                         ),

            LiteralInput("timewin", "Time window",
                         abstract="Number of days following the analogue day the distance will be averaged",
                         default='1',
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         ),
        ]

        outputs = [
            LiteralOutput("config", "Config File",
                          abstract="Config file used for the Fortran process",
                          data_type='string',
                          ),

            ComplexOutput("analogs", "Analogues File",
                          abstract="mulit-column text file",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for one dataset',
                          abstract="Prepared netCDF file as input for weatherregime calculation",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput("output_html", "Analogues Viewer html page",
                          abstract="Interactive visualization of calculated analogues",
                          supported_formats=[Format("text/html")],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]

        super(AnalogsreanalyseProcess, self).__init__(
            self._handler,
            identifier="analogs_reanalyse",
            title="Analogues of circulation (based on reanalyses data)",
            abstract='Search for days with analogue pressure pattern for reanalyses data sets',
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
        response.update_status('execution started at : {}'.format(dt.now()), 5)

        process_start_time = time.time()  # measure process execution time ...
        start_time = time.time()  # measure init ...

        ################################
        # reading in the input arguments
        ################################

        try:
            response.update_status('read input parameter : %s ' % dt.now(), 5)

            refSt = request.inputs['refSt'][0].data
            refEn = request.inputs['refEn'][0].data
            dateSt = request.inputs['dateSt'][0].data
            dateEn = request.inputs['dateEn'][0].data
            seasonwin = request.inputs['seasonwin'][0].data
            nanalog = request.inputs['nanalog'][0].data
            bbox = [-80, 20, 50, 70]
            # if bbox_obj is not None:
            #     LOGGER.info("bbox_obj={0}".format(bbox_obj.coords))
            #     bbox = [bbox_obj.coords[0][0],
            #             bbox_obj.coords[0][1],
            #             bbox_obj.coords[1][0],
            #             bbox_obj.coords[1][1]]
            #     LOGGER.info("bbox={0}".format(bbox))
            # else:
            #     bbox = None
            # region = self.getInputValues(identifier='region')[0]
            # bbox = [float(b) for b in region.split(',')]
            # bbox_obj = self.BBox.getValue()

            normalize = request.inputs['normalize'][0].data
            distance = request.inputs['dist'][0].data
            outformat = request.inputs['outformat'][0].data
            timewin = request.inputs['timewin'][0].data

            model_var = request.inputs['reanalyses'][0].data
            model, var = model_var.split('_')

            # experiment = self.getInputValues(identifier='experiment')[0]
            # dataset, var = experiment.split('_')
            # LOGGER.info('environment set')
            LOGGER.info('input parameters set')
            response.update_status('Read in and convert the arguments', 5)
        except Exception as e:
            msg = 'failed to read input prameter %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)

        ######################################
        # convert types and set environment
        ######################################
        try:
            response.update_status('Preparing enviroment converting arguments', 7)
            LOGGER.debug('date: %s %s %s %s ' % (type(refSt), refEn, dateSt, dateSt))

            start = min(refSt, dateSt)
            end = max(refEn, dateEn)

            #
            # refSt = dt.strftime(refSt, '%Y-%m-%d')
            # refEn = dt.strftime(refEn, '%Y-%m-%d')
            # dateSt = dt.strftime(dateSt, '%Y-%m-%d')
            # dateEn = dt.strftime(dateEn, '%Y-%m-%d')

            if normalize == 'None':
                seacyc = False
            else:
                seacyc = True

            if outformat == 'ascii':
                outformat = '.txt'
            elif outformat == 'netCDF':
                outformat = '.nc'
            else:
                LOGGER.error('output format not valid')


        except Exception as e:
            msg = 'failed to set environment %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)

        ###########################
        # set the environment
        ###########################

        response.update_status('fetching data from archive', 10)

        try:
            if model == 'NCEP':
                if 'z' in var:
                    level = var.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            elif '20CRV2' in model:
                if 'z' in var:
                    level = var.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            else:
                LOGGER.exception('Reanalyses dataset not known')
            LOGGER.info('environment set for model: %s' % model)
        except:
            msg = 'failed to set environment'
            LOGGER.exception(msg)
            raise Exception(msg)

        ##########################################
        # fetch Data from original data archive
        ##########################################

        try:
            model_nc = rl(start=start.year,
                          end=end.year,
                          dataset=model,
                          variable=var)
            LOGGER.info('reanalyses data fetched')
        except:
            msg = 'failed to get reanalyses data'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.update_status('subsetting region of interest', 17)
        # from flyingpigeon.weatherregimes import get_level
        LOGGER.debug("start and end time: %s - %s" % (start, end))
        time_range = [start, end]

        model_subset = call(resource=model_nc, variable=var,
                            geom=bbox, spatial_wrapping='wrap', time_range=time_range,
                            # conform_units_to=conform_units_to
                            )
        LOGGER.info('Dataset subset done: %s ', model_subset)

        response.update_status('dataset subsetted', 19)

        ############################################################
        #  get the required bbox and time region from resource data
        ############################################################
        #
        #
        # try:
        #     if dataset == 'NCEP':
        #         if 'z' in var:
        #             variable = 'hgt'
        #             level = var.strip('z')
        #             # conform_units_to=None
        #         else:
        #             variable = 'slp'
        #             level = None
        #             # conform_units_to='hPa'
        #     elif '20CRV2' in var:
        #         if 'z' in level:
        #             variable = 'hgt'
        #             level = var.strip('z')
        #             # conform_units_to=None
        #         else:
        #             variable = 'prmsl'
        #             level = None
        #             # conform_units_to='hPa'
        #     else:
        #         LOGGER.error('Reanalyses dataset not known')
        #     LOGGER.info('environment set')
        # except Exception as e:
        #     msg = 'failed to set environment %s ' % e
        #     LOGGER.error(msg)
        #     raise Exception(msg)
        #
        # LOGGER.debug("init took %s seconds.", time.time() - start_time)
        # response.update_status('Read in and convert the arguments done', 8)
        #
        # #################
        # # get input data
        # #################
        # start_time = time.time()  # measure get_input_data ...
        # response.update_status('fetching input data', 7)
        # try:
        #     input = reanalyses(start=start.year, end=end.year,
        #                        variable=var, dataset=dataset)
        #     LOGGER.info('input files %s' % input)
        #     nc_subset = call(resource=input, variable=var,
        #                      geom=bbox, spatial_wrapping='wrap')
        # except Exception as e:
        #     msg = 'failed to fetch or subset input files %s' % e
        #     LOGGER.error(msg)
        #     raise Exception(msg)

        LOGGER.debug("get_input_subset_dataset took %s seconds.",
                     time.time() - start_time)
        response.update_status('**** Input data fetched', 10)

        ########################
        # input data preperation
        ########################
        response.update_status('Start preparing input data', 12)
        start_time = time.time()  # measure data preperation ...

        try:
            # Construct descriptive filenames for the three files
            # listed in config file
            refDatesString = dt.strftime(refSt, '%Y-%m-%d') + "_" + dt.strftime(refEn, '%Y-%m-%d')
            simDatesString = dt.strftime(dateSt, '%Y-%m-%d') + "_" + dt.strftime(dateEn, '%Y-%m-%d')
            archiveNameString = "base_" + var + "_" + refDatesString + '_%.1f_%.1f_%.1f_%.1f' \
                                % (bbox[0], bbox[2], bbox[1], bbox[3])
            simNameString = "sim_" + var + "_" + simDatesString + '_%.1f_%.1f_%.1f_%.1f' \
                            % (bbox[0], bbox[2], bbox[1], bbox[3])
            archive = call(resource=model_subset,
                           time_range=[refSt, refEn],
                           prefix=archiveNameString)
            simulation = call(resource=model_subset, time_range=[dateSt, dateEn],
                              prefix=simNameString)
            LOGGER.info('archive and simulation files generated: %s, %s'
                        % (archive, simulation))
        except Exception as e:
            msg = 'failed to prepare archive and simulation files %s ' % e
            LOGGER.debug(msg)
            raise Exception(msg)

        try:
            if seacyc is True:
                LOGGER.info('normalization function with method: %s '
                            % normalize)
                seasoncyc_base, seasoncyc_sim = analogs.seacyc(
                    archive,
                    simulation,
                    method=normalize)
            else:
                seasoncyc_base = seasoncyc_sim = None
        except Exception as e:
            msg = 'failed to generate normalization files %s ' % e
            LOGGER.debug(msg)
            raise Exception(msg)

        ip, output_file = mkstemp(dir='.', suffix='.txt')
        files = [path.abspath(archive), path.abspath(simulation), output_file]
        LOGGER.debug("Data preperation took %s seconds.",
                     time.time() - start_time)

        ############################
        # generate the config file
        ############################
        response.update_status('writing config file', 15)
        start_time = time.time()  # measure write config ...
        try:
            config_file = analogs.get_configfile(
                files=files,
                seasoncyc_base=seasoncyc_base,
                seasoncyc_sim=seasoncyc_sim,
                timewin=timewin,
                varname=var,
                seacyc=seacyc,
                cycsmooth=91,
                nanalog=nanalog,
                seasonwin=seasonwin,
                distfun=distance,
                outformat=outformat,
                calccor=True,
                silent=False,
                period=[dt.strftime(refSt, '%Y-%m-%d'),
                        dt.strftime(refEn, '%Y-%m-%d')],
                bbox="%s,%s,%s,%s" % (bbox[0],
                                      bbox[2],
                                      bbox[1],
                                      bbox[3]))
        except Exception as e:
            msg = 'failed to generate config file %s ' % e
            LOGGER.debug(msg)
            raise Exception(msg)
        LOGGER.debug("write_config took %s seconds.", time.time() - start_time)
        #######################
        # CASTf90 call
        #######################
        import subprocess
        import shlex

        start_time = time.time()  # measure call castf90

        response.update_status('Start CASTf90 call', 20)
        try:
            # response.update_status('execution of CASTf90', 50)
            cmd = 'analogue.out %s' % path.relpath(config_file)
            # system(cmd)
            args = shlex.split(cmd)
            output, error = subprocess.Popen(
                args, stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
            ).communicate()
            LOGGER.info('analogue.out info:\n %s ' % output)
            LOGGER.debug('analogue.out errors:\n %s ' % error)
            response.update_status('**** CASTf90 suceeded', 90)
        except Exception as e:
            msg = 'CASTf90 failed %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)
        LOGGER.debug("castf90 took %s seconds.", time.time() - start_time)

        ########################
        # generate analog viewer
        ########################

        try:
            f = analogs.reformat_analogs(output_file)
            LOGGER.info('analogs reformated')
            response.update_status('Successfully reformatted analog file', 50)

            # put config file into output folder
            config_output_path, config_output_url = analogs.copy_configfile(
                config_file
            )
            output_av = analogs.get_viewer(
                f,
                path.basename(config_output_path))
            LOGGER.info('Viewer generated')
            response.update_status('Successfully generated analogs viewer', 90)
            LOGGER.info('output_av: %s ' % output_av)
        except Exception as e:
            msg = 'Failed to reformat analogs file or generate viewer%s ' % e
            LOGGER.debug(msg)

        response.update_status('preparting output', 99)
        response.outputs['config'].data = config_output_url  # config_file )
        response.outputs['analogs'].file = output_file
        response.outputs['output_netcdf'].file = simulation
        response.outputs['output_html'].file = output_av.name
        response.update_status('execution ended', 100)
        LOGGER.debug("total execution took %s seconds.",
                     time.time() - process_start_time)
        return response
