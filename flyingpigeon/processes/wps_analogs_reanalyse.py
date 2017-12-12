import os
from datetime import date
from datetime import datetime as dt
import time  # performance test
import subprocess
from subprocess import CalledProcessError
import uuid

from netCDF4 import Dataset

from numpy import squeeze

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from pywps.inout.storage import FileStorage

from flyingpigeon.datafetch import _PRESSUREDATA_
from flyingpigeon.datafetch import reanalyses as rl
from flyingpigeon.ocgis_module import call
from flyingpigeon import analogs
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import get_variable
from flyingpigeon.calculation import remove_mean_trend
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

            LiteralInput("timeres", "Reanalyses temporal resolution",
                         abstract="Temporal resolution of the reanalyses (only for 20CRV2)",
                         default="day",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['day', '6h']
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

            LiteralInput("detrend", "Detrend",
                         abstract="Remove long-term trend beforehand",
                         default='None',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['None', 'UVSpline']
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
                         abstract="Number of days before and after the date to be analysed",
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
            ComplexOutput("analog_pdf", "Maps with mean analogs and simulation",
                          abstract="Analogs Maps",
                          supported_formats=[Format('image/pdf')],
                          as_reference=True,
                          ),

            ComplexOutput("config", "Config File",
                          abstract="Config file used for the Fortran process",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput("analogs", "Analogues File",
                          abstract="mulit-column text file",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput("formated_analogs", "Formated Analogues File",
                          abstract="Formated analogues file for viewer",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for one dataset',
                          abstract="Prepared netCDF file as input for simulation (as input for weatherregime calculation)",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('target_netcdf', 'Subsets for one dataset',
                          abstract="Prepared netCDF file as input for archive",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('base_netcdf', 'Base Seasonal cycle',
                          abstract="Base seasonal cycle netCDF",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('sim_netcdf', 'Sim Seasonal cycle',
                          abstract="Sim seasonal cycle netCDF",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput("output", "Analogues Viewer html page",
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
            timres = request.inputs['timeres'][0].data

            #bbox = [-80, 20, 50, 70]
            # TODO: Add checking for wrong cordinates and apply default if nesessary
            bbox = []
            bboxStr = request.inputs['BBox'][0].data
            bboxStr = bboxStr.split(',')
            bbox.append(float(bboxStr[0]))
            bbox.append(float(bboxStr[2]))
            bbox.append(float(bboxStr[1]))
            bbox.append(float(bboxStr[3]))
            LOGGER.debug('BBOX for ocgis: %s ' % (bbox))
            LOGGER.debug('BBOX original: %s ' % (bboxStr))

            normalize = request.inputs['normalize'][0].data
            detrend = request.inputs['detrend'][0].data
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
            LOGGER.exception(msg)
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
                LOGGER.exception('output format not valid')

        except Exception as e:
            msg = 'failed to set environment %s ' % e
            LOGGER.exception(msg)
            raise Exception(msg)

        ###########################
        # set the environment
        ###########################

        response.update_status('fetching data from archive', 10)

        try:
            if model == 'NCEP':
                getlevel = False
                if 'z' in var:
                    level = var.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            elif '20CRV2' in model:
                getlevel = False
                if 'z' in var:
                    level = var.strip('z')
                    conform_units_to = None
                else:
                    level = None
                    conform_units_to = 'hPa'
            else:
                LOGGER.exception('Reanalyses dataset not known')
            LOGGER.info('environment set for model: %s' % model)
        except Exception:
            msg = 'failed to set environment'
            LOGGER.exception(msg)
            raise Exception(msg)

        ##########################################
        # fetch Data from original data archive
        ##########################################
                
        # NOTE: If ref is say 1950 - 1990, and sim is just 1 week in 2017 - ALL the data will be downloaded, 1950 - 2017 
        try:
            model_nc = rl(start=start.year,
                          end=end.year,
                          dataset=model,
                          variable=var, timres=timres, getlevel=getlevel)
            LOGGER.info('reanalyses data fetched')
        except Exception:
            msg = 'failed to get reanalyses data'
            LOGGER.exception(msg)
            raise Exception(msg)

        response.update_status('subsetting region of interest', 17)
        # from flyingpigeon.weatherregimes import get_level
        LOGGER.debug("start and end time: %s - %s" % (start, end))
        time_range = [start, end]


        # For 20CRV2 geopotential height, daily dataset for 100 years is about 50 Gb
        # So it makes sense, to operate it step-by-step
        # TODO: need to create dictionary for such datasets (for models as well)
        # TODO: benchmark the method bellow for NCEP z500 for 60 years

#        if ('20CRV2' in model) and ('z' in var):
        if ('z' in var):  
            tmp_total = []
            origvar = get_variable(model_nc)

            for z in model_nc:
                tmp_n = 'tmp_%s' % (uuid.uuid1()) 
                b0=call(resource=z, variable=origvar, level_range=[int(level), int(level)], geom=bbox,
                spatial_wrapping='wrap', prefix='levdom_'+os.path.basename(z)[0:-3])
                tmp_total.append(b0)

            tmp_total = sorted(tmp_total, key=lambda i: os.path.splitext(os.path.basename(i))[0])
            inter_subset_tmp = call(resource=tmp_total, variable=origvar, time_range=time_range)

            # Clean
            for i in tmp_total:
                tbr='rm -f %s' % (i) 
                os.system(tbr)  

            # Create new variable
            ds = Dataset(inter_subset_tmp, mode='a')
            z_var = ds.variables.pop(origvar)
            dims = z_var.dimensions
            new_var = ds.createVariable('z%s' % level, z_var.dtype, dimensions=(dims[0], dims[2], dims[3]))
            new_var[:, :, :] = squeeze(z_var[:, 0, :, :])
            # new_var.setncatts({k: z_var.getncattr(k) for k in z_var.ncattrs()})
            ds.close()
            model_subset_tmp = call(inter_subset_tmp, variable='z%s' % level)
        else:
            model_subset_tmp = call(resource=model_nc, variable=var,
                                    geom=bbox, spatial_wrapping='wrap', time_range=time_range,
                                    # conform_units_to=conform_units_to
                                    )

        # If dataset is 20CRV2 the 6 hourly file should be converted to daily.  
        # Option to use previously 6h data from cache (if any) and not download daily files.

        if '20CRV2' in model:
            if timres == '6h':
                from cdo import Cdo
                
                cdo = Cdo()
                model_subset = '%s.nc' % uuid.uuid1()
                tmp_f = '%s.nc' % uuid.uuid1()

                cdo_op = getattr(cdo,'daymean')
                cdo_op(input=model_subset_tmp, output=tmp_f)
                sti = '00:00:00' 
                cdo_op = getattr(cdo,'settime')
                cdo_op(sti, input=tmp_f, output=model_subset)
                LOGGER.debug('File Converted from: %s to daily' % (timres))
            else:
                model_subset = model_subset_tmp
        else:
            model_subset = model_subset_tmp

        LOGGER.info('Dataset subset done: %s ', model_subset)

        response.update_status('dataset subsetted', 19)

        # BLOCK OF DETRENDING of model_subset !
        # Original model subset kept to further visualisaion if needed
        # Now is issue with SLP:
        # TODO 1 Keep trend as separate file
        # TODO 2 Think how to add options to plot abomalies AND original data... 
        #        May be do archive and simulation = call.. over NOT detrended data and keep it as well
        # TODO 3 Check with faster smoother add removing trend of each grid

        if detrend == 'None':
            orig_model_subset = model_subset            
        else:
            orig_model_subset = remove_mean_trend(model_subset, varname=var)

        # ======================================

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
        #         LOGGER.exception('Reanalyses dataset not known')
        #     LOGGER.info('environment set')
        # except Exception as e:
        #     msg = 'failed to set environment %s ' % e
        #     LOGGER.exception(msg)
        #     # raise Exception(msg)
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
        #     LOGGER.exception(msg)
        #     # raise Exception(msg)

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
            # TODO check strftime for years <1900 (!)

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
            LOGGER.exception(msg)
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
            LOGGER.exception(msg)
            raise Exception(msg)

        output_file = 'output.txt'
        files = [os.path.abspath(archive), os.path.abspath(simulation), output_file]
        LOGGER.debug("Data preperation took %s seconds.",
                     time.time() - start_time)

        ############################
        # generate the config file
        ############################
        config_file = analogs.get_configfile(
            files=files,
            seasoncyc_base=seasoncyc_base,
            seasoncyc_sim=seasoncyc_sim,
            base_id=model,
            sim_id=model,
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
            period=[dt.strftime(refSt, '%Y-%m-%d'), dt.strftime(refEn, '%Y-%m-%d')],
            bbox="{0[0]},{0[2]},{0[1]},{0[3]}".format(bbox))
        response.update_status('generated config file', 15)
        #######################
        # CASTf90 call
        #######################
        start_time = time.time()  # measure call castf90

        #-----------------------
        try:
            import ctypes
            # TODO: This lib is for linux
            mkl_rt = ctypes.CDLL('libmkl_rt.so')
            nth=mkl_rt.mkl_get_max_threads()
            LOGGER.debug('Current number of threads: %s' % (nth))
            mkl_rt.mkl_set_num_threads(ctypes.byref(ctypes.c_int(64)))
            nth=mkl_rt.mkl_get_max_threads()
            LOGGER.debug('NEW number of threads: %s' % (nth))
            # TODO: Does it \/\/\/ work with default shell=False in subprocess... (?)
            os.environ['MKL_NUM_THREADS']=str(nth)
            os.environ['OMP_NUM_THREADS']=str(nth)
        except Exception as e:
            msg = 'Failed to set THREADS %s ' % e
            LOGGER.debug(msg)
        #-----------------------

        response.update_status('Start CASTf90 call', 20)
        try:
            # response.update_status('execution of CASTf90', 50)
            cmd = ['analogue.out', config_file]
            LOGGER.debug("castf90 command: %s", cmd)
            output = subprocess.check_output(cmd, stderr=subprocess.STDOUT)
            LOGGER.info('analogue output:\n %s', output)
            response.update_status('**** CASTf90 suceeded', 70)
        except CalledProcessError as e:
            msg = 'CASTf90 failed:\n{0}'.format(e.output)
            LOGGER.exception(msg)
            raise Exception(msg)
        LOGGER.debug("castf90 took %s seconds.", time.time() - start_time)

        # TODO: Add try - except for pdfs
        analogs_pdf = analogs.plot_analogs(configfile=config_file)   
        response.update_status('preparing output', 70)
        # response.outputs['config'].storage = FileStorage()
        response.outputs['analog_pdf'].file = analogs_pdf 
        response.outputs['config'].file = config_file
        response.outputs['analogs'].file = output_file
        response.outputs['output_netcdf'].file = simulation
        response.outputs['target_netcdf'].file = archive

        if seacyc is True:
            response.outputs['base_netcdf'].file = seasoncyc_base
            response.outputs['sim_netcdf'].file = seasoncyc_sim
        else:
            # TODO: Still unclear how to overpass unknown number of outputs
            dummy_base='dummy_base.nc'
            dummy_sim='dummy_sim.nc'
            with open(dummy_base, 'a'): os.utime(dummy_base, None)
            with open(dummy_sim, 'a'): os.utime(dummy_sim, None)
            response.outputs['base_netcdf'].file = dummy_base
            response.outputs['sim_netcdf'].file = dummy_sim

        ########################
        # generate analog viewer
        ########################

        formated_analogs_file = analogs.reformat_analogs(output_file)
        # response.outputs['formated_analogs'].storage = FileStorage()
        response.outputs['formated_analogs'].file = formated_analogs_file
        LOGGER.info('analogs reformated')
        response.update_status('reformatted analog file', 80)

        viewer_html = analogs.render_viewer(
            # configfile=response.outputs['config'].get_url(),
            configfile=config_file,
            # datafile=response.outputs['formated_analogs'].get_url())
            datafile=formated_analogs_file)
        response.outputs['output'].file = viewer_html
        response.update_status('Successfully generated analogs viewer', 90)
        LOGGER.info('rendered pages: %s ', viewer_html)

        response.update_status('execution ended', 100)
        LOGGER.debug("total execution took %s seconds.",
                     time.time() - process_start_time)
        return response
