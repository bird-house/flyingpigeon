from datetime import date
from datetime import datetime as dt
from datetime import time as dt_time
import time  # performance test
from os import path
from tempfile import mkstemp

from flyingpigeon import analogs
from flyingpigeon.ocgis_module import call
from flyingpigeon.datafetch import reanalyses
from flyingpigeon.utils import get_variable, rename_variable
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract
from flyingpigeon.log import init_process_logger
from flyingpigeon.datafetch import _PRESSUREDATA_

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class AnalogscompareProcess(Process):
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

            LiteralInput("reanalyses", "Reanalyses Data",
                         abstract="Choose a reanalyses model for comparison",
                         default="NCEP_slp",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=_PRESSUREDATA_
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

            LiteralInput("normalize", "normalization",
                         abstract="Normalize by subtraction of annual cycle",
                         default='None',
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

            LiteralInput("direction", "Comparison direction",
                         abstract="Compare direction. Pick analog days in Modeldata for a simulation period in Reanalyses data \
                                    (re2mo) or vice versa",
                         default='re2mo',
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         allowed_values=['mo2re', 're2mo']
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

            ComplexOutput("formated_analogs", "Formated Analogues File",
                          abstract="Formated analogues file for viewer",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for model',
                          abstract="Prepared netCDF file as input for weatherregime calculation",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput("target_netcdf", 'subset of ref file',
                          abstract="File with subset and normaized values of target model",
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

        super(AnalogscompareProcess, self).__init__(
            self._handler,
            identifier="analogs_compare",
            title="Analogues of circulation (based on reanalyses data and climate model data)",
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
        process_start_time = time.time()  # measure process execution time ...

        response.update_status('execution started at : %s ' % dt.now(), 5)

        start_time = time.time()  # measure init ...

        resource = archiveextract(resource=rename_complexinputs(request.inputs['resource']))

        refSt = request.inputs['refSt'][0].data
        refEn = request.inputs['refEn'][0].data
        dateSt = request.inputs['dateSt'][0].data
        dateEn = request.inputs['dateEn'][0].data

        # fix 31 December issue
        refSt = dt.combine(refSt,dt_time(12,0))
        refEn = dt.combine(refEn,dt_time(12,0))
        dateSt = dt.combine(dateSt,dt_time(12,0))
        dateEn = dt.combine(dateEn,dt_time(12,0))

        seasonwin = request.inputs['seasonwin'][0].data
        nanalog = request.inputs['nanalog'][0].data
        # bbox = [-80, 20, 50, 70]
        # TODO: Add checking for wrong cordinates and apply default if nesessary
        bbox=[]
        bboxStr = request.inputs['BBox'][0].data
        bboxStr = bboxStr.split(',')
        bbox.append(float(bboxStr[0]))
        bbox.append(float(bboxStr[2]))
        bbox.append(float(bboxStr[1]))
        bbox.append(float(bboxStr[3]))

        direction = request.inputs['direction'][0].data
        normalize = request.inputs['normalize'][0].data
        distance = request.inputs['dist'][0].data
        outformat = request.inputs['outformat'][0].data
        timewin = request.inputs['timewin'][0].data

        model_var = request.inputs['reanalyses'][0].data
        model, var = model_var.split('_')

        try:
            if direction == 're2mo':
                anaSt = dateSt #dt.strptime(dateSt[0], '%Y-%m-%d')
                anaEn = dateEn #dt.strptime(dateEn[0], '%Y-%m-%d')
                refSt = refSt #dt.strptime(refSt[0], '%Y-%m-%d')
                refEn = refEn #dt.strptime(refEn[0], '%Y-%m-%d')
            elif direction == 'mo2re':
                anaSt = refSt #dt.strptime(refSt[0], '%Y-%m-%d')
                anaEn = refEn #dt.strptime(refEn[0], '%Y-%m-%d')
                refSt = dateSt #dt.strptime(dateSt[0], '%Y-%m-%d')
                refEn = dateEn #dt.strptime(dateEn[0], '%Y-%m-%d')
            else:
                LOGGER.exception('failed to find time periods for comparison direction')
        except:
            msg = 'failed to put simulation and reference time in order'
            LOGGER.exception(msg)
            raise Exception(msg)

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

        try:
            if model == 'NCEP':
                if 'z' in var:
                    variable = 'hgt'
                    level = var.strip('z')
                    # conform_units_to='hPa'
                else:
                    variable = 'slp'
                    level = None
                    # conform_units_to='hPa'
            elif '20CRV2' in var:
                if 'z' in level:
                    variable = 'hgt'
                    level = var.strip('z')
                    # conform_units_to=None
                else:
                    variable = 'prmsl'
                    level = None
                    # conform_units_to='hPa'
            else:
                LOGGER.exception('Reanalyses model not known')
            LOGGER.info('environment set')
        except:
            msg = 'failed to set environment'
            LOGGER.exception(msg)
            raise Exception(msg)

        # LOGGER.exception("init took %s seconds.", time.time() - start_time)
        response.update_status('Read in the arguments', 5)

        #################
        # get input data
        #################
        start_time = time.time()  # measure get_input_data ...
        response.update_status('fetching input data', 7)
        try:
            nc_reanalyses = reanalyses(start=anaSt.year, end=anaEn.year,
                                       variable=var, dataset=model)
            nc_subset = call(resource=nc_reanalyses, variable=var, geom=bbox, spatial_wrapping='wrap') # XXXXXX wrap
            # LOGGER.exception("get_input_subset_model took %s seconds.", time.time() - start_time)
            response.update_status('**** Input data fetched', 10)
        except:
            msg = 'failed to fetch or subset input files'
            LOGGER.exception(msg)
            raise Exception(msg)
        
        ########################
        # input data preperation
        ########################
        response.update_status('Start preparing input data', 12)
        start_time = time.time()  # mesure data preperation ...
        # TODO: Check the callendars ! for model vs reanalyses.
        # TODO: Check the units! model vs reanalyses.
        try:
            if direction == 're2mo':
                try:
                    response.update_status('Preparing simulation data', 15)
                    reanalyses_subset = call(resource=nc_subset, time_range=[anaSt, anaEn])
                except:
                    msg = 'failed to prepare simulation period'
                    LOGGER.exception(msg)
                try:
                    response.update_status('Preparing target data', 17)
                    var_target = get_variable(resource)
                    # var_simulation = get_variable(simulation)
                    model_subset = call(resource=resource, variable=var_target,
                                        time_range=[refSt, refEn],
                                        geom=bbox,
                                        t_calendar='standard',
                                        # conform_units_to=conform_units_to,
                                        spatial_wrapping='wrap',
                                        regrid_destination=reanalyses_subset,
                                        regrid_options='bil') # XXXXXXXXXXXX ADD WRAP rem calendar 
                   # ISSUE: the regrided model has white border with null! Check it.
                   # check t_calendar!
                except:
                    msg = 'failed subset archive model'
                    LOGGER.exception(msg)
                    raise Exception(msg)
            else:
                try:
                    response.update_status('Preparing target data', 15)
                    var_target = get_variable(resource)
                    # var_simulation = get_variable(simulation)
                    model_subset = call(resource=resource, variable=var_target,
                                        time_range=[refSt, refEn],
                                        geom=bbox,
                                        t_calendar='standard',
                                        # conform_units_to=conform_units_to,
                                        # spatial_wrapping='wrap',
                                        )
                except:
                    msg = 'failed subset archive model'
                    LOGGER.exception(msg)
                    raise Exception(msg)
                try:
                    response.update_status('Preparing simulation data', 17)
                    reanalyses_subset = call(resource=nc_subset,
                                             time_range=[anaSt, anaEn],
                                             regrid_destination=model_subset,
                                             regrid_options='bil')
                except:
                    msg = 'failed to prepare simulation period'
                    LOGGER.exception(msg)
        except:
            msg = 'failed to subset simulation or reference data'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            if direction == 'mo2re':
                simulation = model_subset
                archive = reanalyses_subset
            elif direction == 're2mo':
                simulation = reanalyses_subset
                archive = model_subset
            else:
                LOGGER.exception('direction not valid: %s ' % direction)
        except:
            msg = 'failed to find comparison direction'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            var_archive = get_variable(archive)
            var_simulation = get_variable(simulation)
            if var_archive != var_simulation:
                rename_variable(archive, oldname=var_archive, newname=var_simulation)
                LOGGER.info('varname %s in netCDF renamed to %s' % (var_archive, var_simulation))
        except:
            msg = 'failed to rename variable in target files'
            LOGGER.exception(msg)
            raise Exception(msg)

        try:
            if seacyc is True:
                seasoncyc_base, seasoncyc_sim = analogs.seacyc(
                                        archive, simulation,
                                        method=normalize)
            else:
                seasoncyc_base = None
                seasoncyc_sim = None
        except:
            msg = 'failed to prepare seasonal cycle reference files'
            LOGGER.exception(msg)
            raise Exception(msg)

        ip, output = mkstemp(dir='.', suffix='.txt')
        output_file = path.abspath(output)
        files = [path.abspath(archive), path.abspath(simulation), output_file]

        # LOGGER.exception("data preperation took %s seconds.", time.time() - start_time)

        ############################
        # generating the config file
        ############################

        response.update_status('writing config file', 18)
        start_time = time.time()  # measure write config ...

        try:
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
                period=[dt.strftime(refSt, '%Y-%m-%d'),
                        dt.strftime(refEn, '%Y-%m-%d')],
                bbox="%s,%s,%s,%s" % (bbox[0], bbox[2], bbox[1], bbox[3]))
        except:
            msg = 'failed to generate config file'
            LOGGER.exception(msg)
            raise Exception(msg)

        # LOGGER.exception("write_config took %s seconds.", time.time() - start_time)

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
                args,
                stdout=subprocess.PIPE,
                stderr=subprocess.PIPE
                ).communicate()
            LOGGER.info('analogue.out info:\n %s ' % output)
            LOGGER.exception('analogue.out errors:\n %s ' % error)
            response.update_status('**** CASTf90 suceeded', 90)
        except:
            msg = 'CASTf90 failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        LOGGER.debug("castf90 took %s seconds.", time.time() - start_time)

        response.update_status('preparting output', 91)

        #Stopper to keep twitcher results, for debug
        # dummy=dummy

        response.outputs['config'] = config_file #config_output_url  # config_file )
        response.outputs['analogs'] = output_file
        response.outputs['output_netcdf'] = simulation
        response.outputs['target_netcdf'] = archive

        ########################
        # generate analog viewer
        ########################

        formated_analogs_file = analogs.reformat_analogs(output_file)
        # response.outputs['formated_analogs'].storage = FileStorage()
        response.outputs['formated_analogs'].file = formated_analogs_file
        LOGGER.info('analogs reformated')
        response.update_status('reformatted analog file', 95)

        viewer_html = analogs.render_viewer(
            # configfile=response.outputs['config'].get_url(),
            configfile=config_file,
            # datafile=response.outputs['formated_analogs'].get_url())
            datafile=formated_analogs_file)
        response.outputs['output'].file = viewer_html
        response.update_status('Successfully generated analogs viewer', 99)
        LOGGER.info('rendered pages: %s ', viewer_html)

        response.update_status('execution ended', 100)
        LOGGER.debug("total execution took %s seconds.", time.time() - process_start_time)
        return response
