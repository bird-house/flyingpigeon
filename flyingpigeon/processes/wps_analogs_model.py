from os import path
from tempfile import mkstemp
from datetime import datetime as dt
#from datetime import timedelta as td
from datetime import time as dt_time
from datetime import date
import time  # performance test

from flyingpigeon.datafetch import _PRESSUREDATA_
from flyingpigeon import analogs
from flyingpigeon.ocgis_module import call
from flyingpigeon.datafetch import reanalyses
from flyingpigeon.utils import get_variable
#from flyingpigeon.utils import get_calendar
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import archive, archiveextract

from pywps import Process
from pywps import LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

import logging
LOGGER = logging.getLogger("PYWPS")


class AnalogsmodelProcess(Process):
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

                # self.BBox = self.addBBoxInput(
                #     identifier="BBox",
                #     title="Bounding Box",
                #     abstract="coordinates to define the region to be analysed",
                #     minOccurs=1,
                #     maxOccurs=1,
                #     crss=['EPSG:4326']
                #     )

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
            ComplexOutput("config", "Config File",
                          abstract="Config file used for the Fortran process",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput("analogs", "Analogues File",
                          abstract="mulit-column text file",
                          as_reference=True,
                          supported_formats=[Format("text/plain")],
                          ),

            ComplexOutput("formated_analogs", "Formated Analogues File",
                          abstract="Formated analogues file for viewer",
                          supported_formats=[Format("text/plain")],
                          as_reference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for one dataset',
                          abstract="Prepared netCDF file as input for weatherregime calculation",
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

        super(AnalogsmodelProcess, self).__init__(
            self._handler,
            identifier="analogs_model",
            title="Analogues of circulation (based on climate model data)",
            abstract='Search for days with analogue pressure pattern for models data sets',
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

        response.update_status('execution started at : %s ' % dt.now(), 5)
        start_time = time.time()  # measure init ...

        ################################
        # reading in the input arguments
        ################################

        try:
            response.update_status('read input parameter : %s ' % dt.now(), 5)

            resource = archiveextract(resource=rename_complexinputs(request.inputs['resource']))
            refSt = request.inputs['refSt'][0].data
            refEn = request.inputs['refEn'][0].data
            dateSt = request.inputs['dateSt'][0].data
            dateEn = request.inputs['dateEn'][0].data
            seasonwin = request.inputs['seasonwin'][0].data
            nanalog = request.inputs['nanalog'][0].data

            # bbox = [-80, 20, 50, 70]
            # TODO: Add checking for wrong cordinates and apply default if nesessary
            bbox=[]
            bboxStr = request.inputs['BBox'][0].data
            bboxStr = bboxStr.split(',')
            #for i in bboxStr: bbox.append(int(i))
            bbox.append(float(bboxStr[0]))
            bbox.append(float(bboxStr[2]))
            bbox.append(float(bboxStr[1]))
            bbox.append(float(bboxStr[3]))
            LOGGER.debug('BBOX for ocgis: %s ' % (bbox))
            LOGGER.debug('BBOX original: %s ' % (bboxStr))

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

            # model_var = request.inputs['reanalyses'][0].data
            # model, var = model_var.split('_')

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
            # refSt = dt.strptime(refSt[0], '%Y-%m-%d')
            # refEn = dt.strptime(refEn[0], '%Y-%m-%d')
            # dateSt = dt.strptime(dateSt[0], '%Y-%m-%d')
            # dateEn = dt.strptime(dateEn[0], '%Y-%m-%d')

            #not nesessary if fix ocgis_module.py
            refSt = dt.combine(refSt,dt_time(12,0))
            refEn = dt.combine(refEn,dt_time(12,0))
            dateSt = dt.combine(dateSt,dt_time(12,0))
            dateEn = dt.combine(dateEn,dt_time(12,0))

            # refSt = refSt.replace(hour=12)
            # refEn = refEn.replace(hour=12)
            # dateSt = dateSt.replace(hour=12)
            # dateEn = dateEn.replace(hour=12)

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

            start = min(refSt, dateSt)
            end = max(refEn, dateEn)

#            if bbox_obj is not None:
#                LOGGER.info("bbox_obj={0}".format(bbox_obj.coords))
#                bbox = [bbox_obj.coords[0][0],
#                        bbox_obj.coords[0][1],
#                        bbox_obj.coords[1][0],
#                        bbox_obj.coords[1][1]]
#                LOGGER.info("bbox={0}".format(bbox))
#            else:
#                bbox = None

            LOGGER.info('environment set')
        except Exception as e:
            msg = 'failed to set environment %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)

        LOGGER.debug("init took %s seconds.", time.time() - start_time)
        response.update_status('Read in and convert the arguments', 5)

        ########################
        # input data preperation
        ########################

        # TODO: Check if files containing more than one dataset

        response.update_status('Start preparing input data', 12)
        start_time = time.time()  # mesure data preperation ...
        try:
            variable = get_variable(resource)
            # TODO: Add selection of the level. maybe bellow in call(..., level_range=[...,...])

            # cal,units = get_calendar(resource)
            # tmp workaround to fix calendar issue with ocgis subsetting
            # if cal == 'proleptic_gregorian':
            #     refEnSub = refEn + td(days=1)        
            #     dateEnSub = dateEn + td(days=1)
            # else:
            #     refEnSub = RefEn
            #     dateEnSub = dateEn

            # archive = call(resource=resource, time_range=[refSt, refEnSub], geom=bbox, spatial_wrapping='wrap')
            # simulation = call(resource=resource, time_range=[dateSt, dateEnSub], geom=bbox, spatial_wrapping='wrap')

            archive = call(resource=resource, time_range=[refSt, refEn], geom=bbox, spatial_wrapping='wrap')
            simulation = call(resource=resource, time_range=[dateSt, dateEn], geom=bbox, spatial_wrapping='wrap')

            if seacyc is True:
                seasoncyc_base, seasoncyc_sim = analogs.seacyc(archive, simulation, method=normalize)
            else:
                seasoncyc_base = None
                seasoncyc_sim = None
        except Exception as e:
            msg = 'failed to prepare archive and simulation files %s ' % e
            LOGGER.debug(msg)
            raise Exception(msg)
        ip, output = mkstemp(dir='.', suffix='.txt')
        output_file = path.abspath(output)
        files = [path.abspath(archive), path.abspath(simulation), output_file]

        LOGGER.debug("data preperation took %s seconds.", time.time() - start_time)

        ############################
        # generating the config file
        ############################

        # TODO: add MODEL name as argument

        response.update_status('writing config file', 15)
        start_time = time.time()  # measure write config ...

        try:
            config_file = analogs.get_configfile(
                files=files,
                seasoncyc_base=seasoncyc_base,
                seasoncyc_sim=seasoncyc_sim,
                timewin=timewin,
                varname=variable,
                seacyc=seacyc,
                cycsmooth=91,
                nanalog=nanalog,
                seasonwin=seasonwin,
                distfun=distance,
                outformat=outformat,
                calccor=True,
                silent=False,
                period=[dt.strftime(refSt, '%Y-%m-%d'), dt.strftime(refEn, '%Y-%m-%d')],
                bbox="%s,%s,%s,%s" % (bbox[0], bbox[2], bbox[1], bbox[3]))
        except Exception as e:
            msg = 'failed to generate config file %s ' % e
            LOGGER.debug(msg)
            raise Exception(msg)

        LOGGER.debug("write_config took %s seconds.", time.time() - start_time)

        ##############
        # CASTf90 call
        ##############
        import subprocess
        import shlex

        start_time = time.time()  # measure call castf90
        response.update_status('Start CASTf90 call', 20)
        try:
            # response.update_status('execution of CASTf90', 50)
            cmd = 'analogue.out %s' % path.relpath(config_file)
            # system(cmd)
            args = shlex.split(cmd)
            output, error = subprocess.Popen(args, stdout=subprocess.PIPE, stderr=subprocess.PIPE).communicate()
            LOGGER.info('analogue.out info:\n %s ' % output)
            LOGGER.debug('analogue.out errors:\n %s ' % error)
            response.update_status('**** CASTf90 suceeded', 70)
        except Exception as e:
            msg = 'CASTf90 failed %s ' % e
            LOGGER.error(msg)
            raise Exception(msg)
        
        LOGGER.debug("castf90 took %s seconds.", time.time() - start_time)
        response.update_status('preparing output', 70)

        response.outputs['config'].file = config_file #config_output_url  # config_file )
        response.outputs['analogs'].file = output_file
        response.outputs['output_netcdf'].file = simulation

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
