from datetime import date
from pywps.Process import WPSProcess
from flyingpigeon.datafetch import _PRESSUREDATA_

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format, FORMATS
from pywps.app.Common import Metadata
from flyingpigeon.log import init_process_logger

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
                         default=30,
                         data_type='integer',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput("nanalog", "Nr of analogues",
                         abstract="Number of analogues to be detected",
                         default=20,
                         type='integer',
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
                         default=1,
                         data_type='inter',
                         min_occurs=0,
                         max_occurs=1,
                         ),

        # self.direction = self.addLiteralInput(
        #   identifier="direction",
        #   title="Direction",
        #   abstract="Compare direction. Pick analog days in Modeldata for a simulation period in Reanalyses data \
        #             (re2mo) or vice versa",
        #   default='re2mo',
        #   type=type(''),
        #   minOccurs=1,
        #   maxOccurs=1,
        #   allowedValues=['mo2re', 're2mo']
            # )
        ]
        outputs = [
            LiteralOutput("config", "Config File",
                          abstract="Config file used for the Fortran process",
                          default=None,
                          data_type='string',
                          # formats=[{"mimeType":"text/plain"}],
                          # asReference=True,
                          ),

            ComplexOutput("analogs", "Analogues File",
                          abstract="mulit-column text file",
                          data_formats=[Format("text/plain")],
                          asReference=True,
                          ),

            ComplexOutput('output_netcdf', 'Subsets for dataset',
                          abstract="Prepared netCDF file as input for weatherregime calculation",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput("target_netcdf", 'subset of ref file'
                          abstract="File with subset and normaized values of target dataset",
                          formats=[{"mimeType": "application/x-netcdf"}],
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),


            # ComplexOutput("output_html", "Analogues Viewer html page",
            #               abstract="Interactive visualization of calculated analogues",
            #               data_formats=[Format("text/html")],
            #               as_reference=True,
            #               )

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


        # definition of this process


    def execute(self):
        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        import time  # performance test
        process_start_time = time.time()  # measure process execution time ...

        from os import path
        from tempfile import mkstemp
        from flyingpigeon import analogs
        from datetime import datetime as dt
        from flyingpigeon.ocgis_module import call
        from flyingpigeon.datafetch import reanalyses
        from flyingpigeon.utils import get_variable, rename_variable
        self.status.set('execution started at : %s ' % dt.now(), 5)

        start_time = time.time()  # measure init ...

        resource = self.getInputValues(identifier='resource')
        bbox_obj = self.BBox.getValue()
        refSt = self.getInputValues(identifier='refSt')
        refEn = self.getInputValues(identifier='refEn')
        dateSt = self.getInputValues(identifier='dateSt')
        dateEn = self.getInputValues(identifier='dateEn')
        seasonwin = int(self.getInputValues(identifier='seasonwin')[0])
        nanalog = int(self.getInputValues(identifier='nanalog')[0])
        direction = self.getInputValues(identifier='direction')[0]
        normalize = self.getInputValues(identifier='normalize')[0]
        distance = self.getInputValues(identifier='dist')[0]
        outformat = self.getInputValues(identifier='outformat')[0]
        timewin = int(self.getInputValues(identifier='timewin')[0])
        reanalyses = self.getInputValues(identifier='reanalyses')[0]
        dataset, var = reanalyses.split('_')

        try:
            if direction == 're2mo':
                anaSt = dt.strptime(dateSt[0], '%Y-%m-%d')
                anaEn = dt.strptime(dateEn[0], '%Y-%m-%d')
                refSt = dt.strptime(refSt[0], '%Y-%m-%d')
                refEn = dt.strptime(refEn[0], '%Y-%m-%d')
            elif direction == 'mo2re':
                anaSt = dt.strptime(refSt[0], '%Y-%m-%d')
                anaEn = dt.strptime(refEn[0], '%Y-%m-%d')
                refSt = dt.strptime(dateSt[0], '%Y-%m-%d')
                refEn = dt.strptime(dateEn[0], '%Y-%m-%d')
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
        if bbox_obj is not None:
            LOGGER.info("bbox_obj={0}".format(bbox_obj.coords))
            bbox = [bbox_obj.coords[0][0],
                    bbox_obj.coords[0][1],
                    bbox_obj.coords[1][0],
                    bbox_obj.coords[1][1]]
            LOGGER.info("bbox={0}".format(bbox))
        else:
            bbox = None

        try:
            if dataset == 'NCEP':
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
                LOGGER.exception('Reanalyses dataset not known')
            LOGGER.info('environment set')
        except:
            msg = 'failed to set environment'
            LOGGER.exception(msg)
            raise Exception(msg)

        LOGGER.exception("init took %s seconds.", time.time() - start_time)
        self.status.set('Read in the arguments', 5)

        #################
        # get input data
        #################
        start_time = time.time()  # measure get_input_data ...
        self.status.set('fetching input data', 7)
        try:
            nc_reanalyses = reanalyses(start=anaSt.year, end=anaEn.year,
                                       variable=var, dataset=dataset)
            nc_subset = call(resource=nc_reanalyses, variable=var, geom=bbox)
            LOGGER.exception("get_input_subset_dataset took %s seconds.", time.time() - start_time)
            self.status.set('**** Input data fetched', 10)
        except:
            msg = 'failed to fetch or subset input files'
            LOGGER.exception(msg)
            raise Exception(msg)

        ########################
        # input data preperation
        ########################
        self.status.set('Start preparing input data', 12)
        start_time = time.time()  # mesure data preperation ...

        try:
            if direction == 're2mo':
                try:
                    self.status.set('Preparing simulation data', 15)
                    reanalyses_subset = call(resource=nc_subset, time_range=[anaSt, anaEn])
                except:
                    msg = 'failed to prepare simulation period'
                    LOGGER.exception(msg)
                try:
                    self.status.set('Preparing target data', 17)
                    var_target = get_variable(resource)
                    # var_simulation = get_variable(simulation)
                    model_subset = call(resource=resource, variable=var_target,
                                        time_range=[refSt, refEn],
                                        geom=bbox,
                                        t_calendar='standard',
                                        # conform_units_to=conform_units_to,
                                        # spatial_wrapping='wrap',
                                        regrid_destination=reanalyses_subset,
                                        regrid_options='bil')
                except:
                    msg = 'failed subset archive dataset'
                    LOGGER.exception(msg)
                    raise Exception(msg)
            else:
                try:
                    self.status.set('Preparing target data', 17)
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
                    msg = 'failed subset archive dataset'
                    LOGGER.exception(msg)
                    raise Exception(msg)
                try:
                    self.status.set('Preparing simulation data', 15)
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
                seasoncyc_base, seasoncyc_sim = None
        except:
            msg = 'failed to prepare seasonal cycle reference files'
            LOGGER.exception(msg)
            raise Exception(msg)

        ip, output = mkstemp(dir='.', suffix='.txt')
        output_file = path.abspath(output)
        files = [path.abspath(archive), path.abspath(simulation), output_file]

        LOGGER.exception("data preperation took %s seconds.", time.time() - start_time)

        ############################
        # generating the config file
        ############################

        self.status.set('writing config file', 15)
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
                bbox="%s,%s,%s,%s" % (bbox[0], bbox[2], bbox[1], bbox[3]))
        except:
            msg = 'failed to generate config file'
            LOGGER.exception(msg)
            raise Exception(msg)

        LOGGER.exception("write_config took %s seconds.", time.time() - start_time)

        #######################
        # CASTf90 call
        #######################
        import subprocess
        import shlex

        start_time = time.time()  # measure call castf90

        self.status.set('Start CASTf90 call', 20)
        try:
            # self.status.set('execution of CASTf90', 50)
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
            self.status.set('**** CASTf90 suceeded', 90)
        except:
            msg = 'CASTf90 failed'
            LOGGER.exception(msg)
            raise Exception(msg)

        LOGGER.exception("castf90 took %s seconds.", time.time() - start_time)

        self.status.set('preparting output', 99)
        self.config.setValue(config_file)
        self.analogs.setValue(output_file)
        self.simulation_netcdf.setValue(simulation)
        self.target_netcdf.setValue(archive)

        self.status.set('execution ended', 100)
        LOGGER.exception("total execution took %s seconds.", time.time() - process_start_time)
