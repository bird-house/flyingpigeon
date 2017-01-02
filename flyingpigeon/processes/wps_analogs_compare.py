from datetime import date
from pywps.Process import WPSProcess
from flyingpigeon.datafetch import _PRESSUREDATA_
import logging
logger = logging.getLogger(__name__)


class AnalogsProcess(WPSProcess):

    def __init__(self):
        # definition of this process
        WPSProcess.__init__(
            self,
            identifier="analogs_compare",
            title="Analogues -- Comparison",
            version="0.9",
            metadata=[
                  {"title": "LSCE",
                   "href": "http://www.lsce.ipsl.fr/en/index.php"},
                  {"title": "Doc",
                   "href": "http://flyingpigeon.readthedocs.io/en/latest/descriptions/\
                   analogues.html#analogues-of-circulation"}
                  ],
            abstract="Search in a dataset for days with analogue pressure pattern for a given period in\
             a reanalyses dataset",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
          identifier="resource",
          title="Resource",
          abstract="Input dataset for analougs processing",
          minOccurs=0,
          maxOccurs=1000,
          maxmegabites=5000,
          formats=[{"mimeType": "application/x-netcdf"}],
          )

        self.experiment = self.addLiteralInput(
          identifier="experiment",
          title="Data experiment",
          abstract="Choose the reanalyses experiment",
          default="NCEP_slp",
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
          allowedValues=_PRESSUREDATA_
          )

        self.BBox = self.addBBoxInput(
          identifier="BBox",
          title="Bounding Box",
          abstract="coordinates to define the region to be analysed",
          minOccurs=1,
          maxOccurs=1,
          crss=['EPSG:4326']
          )

        self.dateSt = self.addLiteralInput(
          identifier="dateSt",
          title="Start date of analysis period",
          abstract="This is a Date: 2013-07-15",
          default="2013-07-15",
          type=type(date(2013, 7, 15)),
          minOccurs=1,
          maxOccurs=1,
          )

        self.dateEn = self.addLiteralInput(
          identifier="dateEn",
          title="End date of analysis period",
          abstract="This is a Date: 2013-12-31",
          default="2014-12-31",
          type=type(date(2014, 12, 31)),
          minOccurs=1,
          maxOccurs=1,
          )

        self.refSt = self.addLiteralInput(
          identifier="refSt",
          title="Start reference period",
          abstract="Start YEAR of reference period",
          default="2013-01-01",
          type=type(date(1955, 01, 01)),
          minOccurs=1,
          maxOccurs=1,
          )

        self.refEn = self.addLiteralInput(
          identifier="refEn",
          title="End reference period",
          abstract="End YEAR of reference period",
          default="2014-12-31",
          type=type(date(1957, 12, 31)),
          minOccurs=1,
          maxOccurs=1,
          )

        self.direction = self.addLiteralInput(
          identifier="direction",
          title="Direction",
          abstract="Compare direction. Pick analog days in Modeldata for a simulation period in Reanalyses data \
                    (re2mo) or vice versa",
          default='re2mo',
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
          allowedValues=['mo2re', 're2mo']
            )

        self.seasonwin = self.addLiteralInput(
          identifier="seasonwin",
          title="Seasonal window",
          abstract="Number of days befor and after the date to be analysed",
          default=30,
          type=type(1),
          minOccurs=0,
          maxOccurs=1,
          )

        self.nanalog = self.addLiteralInput(
          identifier="nanalog",
          title="Nr of analogues",
          abstract="Number of analogues to be detected",
          default=20,
          type=type(1),
          minOccurs=0,
          maxOccurs=1,
          )

        self.normalize = self.addLiteralInput(
          identifier="normalize",
          title="normalization",
          abstract="Normalize by subtraction of annual cycle",
          default='own',
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
          allowedValues=['None', 'base', 'sim', 'own']
            )

        self.distance = self.addLiteralInput(
          identifier="dist",
          title="Distance",
          abstract="Distance function to define analogues",
          default='euclidean',
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
          allowedValues=['euclidean', 'mahalanobis', 'cosine', 'of']
            )

        self.outformat = self.addLiteralInput(
          identifier="outformat",
          title="output file format",
          abstract="Choose the format for the analogue output file",
          default="ascii",
          type=type(''),
          minOccurs=1,
          maxOccurs=1,
          allowedValues=['ascii', 'netCDF4']
          )

        self.timewin = self.addLiteralInput(
          identifier="timewin",
          title="Time window",
          abstract="Number of days following the analogue day the distance will be averaged",
          default=1,
          type=type(1),
          minOccurs=0,
          maxOccurs=1,
          )

        # self.variable = self.addLiteralInput(
        #   identifier="variable",
        #   title="Variable",
        #   abstract="Variable name in resource",
        #   default='slp',
        #   type=type(''),
        #   minOccurs=0,
        #   maxOccurs=1,
        #   )

        # self.seacyc = self.addLiteralInput(
        #   identifier="seacyc",
        #   title="Seasonal Cycle",
        #   abstract="normalized by the Seasonal Cycle",
        #   default=True,
        #   type=type(boolean),
        #   minOccurs=0,
        #   maxOccurs=1,
        #   )

        ######################
        # define the outputs
        ######################

        self.config = self.addComplexOutput(
          identifier="config",
          title="Config File",
          abstract="Config file used for the Fortran process",
          formats=[{"mimeType": "text/plain"}],
          asReference=True,
          )

        self.analogs = self.addComplexOutput(
          identifier="analogs",
          title="Analogues File",
          abstract="mulit-column text file",
          formats=[{"mimeType": "text/plain"}],
          asReference=True,
          )

        self.simulation_netcdf = self.addComplexOutput(
          title="prepared netCDF",
          abstract="NetCDF file with subset and normaized values of simulation dataset",
          formats=[{"mimeType": "application/x-netcdf"}],
          asReference=True,
          identifier="simulation_netcdf",
          )

        self.target_netcdf = self.addComplexOutput(
          title="Target netCDF",
          abstract="File with subset and normaized values of target dataset",
          formats=[{"mimeType": "application/x-netcdf"}],
          asReference=True,
          identifier="target_netcdf",
          )

    def execute(self):
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
        direction = self.getInputValues(identifier='direction')[0]
        normalize = self.getInputValues(identifier='normalize')[0]
        distance = self.getInputValues(identifier='dist')[0]
        outformat = self.getInputValues(identifier='outformat')[0]
        timewin = int(self.getInputValues(identifier='timewin')[0])
        experiment = self.getInputValues(identifier='experiment')[0]
        dataset, var = experiment.split('_')

        try:
            if direction == 're2mo':
                refSt = dt.strptime(refSt[0], '%Y-%m-%d')
                refEn = dt.strptime(refEn[0], '%Y-%m-%d')
                dateSt = dt.strptime(dateSt[0], '%Y-%m-%d')
                dateEn = dt.strptime(dateEn[0], '%Y-%m-%d')
            elif direction == 'mo2re':
                dateSt = dt.strptime(refSt[0], '%Y-%m-%d')
                dateEn = dt.strptime(refEn[0], '%Y-%m-%d')
                refSt = dt.strptime(dateSt[0], '%Y-%m-%d')
                refEn = dt.strptime(dateEn[0], '%Y-%m-%d')
            else:
                logger.error('failed to find time periods for comparison direction')
        except Exception as e:
            msg = 'failed to put simulation and reference time in order %s ' % e
            logger.debug(msg)
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
            logger.error('output format not valid')
        if bbox_obj is not None:
            logger.info("bbox_obj={0}".format(bbox_obj.coords))
            bbox = [bbox_obj.coords[0][0],
                    bbox_obj.coords[0][1],
                    bbox_obj.coords[1][0],
                    bbox_obj.coords[1][1]]
            logger.info("bbox={0}".format(bbox))
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
                logger.error('Reanalyses dataset not known')
            logger.info('environment set')
        except Exception as e:
            msg = 'failed to set environment %s ' % e
            logger.error(msg)
            raise Exception(msg)

        logger.debug("init took %s seconds.", time.time() - start_time)
        self.status.set('Read in the arguments', 5)

        #################
        # get input data
        #################
        start_time = time.time()  # measure get_input_data ...
        self.status.set('fetching input data', 7)
        try:
            nc_reanalyses = reanalyses(start=dateSt.year, end=dateEn.year,
                                       variable=var, dataset=dataset)
            nc_subset = call(resource=nc_reanalyses, variable=var, geom=bbox)
            logger.debug("get_input_subset_dataset took %s seconds.",
                         time.time() - start_time)
            self.status.set('**** Input data fetched', 10)
        except Exception as e:
            msg = 'failed to fetch or subset input files %s' % e
            logger.error(msg)
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
                    reanalyses_subset = call(resource=nc_subset, time_range=[dateSt, dateEn])
                except:
                    msg = 'failed to prepare simulation period'
                    logger.debug(msg)
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
                except Exception as e:
                    msg = 'failed subset archive dataset %s ' % e
                    logger.debug(msg)
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
                except Exception as e:
                    msg = 'failed subset archive dataset %s ' % e
                    logger.debug(msg)
                    raise Exception(msg)
                try:
                    self.status.set('Preparing simulation data', 15)
                    reanalyses_subset = call(resource=nc_subset,
                                             time_range=[dateSt, dateEn],
                                             regrid_destination=model_subset,
                                             regrid_options='bil')
                except:
                    msg = 'failed to prepare simulation period'
                    logger.debug(msg)
        except Exeption as e:
            msg = 'failed to subset simulation or reference data %s ' % e
            logger.debug(msg)
            raise Exception(msg)

        try:
            if direction == 'mo2re':
                simulation = model_subset
                archive = reanalyses_subset
            elif direction == 're2mo':
                simulation = reanalyses_subset
                archive = model_subset
            else:
                logger.error('direction not valid: %s ' % direction)
        except Exception as e:
            msg = 'failed to find comparison direction %s' % e
            logger.debug(msg)
            raise Exception(msg)

        try:
            if var != var_target:
                rename_variable(archive, oldname=var_target, newname=var)
                logger.info('varname %s in netCDF renamed to %s'
                            % (var_target, var))
        except Exception as e:
            msg = 'failed to rename variable in target files %s ' % e
            logger.debug(msg)
            raise Exception(msg)

        try:
            if seacyc is True:
                seasoncyc_base, seasoncyc_sim = analogs.seacyc(
                                        archive, simulation,
                                        method=normalize)
            else:
                seasoncyc_base, seasoncyc_sim = None
        except Exception as e:
            msg = 'failed to prepare seasonal cycle reference files %s ' % e
            logger.debug(msg)
            raise Exception(msg)

        ip, output = mkstemp(dir='.', suffix='.txt')
        output_file = path.abspath(output)
        files = [path.abspath(archive), path.abspath(simulation), output_file]

        logger.debug("data preperation took %s seconds.",
                     time.time() - start_time)

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
        except Exception as e:
            msg = 'failed to generate config file %s ' % e
            logger.debug(msg)
            raise Exception(msg)

        logger.debug("write_config took %s seconds.", time.time() - start_time)

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
            logger.info('analogue.out info:\n %s ' % output)
            logger.debug('analogue.out errors:\n %s ' % error)
            self.status.set('**** CASTf90 suceeded', 90)
        except Exception as e:
            msg = 'CASTf90 failed %s ' % e
            logger.error(msg)
            raise Exception(msg)

        logger.debug("castf90 took %s seconds.", time.time() - start_time)

        self.status.set('preparting output', 99)
        self.config.setValue(config_file)
        self.analogs.setValue(output_file)
        self.simulation_netcdf.setValue(simulation)
        self.target_netcdf.setValue(archive)

        self.status.set('execution ended', 100)
        logger.debug("total execution took %s seconds.",
                     time.time() - process_start_time)
