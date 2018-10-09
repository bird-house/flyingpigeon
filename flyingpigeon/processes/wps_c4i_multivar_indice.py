<<<<<<< HEAD
=======
import logging

>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41
import dateutil.parser
import icclim
from pywps.Process import WPSProcess

# from os.path import expanduser
# from mkdir_p import *

transfer_limit_Mb = 100

<<<<<<< HEAD
=======
logger = logging.getLogger()
>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41

# TODO: Integrate or remove logging from this
# import logging
# LOGGER = logging.getLogger()

class ProcessMultivarIndice(WPSProcess):

class ProcessMultivarIndice(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(self,
                            identifier='wps_c4i_multivar_indice',  # only mandatory attribute = same file name
                            title='c4i - Range Indices',
                            abstract='Computes temperature range indices: ETR, DTR, DTRv. This processes is also available in Climate4Impact and uses ICCLIM.',
                            version="1.0",
                            metadata=[
                                {"title": "ICCLIM", "href": "http://icclim.readthedocs.io/en/latest/"},
                                {"title": "Climate4Impact",
                                 "href": "http://climate4impact.eu/impactportal/general/index.jsp"},
                            ],
                            storeSupported=True,
                            statusSupported=True,
                            grassLocation=False)

        self.indiceNameIn = self.addLiteralInput(identifier='indiceName',
                                                 title='Index name',
                                                 type=type("String"),
                                                 minOccurs=1,
                                                 default='ETR')
        self.indiceNameIn.values = ['DTR', 'ETR', 'vDTR']

        self.sliceModeIn = self.addLiteralInput(identifier='sliceMode',
                                                title='Slice mode (temporal grouping to apply to calculations)',
                                                type=type("String"),
                                                minOccurs=1,
                                                default='year')
        self.sliceModeIn.values = ["year", "month", "ONDJFM", "AMJJAS", "DJF", "MAM", "JJA", "SON"]

        self.filesTasmaxIn = self.addLiteralInput(identifier='filesTasmax',
                                                  title='Input netCDF files list (daily max temperature)',
                                                  abstract="application/netcdf",
                                                  type=type("S"),
                                                  minOccurs=0,
                                                  maxOccurs=1024,
                                                  default='http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmax_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmax_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmax_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmax_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.varTasmaxIn = self.addLiteralInput(identifier='varTasmax',
                                                title='Variable name to process (daily max temperature)',
                                                type=type("String"),
                                                minOccurs=1,
                                                default='tasmax')

        self.filesTasminIn = self.addLiteralInput(identifier='filesTasmin',
                                                  title='Input netCDF files list (daily min temperature)',
                                                  abstract="application/netcdf",
                                                  type=type("S"),
                                                  minOccurs=0,
                                                  maxOccurs=1024,
                                                  default='http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmin_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmin_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmin_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tasmin_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.varTasminIn = self.addLiteralInput(identifier='varTasmin',
                                                title='Variable name to process (daily min temperature)',
                                                type=type("String"),
                                                minOccurs=1,
                                                default='tasmin')

        self.timeRangeIn = self.addLiteralInput(identifier='timeRange',
                                                title='Time range, e.g. 2010-01-01/2012-12-31',
                                                type=type("String"),
                                                minOccurs=0)

<<<<<<< HEAD
        # self.outputFileNameIn = self.addLiteralInput(identifier = 'outputFileName'
        #     title = 'Name of output netCDF file',
        #     type="String",
        #     default = './out_icclim.nc')
=======
        ## self.outputFileNameIn = self.addLiteralInput(identifier = 'outputFileName', 
        ##                                        title = 'Name of output netCDF file',
        ##                                        type="String",
        ##                                        default = './out_icclim.nc')
>>>>>>> 19815922c9b8e810550156a12b0c458b221d7c41

        self.NLevelIn = self.addLiteralInput(identifier='NLevel',
                                             title='Number of levels (if 4D variable)',
                                             type=type(1),
                                             minOccurs=0)

        # self.opendapURL = self.addLiteralOutput(identifier = "opendapURL",title = "opendapURL");

        self.output = self.addComplexOutput(
            identifier="output",
            title="Climate Index",
            abstract="Calculated climate index with icclim.",
            formats=[{"mimeType": "application/x-netcdf"}],
            asReference=True)

    def callback(self, message, percentage):
        self.status.set("%s" % str(message), str(percentage));

    def execute(self):
        # Very important: This allows the NetCDF library to find the users credentials (X509 cert)
        # homedir = os.environ['HOME']
        # os.chdir(homedir)
        def callback(b):
            self.callback("Processing", b)

        files_tasmax = [];
        files_tasmax.extend(self.filesTasmaxIn.getValue())

        var_tasmax = self.varTasmaxIn.getValue()

        files_tasmin = [];
        files_tasmin.extend(self.filesTasminIn.getValue())

        var_tasmin = self.varTasminIn.getValue()
        indice_name = self.indiceNameIn.getValue()
        slice_mode = self.sliceModeIn.getValue()
        time_range = self.timeRangeIn.getValue()
        # out_file_name = self.outputFileNameIn.getValue()
        out_file_name = "out.nc"
        level = self.NLevelIn.getValue()

        if time_range:
            startdate = dateutil.parser.parse(time_range.split("/")[0])
            stopdate = dateutil.parser.parse(time_range.split("/")[1])
            time_range = [startdate, stopdate]

        self.status.set("Preparing....", 0)

        # pathToAppendToOutputDirectory = "/WPS_"+self.identifier+"_" + datetime.now().strftime("%Y%m%dT%H%M%SZ")

        """ URL output path """
        # fileOutURL  = os.environ['POF_OUTPUT_URL']  + pathToAppendToOutputDirectory+"/"

        """ Internal output path"""
        # fileOutPath = os.environ['POF_OUTPUT_PATH']  + pathToAppendToOutputDirectory +"/"

        """ Create output directory """
        # mkdir_p(fileOutPath)

        self.status.set("Processing input lists: " + str(files_tasmax) + " " + str(files_tasmin), 0)

        icclim.indice(indice_name=indice_name,
                      in_files=[files_tasmax, files_tasmin],
                      var_name=[var_tasmax, var_tasmin],
                      slice_mode=slice_mode,
                      time_range=time_range,
                      out_file=out_file_name,
                      N_lev=level,
                      transfer_limit_Mbytes=transfer_limit_Mb,
                      callback=callback,
                      callback_percentage_start_value=0,
                      callback_percentage_total=100,
                      base_period_time_range=None,
                      window_width=5,
                      only_leap_years=False,
                      ignore_Feb29th=True,
                      interpolation='hyndman_fan',
                      netcdf_version='NETCDF4_CLASSIC',
                      out_unit='days')

        """ Set output """
        # url = fileOutURL+"/"+out_file_name;
        # self.opendapURL.setValue(url);
        self.output.setValue(out_file_name)

        self.status.set("ready", 100);
