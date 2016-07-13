from pywps.Process import WPSProcess

import icclim
import icclim.util.callback as callback

import dateutil.parser
from datetime import datetime
import os
#from os.path import expanduser
#from mkdir_p import *

transfer_limit_Mb = 100

import logging
logger = logging.getLogger()

class ProcessCompoundIndice(WPSProcess):


    def __init__(self):
        WPSProcess.__init__(self,
                            identifier = 'wps_c4i_compound_indice', # only mandatory attribute = same file name
                            title = 'c4i - Rain Temperature Indices',
                            abstract = 'Computes dual input indices of rain and temperature: CD, CW, WD, WW.',
                            version = "1.0",
                            storeSupported = True,
                            statusSupported = True,
                            grassLocation =False)
        
        
        self.indiceNameIn = self.addLiteralInput(identifier = 'indiceName',
                                                title = 'Indice name',
                                                type=type("String"),
                                                default = 'CW')
        self.indiceNameIn.values = ['CD', 'CW', 'WD', 'WW']
        
        
        self.sliceModeIn = self.addLiteralInput(identifier = 'sliceMode',
                                               title = 'Slice mode (temporal grouping to applay for calculations)',
                                               type=type("String"),
                                               default = 'year')
        self.sliceModeIn.values = ["year","month","ONDJFM","AMJJAS","DJF","MAM","JJA","SON"]        
        
        self.filesBasePeriodTemperatureIn = self.addLiteralInput(identifier = 'filesBasePeriodTemperature',
                                                title = 'Input netCDF files list (base (reference) period), daily mean temperature',
                                                abstract="application/netcdf",
                                                type=type("S"),
                                                minOccurs=0,
                                                maxOccurs=1024,
                                                default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')
        
        self.filesBasePeriodPrecipitationIn = self.addLiteralInput(identifier = 'filesBasePeriodPrecipitation',
                                                title = 'Input netCDF files list (base (reference) period), daily precipitation amount',
                                                abstract="application/netcdf",
                                                type=type("S"),
                                                minOccurs=0,
                                                maxOccurs=1024,                                                
                                                default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.timeRangeBasePeriodIn = self.addLiteralInput(identifier = 'timeRangeBasePeriod', 
                                                title = 'Time range of base (reference) period, e.g. 1961-01-01/1990-12-31',
                                                type=type("String"),
                                                default = 'Please Fill-in')                                                 

                                                
        self.varNameTemperatureIn = self.addLiteralInput(identifier = 'varNameTemperature',
                                                title = 'Variable name to process (daily mean temperature)',
                                                type=type("String"),
                                                default = 'tas')
        
        self.varNamePrecipitationIn = self.addLiteralInput(identifier = 'varNamePrecipitation',
                                                title = 'Variable name to process (daily precipitation amount)',
                                                type=type("String"),
                                                default = 'pr')
        
        self.filesStudyPeriodTemperatureIn = self.addLiteralInput(identifier = 'filesStudyPeriodTemperature',
                                                title = 'Input netCDF files list (study period), daily mean temperature',
                                                abstract="application/netcdf",
                                                type=type("S"),
                                                minOccurs=0,
                                                maxOccurs=1024,
                                                default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.filesStudyPeriodPrecipitationIn = self.addLiteralInput(identifier = 'filesStudyPeriodPrecipitation',
                                                title = 'Input netCDF files list (study period), daily precipitation amount',
                                                abstract="application/netcdf",
                                                type=type("S"),
                                                minOccurs=0,
                                                maxOccurs=1024,
                                                default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                           'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/pr_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')
        
        
        self.timeRangeStudyPeriodIn = self.addLiteralInput(identifier = 'timeRangeStudyPeriod', 
                                                title = 'Time range',
                                                abstract = 'Time range is mandatory, e.g. 2010-01-01/2012-12-31. Please fill-in.'
                                                type=type("String"),
                                                minOccurs = 1,
                                                default = '2010-01-01/2012-12-31')                                                 

         self.leapNonLeapYearsIn = self.addLiteralInput(
            identifier = 'leapNonLeapYears',
            title = 'Take only leap years?',
            abstract = "Method for computing a percentile value for the calendar day of February 29th. Default: take all years (leap and non-leap)",
            type=type(False),
            minOccurs=1,
            maxOccurs=1,
            default = False)
        #self.leapNonLeapYearsIn.values = ["take all years (leap and non-leap)","take only leap years"]
        
        
        ## self.outputFileNameIn = self.addLiteralInput(identifier = 'outputFileName', 
        ##                                         title = 'Name of output netCDF file',
        ##                                         type="String",
        ##                                         default = './out_icclim.nc')
        
        self.NLevelIn = self.addLiteralInput(identifier = 'NLevel', 
                                                title = 'Number of level (if 4D variable)',
                                                minOccurs = 0,
                                                type=type(1))

        #self.opendapURL = self.addLiteralOutput(identifier = "opendapURL",title = "opendapURL");
        self.output = self.addComplexOutput(
            identifier="output",
            title="Climate Indice",
            abstract="Calculated climate indice with icclim.",
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True,
        )
        
        
    def callback(self,message,percentage):
        self.status.set("%s" % str(message),str(percentage));        
                                
                                
    def execute(self):
        # Very important: This allows the NetCDF library to find the users credentials (X509 cert)
        #homedir = os.environ['HOME']
        #os.chdir(homedir)
        
        def callback(b):
            self.callback("Processing",b)           
                    
    
        in_files_t = []
        in_files_t.extend(self.filesBasePeriodTemperatureIn.getValue())
        
        in_files_p = []
        in_files_p.extend(self.filesBasePeriodPrecipitationIn.getValue())
        
        time_range_base_period = self.timeRangeBasePeriodIn.getValue()
        time_range_study_period = self.timeRangeStudyPeriodIn.getValue()
        
        var_name_t = self.varNameTemperatureIn.getValue()
        var_name_p = self.varNamePrecipitationIn.getValue()
        
        indice_name = self.indiceNameIn.getValue()
        
        in_files_t.extend(self.filesStudyPeriodTemperatureIn.getValue())
        
        in_files_p.extend(self.filesStudyPeriodPrecipitationIn.getValue())
    
        leap_nonleap_years = self.leapNonLeapYearsIn.getValue()
    
        slice_mode = self.sliceModeIn.getValue()
        #out_file_name = self.outputFileNameIn.getValue()
        out_file_name = 'out.nc'
        level = self.NLevelIn.getValue()
        
        if time_range_base_period:
            startdate = dateutil.parser.parse(time_range_base_period.split("/")[0])
            stopdate  = dateutil.parser.parse(time_range_base_period.split("/")[1])
            time_range_base_period = [startdate,stopdate]
        
        
        if time_range_study_period:
            startdate = dateutil.parser.parse(time_range_study_period.split("/")[0])
            stopdate  = dateutil.parser.parse(time_range_study_period.split("/")[1])
            time_range_study_period = [startdate,stopdate]
        
        
        #home = expanduser("~")
        
        self.status.set("Preparing....", 0)
        
        #pathToAppendToOutputDirectory = "/WPS_"+self.identifier+"_" + datetime.now().strftime("%Y%m%dT%H%M%SZ")
        
        """ URL output path """
        #fileOutURL  = os.environ['POF_OUTPUT_URL']  + pathToAppendToOutputDirectory+"/"
        
        """ Internal output path"""
        #fileOutPath = os.environ['POF_OUTPUT_PATH']  + pathToAppendToOutputDirectory +"/"

        """ Create output directory """
        #mkdir_p(fileOutPath)
        
        self.status.set("Processing input lists: " + str(in_files_t) +  " " + str(in_files_p), 0)  

        # CW (cold/wet days): (TG < 25th pctl) and (RR > 75th pctl)
        # CD (cold/dry days): (TG < 25th pctl) and (RR < 25th pctl)
        # WD (warm/dry days): (TG > 75th pctl) and (RR < 25th pctl)
        # WW (warm/wet days): (TG > 75th pctl) and (RR > 75th pctl)
        
        if indice_name == 'CW':
            logical_operation = ['lt', 'gt']
            thresh = ['p25', 'p75']
        elif indice_name == 'CD':
            logical_operation = ['lt', 'lt']
            thresh = ['p25', 'p25']
        elif indice_name == 'WD':
            logical_operation = ['gt', 'lt']
            thresh = ['p75', 'p25']
        elif indice_name == 'WW':
            logical_operation = ['gt', 'gt']
            thresh = ['p75', 'p75']
            
        my_indice_params = {'indice_name': indice_name,
                            'calc_operation': 'nb_events', ### 'calc_operation': 'max_nb_consecutive_events'
                            'logical_operation': logical_operation,
                            'thresh': thresh,
                            'var_type': ['t', 'p'],
                            'link_logical_operations': 'and'
                           }

        icclim.indice(user_indice=my_indice_params,
                      in_files=[in_files_t,in_files_p],
                      var_name=[var_name_t,var_name_p],
                      slice_mode=slice_mode,
                      time_range=time_range_study_period,
                      out_file=out_file_name,
                      N_lev=level,
                      transfer_limit_Mbytes=transfer_limit_Mb,
                      callback=callback,
                      callback_percentage_start_value=0,
                      callback_percentage_total=100,
                      base_period_time_range=time_range_base_period,
                      window_width=5,
                      only_leap_years=leap_nonleap_years,
                      ignore_Feb29th=True,
                      interpolation='hyndman_fan',
                      out_unit='days')

                    
        """ Set output """
        #url = fileOutURL+"/"+out_file_name
        #self.opendapURL.setValue(url)
        self.output.setValue(out_file_name)
        self.status.set("ready",100)
