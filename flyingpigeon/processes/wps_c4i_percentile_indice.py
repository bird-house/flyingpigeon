from pywps.Process import WPSProcess

import icclim
import icclim.util.callback as callback

import dateutil.parser
from datetime import datetime
import os
from os.path import expanduser
#from mkdir_p import *

from flyingpigeon.utils import make_dirs

transfer_limit_Mb = 100

import logging
logger = logging.getLogger()

class ProcessPercentileIndice(WPSProcess):


    def __init__(self):
        WPSProcess.__init__(self,
                            identifier = 'wps_percentile_indice', # only mandatory attribute = same file name
                            title = 'c4i - Percentile Indices',
                            abstract = 'Computes single input, percentile based indices of temperature: TG10p, TX10p, TN10p, TG90p, TX90p, TN90p, WSDI, CSDI; and of rainfall R75p, R95p, R99p, R75TOT, R95TOT, R99TOT. This processes is also available in Climate4Impact and uses ICCLIM.',
                            version = "1.0",
                            metadata = [
                                {"title": "ICCLIM" , "href": "http://icclim.readthedocs.io/en/latest/"},
                                {"title": "Climate4Impact", "href": "http://climate4impact.eu/impactportal/general/index.jsp"},
                            ],
                            storeSupported = True,
                            statusSupported = True,
                            grassLocation =False)


        ## self.filesBasePeriodIn = self.addLiteralInput(identifier = 'filesBasePeriod',
        ##                                        title = 'Input netCDF files list (base (reference) period)',
        ##                                        abstract="application/netcdf",
        ##                                        type=type("S"),
        ##                                        minOccurs=0,
        ##                                        maxOccurs=1024,
        ##                                        default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
        ##                                                   'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
        ##                                                   'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
        ##                                                   'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.filesBasePeriodIn = self.addComplexInput(
            identifier="filesBasePeriod",
            title="URL to your NetCDF File",
            abstract="Input netCDF files list (base (reference) period). You may provide a URL or upload a NetCDF file.",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=10000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.indiceNameIn = self.addLiteralInput(identifier = 'indiceName',
                                               title = 'Indice name',
                                               type=type("String"),
                                               minOccurs = 1,
                                               maxOccurs = 1,
                                               default = 'TG90p')

        self.indiceNameIn.values = ['TG10p', 'TX10p', 'TN10p', 'TG90p', 'TX90p', 'TN90p', 'WSDI', 'CSDI', 'R75p', 'R75TOT', 'R95p', 'R95TOT', 'R99p', 'R99TOT']


        self.sliceModeIn = self.addLiteralInput(identifier = 'sliceMode',
                                               title = 'Slice mode (temporal grouping to apply for calculations)',
                                               type=type("String"),
                                               minOccurs = 1,
                                               maxOccurs = 1,
                                               default = 'year')
        self.sliceModeIn.values = ["year","month","ONDJFM","AMJJAS","DJF","MAM","JJA","SON"]
        
       
        
        
        self.timeRangeBasePeriodIn = self.addLiteralInput(identifier = 'timeRangeBasePeriod', 
                                               title = 'Time range of base (reference) period, e.g. 1961-01-01/1990-12-31',
                                               abstract = "Time range is mandatory. Please fill-in.",
                                               type=type("String"),
                                               minOccurs = 1,
                                               maxOccurs = 1,
                                               default = '1961-01-01/1990-12-31')                                                 
        
        
        self.filesStudyPeriodIn = self.addLiteralInput(identifier = 'filesStudyPeriod',
                                               title = 'Input netCDF files list (study period)',
                                               abstract="application/netcdf",
                                               type=type("S"),
                                               minOccurs=0,
                                               maxOccurs=1024,
                                               default = 'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20060101-20251231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20260101-20501231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20510101-20751231.nc,' +
                                                          'http://opendap.knmi.nl/knmi/thredds/dodsC/IS-ENES/TESTSETS/tas_day_EC-EARTH_rcp26_r8i1p1_20760101-21001231.nc')

        self.timeRangeStudyPeriodIn = self.addLiteralInput(identifier = 'timeRangeStudyPeriod', 
                                               title = 'Time range, e.g. 2010-01-01/2012-12-31',
                                               abstract = "Time range is mandatory. Please fill in.",
                                               type=type("String"),
                                               default = '2010-01-01/2012-12-31')                                                 
        
        
        self.varNameIn = self.addLiteralInput(identifier = 'varName',
                                               title = 'Variable name to process',
                                               type=type("String"),
                                               default = 'tas')

        
        self.leapNonLeapYearsIn = self.addLiteralInput(
            identifier = 'leapNonLeapYears',
            title = 'Take only leap years?',
            abstract = "Method for computing a percentile value for the calendar day of February 29th. Default: take all years (leap and non-leap)",
            type=type(False),
            minOccurs=1,
            maxOccurs=1,
            default = False)
        #self.leapNonLeapYearsIn.values = ["take all years (leap and non-leap)", "take only leap years"]
        
        
        ## self.outputFileNameIn = self.addLiteralInput(identifier = 'outputFileName', 
        ##                                        title = 'Name of output netCDF file',
        ##                                        type=type("String"),
        ##                                        default = './out_icclim.nc')
        
        self.NLevelIn = self.addLiteralInput(
            identifier = 'NLevel', 
            title = 'Number of level (if 4D variable)',
            type=type(1),
            minOccurs = 0)

        #self.opendapURL = self.addLiteralOutput(identifier = "opendapURL",title = "opendapURL");

        self.output = self.addComplexOutput(
            identifier="output",
            title="Climate Indice",
            abstract="Calculated climate indice with icclim.",
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True)
        
    def callback(self,message,percentage):
        self.status.set("%s" % str(message),str(percentage));

    def execute(self):
        # Very important: This allows the NetCDF library to find the users credentials (X509 cert)
        #homedir = os.environ['HOME']
        #os.chdir(homedir)
        def callback(b):
          self.callback("Processing",b)
        
        indice_name = self.indiceNameIn.getValue()
        
        in_files = self.getInputValues(identifier='filesBasePeriod')      
        
        time_range_base_period = self.timeRangeBasePeriodIn.getValue()
        
        var_name = self.varNameIn.getValue()
                
        leap_nonleap_years = self.leapNonLeapYearsIn.getValue()
        
        in_files.extend(self.getInputValues(identifier='filesStudyPeriod'))
        
        time_range_study_period = self.timeRangeStudyPeriodIn.getValue()

        slice_mode = self.sliceModeIn.getValue()
        #out_file_name = self.outputFileNameIn.getValue()
        out_file_name = out.nc
        level = self.NLevelIn.getValue()
          
        if time_range_base_period:
            startdate = dateutil.parser.parse(time_range_base_period.split("/")[0])
            stopdate  = dateutil.parser.parse(time_range_base_period.split("/")[1])
            time_range_base_period = [startdate,stopdate]
                
        if time_range_study_period:
            startdate = dateutil.parser.parse(time_range_study_period.split("/")[0])
            stopdate  = dateutil.parser.parse(time_range_study_period.split("/")[1])
            time_range_study_period = [startdate,stopdate]
        
        ## if (leap_nonleap_years == "take all years (leap and non-leap)"):
        ##     leap_nonleap_years = False
        ## else:
        ##     leap_nonleap_years = True 
        
        
        #home = expanduser("~")
        
        self.status.set("Preparing....", 0)
        
        #pathToAppendToOutputDirectory = "/WPS_"+self.identifier+"_" + datetime.now().strftime("%Y%m%dT%H%M%SZ")
        
        """ URL output path """
        #fileOutURL  = os.environ['POF_OUTPUT_URL']  + pathToAppendToOutputDirectory+"/"
        
        """ Internal output path"""
        #fileOutPath = os.environ['POF_OUTPUT_PATH']  + pathToAppendToOutputDirectory +"/"

        """ Create output directory """
        #mkdir_p(fileOutPath)
        
        self.status.set("Processing input list: " + str(in_files), 0)
        
        icclim.indice(indice_name=indice_name,
                      in_files=in_files,
                      var_name=var_name,
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
                      netcdf_version='NETCDF4_CLASSIC',
                      out_unit='days')

        
        
        """ Set output """
        #url = fileOutURL+"/"+out_file_name;
        #self.opendapURL.setValue(url);
        self.output.setValue(out_file_name)
        self.status.set("ready",100);
        
        
