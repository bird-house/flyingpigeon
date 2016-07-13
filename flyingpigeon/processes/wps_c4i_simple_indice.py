from pywps.Process import WPSProcess

import icclim
import icclim.util.callback as callback

import dateutil.parser
from datetime import datetime
import os
from os.path import expanduser

from flyingpigeon.utils import make_dirs

transfer_limit_Mb = 100

import logging
logger = logging.getLogger()
    
class ProcessSimpleIndice(WPSProcess):


    def __init__(self):
        WPSProcess.__init__(self,
                            identifier = 'wps_c4i_simple_indice', # only mandatary attribute = same file name
                            title = 'c4i - Simple Climate Indices',
                            abstract = 'Computes single input indices of temperature TG, TX, TN, TXx, TXn, TNx, TNn, SU, TR, CSU, GD4, FD, CFD, ID, HD17; of rainfal: CDD, CWD, RR, RR1, SDII, R10mm, R20mm, RX1day, RX5day; and of snowfall: SD, SD1, SD5, SD50. This processes is also available in Climate4Impact and uses ICCLIM.',
                            version = "1.0",
                            metadata = [
                                {"title": "ICCLIM" , "href": "http://icclim.readthedocs.io/en/latest/"},
                                {"title": "Climate4Impact", "href": "http://climate4impact.eu/impactportal/general/index.jsp"},
                            ],
                            storeSupported = True,
                            statusSupported = True,
                            grassLocation =False)


        ## self.filesIn = self.addLiteralInput(identifier = 'files',
        ##                                        title = 'Input netCDF files list',
        ##                                        abstract="application/netcdf",
        ##                                        type=type("S"),
        ##                                        minOccurs=0,
        ##                                        maxOccurs=1024,
        ##                                        default = 'http://aims3.llnl.gov/thredds/dodsC/cmip5_css02_data/cmip5/output1/CMCC/CMCC-CM/rcp85/day/atmos/day/r1i1p1/tasmax/1/tasmax_day_CMCC-CM_rcp85_r1i1p1_20060101-20061231.nc')

        self.filesIn = self.addComplexInput(
            identifier="files",
            title="URL to your NetCDF File",
            abstract="You may provide a URL or upload a NetCDF file.",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=10000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.indiceNameIn = self.addLiteralInput(identifier = 'indiceName',
                                               title = 'Indice name',
                                               type = type("String"),
                                               minOccurs=1,
                                               maxOccurs=1,
                                               default = 'SU')        

        self.indiceNameIn.values = ["TG","TX","TN","TXx","TXn","TNx","TNn","SU","TR","CSU","GD4","FD","CFD","ID","HD17","CDD","CWD","PRCPTOT","RR1","SDII","R10mm","R20mm","RX1day","RX5day","SD","SD1","SD5cm","SD50cm"]


        self.sliceModeIn = self.addLiteralInput(identifier = 'sliceMode',
                                              title = 'Slice mode (temporal grouping to apply for calculations)',
                                              type = type("String"),
                                              default = 'year')
        self.sliceModeIn.values = ["year","month","ONDJFM","AMJJAS","DJF","MAM","JJA","SON"]


        self.thresholdIn = self.addLiteralInput(identifier = 'threshold', 
                                               title = 'Optional threshold(s) for certain indices (SU, CSU and TR). Can be a comma separated list, e.g. 20,21,22',
                                               type=type("S"),
                                               minOccurs=0,
                                               maxOccurs=50)

       
      
        
                                                
        self.varNameIn = self.addLiteralInput(identifier = 'varName',
                                               title = 'Variable name to process',
                                               type=type("String"),
                                               minOccurs=1,
                                               maxOccurs=1,
                                               default = 'tasmax')
        

        self.timeRangeIn = self.addLiteralInput(identifier = 'timeRange', 
                                               title = 'Optional time range, e.g. 2010-01-01/2012-12-31. If no time range is given then all dates in the file are taken.',
                                               type=type("String"),
                                               minOccurs=0,
                                               maxOccurs=1)
        
        ## self.outputFileNameIn = self.addLiteralInput(identifier = 'outputFileName', 
        ##                                        title = 'Name of output netCDF file',
        ##                                        type = type("String"),
        ##                                        minOccurs=1,
        ##                                        maxOccurs=1,
        ##                                        default = 'out_icclim.nc')
        
        
        self.NLevelIn = self.addLiteralInput(identifier = 'NLevel', 
                                               title = 'Number of level (if 4D variable)',
                                               type = type("String"),
                                               minOccurs=0,
                                               maxOccurs=1)

        self.output = self.addComplexOutput(
            identifier="output",
            title="Climate Indice",
            abstract="Calculated climate indice with icclim.",
            formats=[{"mimeType":"application/x-netcdf"}],
            asReference=True,
        )
        
        #self.opendapURL = self.addLiteralOutput(identifier = "opendapURL",title = "opendapURL");   
        
    def callback(self,message,percentage):
        self.status.set("%s" % str(message),str(percentage));

    
    def execute(self):
        # Very important: This allows the NetCDF library to find the users credentials (X509 cert)
        #homedir = os.environ['HOME']
        #os.chdir(homedir)
        
        def callback(b):
          self.callback("Processing",b)
         
        files = self.getInputValues(identifier='files')
        var = self.varNameIn.getValue()
        indice_name = self.indiceNameIn.getValue()
        slice_mode = self.sliceModeIn.getValue()
        time_range = self.timeRangeIn.getValue()
        #out_file_name = self.outputFileNameIn.getValue()
        out_file_name = 'out.nc'
        level = self.NLevelIn.getValue()
        thresholdlist = self.getInputValues(identifier='threshold')
        
        if (time_range):
            startdate = dateutil.parser.parse(time_range.split("/")[0])
            stopdate  = dateutil.parser.parse(time_range.split("/")[1])
            time_range = [startdate,stopdate]

        logger.debug("time_range: %s", time_range)
        
        thresh = None
        if(thresholdlist):
            thresh = [float(threshold) for threshold in threshholdList]

        logger.debug("thresh: %s", thresh)
        
      
        self.status.set("Preparing....", 0)
        
        #pathToAppendToOutputDirectory = "/WPS_"+self.identifier+"_" + datetime.now().strftime("%Y%m%dT%H%M%SZ")
        
        """ URL output path """
        from flyingpigeon import config
        
        #fileOutURL  = os.environ['POF_OUTPUT_URL']  + pathToAppendToOutputDirectory+"/"
        #fileOutURL  = config.outputUrl_path()  + pathToAppendToOutputDirectory+"/"
        
        """ Internal output path"""
        #fileOutPath = os.environ['POF_OUTPUT_PATH']  + pathToAppendToOutputDirectory +"/"
        #fileOutPath = config.output_path()  + pathToAppendToOutputDirectory +"/"

        """ Create output directory """
        #make_dirs(fileOutPath)
        

        self.status.set("Processing input list: "+str(files),0)
        
        icclim.indice(indice_name=indice_name,
                        in_files=files,
                        var_name=var,
                        slice_mode=slice_mode,
                        time_range=time_range,
                        out_file=out_file_name,
                        threshold=thresh,
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
        #url = fileOutURL+"/"+out_file_name;
        #self.opendapURL.setValue(url);
        self.output.setValue(out_file_name);
        self.status.set("ready",100);
        
        
