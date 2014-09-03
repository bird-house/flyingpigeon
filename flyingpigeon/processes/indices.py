#from malleefowl.process import WorkerProcess
import malleefowl.process 
import subprocess
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)



class IndicesProcess(malleefowl.process.WorkerProcess):
    """This process calculates the relative humidity"""

    def __init__(self):
        # definition of this process
        malleefowl.process.WorkerProcess.__init__(self, 
            identifier = "de.csc.indices",
            title="Calculation of climate indices",
            version = "0.1",
            #storeSupported = "true",   # async
            #statusSupported = "true",  # retrieve status, needs to be true for async 
            ## TODO: what can i do with this?
            metadata=[
                {"title":"Foobar","href":"http://foo/bar"},
                {"title":"Barfoo","href":"http://bar/foo"},
                {"title":"Literal process"},
                {"href":"http://foobar/"}],
            abstract="Just testing a python script to  ...",
            #extra_metadata={
                  #'esgfilter': 'variable:tas, variable:evspsblpot, variable:huss, variable:ps, variable:pr, variable:sftlf, time_frequency:day', 
                  #'esgquery': 'data_node:esg-dn1.nsc.liu.se' 
                  #},
            extra_metadata={
                  'esgfilter': 'variable:tas,variable:pr',  #institute:MPI-M,
                  'esgquery': 'time_frequency:day AND project:CORDEX' # institute:MPI-M 
                  },
            )

        # Literal Input Data
        # ------------------
        
        self.tasThreshold = self.addLiteralInput(
            identifier="tasThreshold",
            title="Base temperature",
            abstract="Threshold for termal vegetation period",
            default="5.6",
            type=type(0.1),
            minOccurs=1,
            maxOccurs=1,
            )

            
        self.prThreshold = self.addLiteralInput(
            identifier="prThreshold",
            title="Threshold precipitation (mm)",
            abstract="Threshold for heavy daily precipitation",
            default="20",
            type=type(0.1),
            minOccurs=1,
            maxOccurs=1,
            )

        self.tas_yearmean = self.addLiteralInput(
            identifier="tas_yearmean",
            title="annual mean temperature (K)",
            abstract="annual mean temperature",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        self.pr_yearsum = self.addLiteralInput(
            identifier="pr_yearsum",
            title="annual precipitation sum (mm)",
            abstract="annual precipitation sum",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        self.tas_5to9mean = self.addLiteralInput(
            identifier="tas_5to9mean",
            title="tas_5to9mean",
            abstract="mean temperature (K) form Mai to September",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )

        self.tas_6to8mean = self.addLiteralInput(
            identifier="tas_6to8mean",
            title="tas_6to8mean",
            abstract="mean temperature (K) form Juni to August",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        self.pr_5to9sum = self.addLiteralInput(
            identifier="pr_5to9sum",
            title="pr_5to9sum",
            abstract="precipitation sum (mm) form Mai to September",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        self.pr_6to8sum = self.addLiteralInput(
            identifier="pr_6to8sum",
            title="pr_6to8sum",
            abstract="precipitation sum (mm) form Juni to August",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        self.heavyprecip = self.addLiteralInput(
            identifier="heavyprecip",
            title="heavy precipitation",
            abstract="Number of days with heavy precipitation",
            type=type(False),
            minOccurs=1,
            maxOccurs=0,
            )
            
        # defined in WorkflowProcess ...

        # complex output
        # -------------

        self.output = self.addComplexOutput(
            identifier="output",
            title="Indices Output tar",
            abstract="Indices Output file",
            metadata=[],
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )
            
    def execute(self):
        
        from netCDF4 import Dataset
        from os import curdir, path
        import numpy as np
        from cdo import *
        import datetime 
        import tarfile

        cdo = Cdo()
        
        self.show_status('starting indices ...', 5)
        
        
        # get the appropriate files
        nc_files = self.get_nc_files()
        for nc_file in nc_files: 
            ds = Dataset(nc_file)
            if "tas" in ds.variables.keys():
                tasFilePath = nc_file
            elif "pr" in ds.variables.keys():
                prFilePath = nc_file
            else:
                raise Exception("input netcdf file has not variable tas|pr")
        
        self.show_status('get files ...', 7)
        

        #tasFilePath = '/home/main/sandbox/climdaps/parts/files/tas_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'       
        #prFilePath = '/home/main/sandbox/climdaps/parts/files/pr_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'        

        tasFile = Dataset(tasFilePath , 'r')        
        prFile = Dataset(prFilePath ,'r')
        output_files = list()
        
        self.show_status('open files ...', 10)
        
        # get the dimensions
        # dimNames = tasFile.dimensions.keys()

        # simple precesses realized by cdo commands:
        if self.tas_yearmean.getValue() == True :  
            tas_yearmean_filename = self.mktempfile(suffix='_tas_yearmean.nc')
            output_files.append(tas_yearmean_filename)
            cdo.yearmean(input= tasFilePath, options='-f nc', output = tas_yearmean_filename)
            self.show_status("tas_yearmean done", 50)
        
        if self.pr_yearsum.getValue() == True :
            pr_yearsum_filename = self.mktempfile(suffix='_pr_yearsum.nc')
            output_files.append(pr_yearsum_filename)
            cdo.yearsum(input= prFilePath, options='-f nc', output = pr_yearsum_filename )
            self.show_status("pr_yearsum done", 50)
            
        if self.tas_5to9mean.getValue() == True :
            tas_5to9mean_filename = self.mktempfile(suffix='_tas_5to9mean.nc')
            output_files.append(tas_5to9mean_filename)
            cdo.setname('tas_5to9mean',input = "-yearmean -selmon,5,6,7,8,9 "+ tasFilePath , output = tas_5to9mean_filename, options =  '-f nc')  #python
            self.show_status("tas_5to9mean done", 50)
            
        if self.tas_6to8mean.getValue() == True :
            tas_6to8mean_filename = self.mktempfile(suffix='_tas_6to8mean.nc')
            output_files.append(tas_6to8mean_filename)
            cdo.setname('tas_6to8mean',input = "-yearmean -selmon,6,7,8 "+ tasFilePath , output = tas_6to8mean_filename, options =  '-f nc')  #python
            self.show_status("tas_6to8mean done", 50)
            
        if self.pr_5to9sum.getValue() == True :
            pr_5to9sum_filename = self.mktempfile(suffix='_pr_5to9sum.nc')
            output_files.append(pr_5to9sum_filename)
            cdo.setname('pr_5to9sum',input = "-yearsum -selmon,5,6,7,8,9 "+ prFilePath , output = pr_5to9sum_filename, options =  '-f nc')  #python
            self.show_status("pr_5to9sum done", 50)
            
        if self.pr_6to8sum.getValue() == True :
            pr_6to8sum_filename = self.mktempfile(suffix='_pr_6to8sum.nc')
            output_files.append(pr_6to8sum_filename)
            cdo.setname('pr_6to8sum',input = "-yearsum -selmon,6,7,8 "+ prFilePath , output = pr_6to8sum_filename, options =  '-f nc')  #python
            self.show_status("pr_6to8sum done", 50)

        if self.heavyprecip.getValue() == True :  
            prThreshold_filename = self.mktempfile(suffix='_prThreshold.nc')
            days_heavyprecip_filename = self.mktempfile(suffix='_days_heavyprecip.nc')
            output_files.append(days_heavyprecip_filename)
            cdo.gtc(str(self.prThreshold.getValue() / 60 / 60 / 24), input = prFilePath, options='-f nc', output = prThreshold_filename)
            cdo.yearsum(input = prThreshold_filename, options='-f nc', output = days_heavyprecip_filename)
            self.show_status("heavy precipitation days done", 50)

            #pr_6to8sum = np.squeeze(cdo.yearsum(input  =  " ".join([cdo.selmon('6,7,8',input  =  prFilePath)] ), options='-f nc', returnMaArray='pr'))  #python
            #pr_6to8sum = pr_6to8sum * 60 * 60 * 24 # convert flux to amount            
            #pr_6to8sumFile = Dataset(pr_6to8sum_filename , 'w')
            #pr_6to8sumTimeDim = pr_6to8sumFile.createDimension('time', pr_6to8sum.shape[0])
            #pr_6to8sumLatDim = pr_6to8sumFile.createDimension('lat', pr_6to8sum.shape[1])
            #pr_6to8sumLonDim = pr_6to8sumFile.createDimension('lon', pr_6to8sum.shape[2])
            #dims = ('time','lat','lon')
            #pr_6to8sumVar = pr_6to8sumFile.createVariable('pr_6to8sum', 'f', dims)
            #pr_6to8sumVar.assignValue(pr_6to8sum)
            #pr_6to8sumFile.close()
        
        # more sufesticated processes
        # get the raw values into memory: 

        #dates = str(cdo.showdate(input=tasFilePath)).replace("'","").replace(']','').replace('[','').split('  ')
        #for xx,date in enumerate(dates):
            #date = date.split('-')
            #dates[xx] = datetime.date(int(date[0]),int(date[1]),int(date[2]))

        #tas = np.squeeze(tasFile.variables["tas"])
        #tas = tas - 273.15
        #pr = np.squeeze(prFile.variables["pr"])
        #pr = pr  * 60 * 60 * 24
        
        ## calculation of running mean 
        #tas_runmean = np.squeeze(cdo.runmean(11,input=tasFilePath, options='-f nc', returnMaArray='tas'))
        
        # close inFiles
        tasFile.close()
        prFile.close() 
        
        self.show_status("input files closed", 50)

        # make tar archive
        tar_archive = self.mktempfile(suffix='.tar')
        tar = tarfile.open(tar_archive, "w")
        for name in output_files:
            tar.add(name, arcname = name.replace(self.working_dir, ""))
        tar.close()
        
        self.show_status("make tar archive ... done", 50)
        logger.debug('tar archive = %s' %(tar_archive))
        
        #mystring.replace('\r\n', '')
        
        # output
        self.show_status("processing done", 52)
        self.output.setValue( tar_archive )
        
        self.show_status("result published", 100)
        
