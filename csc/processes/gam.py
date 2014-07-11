"""
Processes for rel_hum 
Author: Nils Hempelmann (nils.hempelmann@hzg)
"""

from datetime import datetime, date
#from malleefowl.process import WorkerProcess
import malleefowl.process 
import subprocess

class GamProcess(malleefowl.process.WorkerProcess):
    """This process calculates the relative humidity"""

    def __init__(self):
        # definition of this process
        malleefowl.process.WorkerProcess.__init__(self, 
            identifier = "de.csc.gam",
            title="Species distribution modell ",
            version = "0.1",
            #storeSupported = "true",   # async
            #statusSupported = "true",  # retrieve status, needs to be true for async 
            ## TODO: what can i do with this?
            metadata=[
                {"title":"Foobar","href":"http://foo/bar"},
                {"title":"Barfoo","href":"http://bar/foo"},
                {"title":"Literal process"},
                {"href":"http://foobar/"}],
            abstract="Calculation of species distribution",
            extra_metadata={
                  'esgquery': 'variable:tas OR variable:pr ', # institute:MPI-M 
                  'esgfilter': 'project:CORDEX,time_frequency:mon'  #institute:MPI-M,
                  },
           )

        # Literal Input Data
        # ------------------

        self.period_in = self.addLiteralInput(
            identifier="period",
            title="Select period",
            abstract="Select between reference or projection period",
            default="reference",
            type=type(''),
            minOccurs=1,
            maxOccurs=1,
            allowedValues=['reference','projection']
            )
        
        self.R_in = self.addComplexInput(
            identifier="R_in",
            title="PA-Data or RData",
            abstract="Files: PA-Data.csv for reverence and RData for projection (5MB max)",
            metadata=[],
            minOccurs=1,
            maxOccurs=1,
            maxmegabites=5,
            formats=[{"mimeType":"text/csv"}],
            upload=True,
            )

        self.TG = self.addLiteralInput(
            identifier="TG",
            title="TG",
            abstract="Mean of mean temperatur (tas files as input files)",
            type=type(1),
            default="3",
            minOccurs=0,
            maxOccurs=0,
            )
            
        self.TX = self.addLiteralInput(
            identifier="TX",
            title="TX",
            abstract="mean of max temperatur (tasmax files as input files)",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
            
        self.TN = self.addLiteralInput(
            identifier="TN",
            title="TN",
            abstract="Mean over min temperatur (tasmin files as input files)",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
        
        self.RR = self.addLiteralInput(
            identifier="RR",
            title="RR",
            abstract="precipitation sum (pr files as input files) ",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )

        self.TG_5to9 = self.addLiteralInput(
            identifier="TG_5to9",
            title="TG_5to9",
            abstract="mean temperature (K) form Mai to September",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )

        self.TG_6to8 = self.addLiteralInput(
            identifier="TG_6to8",
            title="TG_6to8",
            abstract="mean temperature (K) form Juni to August",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
            
        self.RR_5to9 = self.addLiteralInput(
            identifier="RR_5to9",
            title="RR_5to9",
            abstract="precipitation sum (mm) form Mai to September",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
            
        self.RR_6to8 = self.addLiteralInput(
            identifier="RR_6to8",
            title="RR_6to8",
            abstract="precipitation sum (mm) form Juni to August",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
            
            
        self.SU = self.addLiteralInput(
            identifier="SU",
            title="SU",
            abstract="Nr of summer days (tasmax files as input files)",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
            
            
        #self.climin1 = self.addLiteralInput(
            #identifier="climin1",
            #title="mean temperature June to August",
            #abstract="Kappa Value (choose 0 if Indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)
            
        #self.climin2 = self.addLiteralInput(
            #identifier="climin2",
            #title="mean temperature May to September",
            #abstract="Kappa Value (choose 0 if indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)
            
        #self.climin3 = self.addLiteralInput(
            #identifier="climin3",
            #title="precipitation sum June to August",
            #abstract="Kappa Value (choose 0 if indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)

        #self.climin4 = self.addLiteralInput(
            #identifier="climin4",
            #title="precipitation sum May to September",
            #abstract="Kappa Value (choose 0 if indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)
            
        #self.climin5 = self.addLiteralInput(
            #identifier="climin5",
            #title="mean temperature of coldest month",
            #abstract="Kappa Value (choose 0 if indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)
            
        #self.climin6 = self.addLiteralInput(
            #identifier="climin6",
            #title="precipitation sum of dyest month",
            #abstract="Kappa Value (choose 0 if indice should not be used)",
            #default="0",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['0','1','2','3','4','5','6','7','8','9','10','11','12']
            #)
  
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
        # from os import os.curdir, os.path, system
        import os 
        import numpy as np
        from cdo import *
        import datetime
        import string
        
        cdo = Cdo()
        
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

        #tasFilePath = '/home/main/sandbox/climdaps/parts/files/tas_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'       
        #prFilePath = '/home/main/sandbox/climdaps/parts/files/pr_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'        

        #tasFile = Dataset(tasFilePath , 'r')        
        #prFile = Dataset(prFilePath ,'r')
        
        c_files = list()
        c_kappa = list()
        c_names = list()
        
        if int(self.climin1.getValue()) > 0 :  
            c1_file = self.mktempfile(suffix='.nc')
            c1_temp = self.mktempfile(suffix='.nc')
            c_files.append(c1_file)
            c_kappa.append(self.climin1.getValue())
            c_names.append('c1')
            cdo.selmon('6,7,8', input= tasFilePath, options='-f nc', output = c1_temp)
            cdo.yearmean(input= c1_temp, options='-f nc', output = c1_file)
            self.status.set(msg="c1 done", percentDone=10, propagate=True)

        if int(self.climin2.getValue()) > 0 :  
            c2_file = self.mktempfile(suffix='.nc')
            c2_temp = self.mktempfile(suffix='.nc')
            c_files.append(c2_file)
            c_kappa.append(self.climin2.getValue())
            c_names.append('c2')
            cdo.selmon('5,6,7,8,9' , input= tasFilePath, options='-f nc', output = c2_temp)
            cdo.yearmean(input= c2_temp, options='-f nc', output = c2_file)
            self.status.set(msg="c2 done", percentDone=20, propagate=True)

        if int(self.climin3.getValue()) > 0 :  
            c3_file = self.mktempfile(suffix='.nc')
            c3_temp = self.mktempfile(suffix='.nc')
            c3_temp2 = self.mktempfile(suffix='.nc')
            c3_temp3 = self.mktempfile(suffix='.nc')
            c_files.append(c3_file)
            c_kappa.append(self.climin3.getValue())
            c_names.append('c3')
            cdo.selmon('6,7,8' , input= prFilePath, options='-f nc', output = c3_temp)
            cdo.mulc('86400', input= c3_temp, options='-f nc', output = c3_temp2)
            cdo.muldpm( input= c3_temp2, options='-f nc', output = c3_temp3)
            cdo.yearsum(input= c3_temp3, options='-f nc', output = c3_file)
            self.status.set(msg="c3 done", percentDone=30, propagate=True)

        if int(self.climin4.getValue()) > 0 :  
            c4_file = self.mktempfile(suffix='.nc')
            c4_temp = self.mktempfile(suffix='.nc')
            c_files.append(c4_file)
            c_kappa.append(self.climin4.getValue())
            c_names.append('c4')
            cdo.selmon('5,6,7,8,9' , input= prFilePath, options='-f nc', output = c4_temp)
            cdo.yearsum(input= c4_temp, options='-f nc', output = c4_file)
            self.status.set(msg="c4 done", percentDone=40, propagate=True)
            
        if int(self.climin5.getValue()) > 0 :  
            c5_file = self.mktempfile(suffix='.nc')
            c_files.append(c5_file)
            c_kappa.append(self.climin5.getValue())
            c_names.append('c5')
            cdo.yearmin(input= tasFilePath, options='-f nc', output = c5_file)
            self.status.set(msg="c5 done", percentDone=50, propagate=True)

        if int(self.climin6.getValue()) > 0 :  
            c6_file = self.mktempfile(suffix='.nc')
            c_files.append(c6_file)
            c_kappa.append(self.climin6.getValue())
            c_names.append('c6')
            cdo.yearmin(input= prFilePath, options='-f nc', output = c6_file)
            self.status.set(msg="c6 done", percentDone=50, propagate=True)
        
        if self.period_in.getValue() == 'reference' :
            rworkspace = self.mktempfile(suffix='.RData')
            paData = self.R_in.getValue()
            c_nr = str(len(c_files))
            names = string.join(c_names," ")
            files = string.join(c_files," ")
            kappa = string.join(c_kappa," ")
            #cmd=["R --no-save --args %s %s %s %s %s %s < %s/gam_reference.r " % (rworkspace, paData, c_nr, names , files , kappa , workdir)]
            #self.cmd(cmd=cmd, stdout=True)
            os.system("R --no-save --args %s %s %s %s %s %s  < ./../../../../src/Malleefowl/processes/gam_reference.r" % (rworkspace, paData, c_nr, names , files , kappa ))
            self.output.setValue( rworkspace )
        
        if self.period_in.getValue() == 'projection' :
            out_pdf = self.mktempfile(suffix='.pdf')
            #summary = self.mktempfile(suffix='.pdf')
            rworkspace = self.R_in.getValue()
            c_nr = str(len(c_files))
            names = string.join(c_names," ")
            files = string.join(c_files," ")
            kappa = string.join(c_kappa," ")
            os.system("R --no-save --args %s %s %s %s %s %s < ./../../../../src/Malleefowl/processes/gam_projection.r " % (out_pdf, rworkspace, c_nr, names , files , kappa ))
            self.output.setValue( out_pdf )
        
        # from os import os.curdir, os.path
        # nc_filename = os.path.absos.path(self.netcdf_in.getValue(asFile=False))
        #result = self.cmd(cmd=["/home/main/sandbox/climdaps/src/Malleefowl/processes/dkrz/gam_job.sh", self.os.path_in.getValue(), self.stringIn.getValue(), self.individualBBoxIn.getValue(), self.start_date_in.getValue(), self.end_date_in.getValue()], stdout=True)
        ## literals
        # subprocess.check_output(["/home/main/sandbox/climdaps/src/ClimDaPs_WPS/processes/dkrz/rel_hum.sh", self.os.path_in.getValue(), self.stringIn.getValue(), self.individualBBoxIn.getValue(), self.start_date_in.getValue(),   self.end_date_in.getValue()])
        #self.file_out_ref.setValue("/home/main/wps_data/gam_ref.nc")
        #self.file_out_pred.setValue("/home/main/wps_data/gam_pred.nc")
        #self.output.setValue( c6_file )
        
        
