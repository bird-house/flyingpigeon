"""
Processes for rel_hum 
Author: Nils Hempelmann (nils.hempelmann@hzg)
"""

from malleefowl.process import WPSProcess
from malleefowl import wpslogging as logging
import malleefowl.process 
logger = logging.getLogger(__name__)

class gam(WPSProcess):
    
    def __init__(self):
        WPSProcess.__init__(
            self,
            identifier = "gam",
            title = "Species destribution model",
            version = "0.1",
            metadata=[
                {"title":"GAM"},
                ],
            abstract="Species destribution model based on PA - Data",
            )


        # Literal Input Data
        # ------------------

        self.netcdf_file = self.addComplexInput(
            identifier="netcdf_file",
            title="NetCDF File",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        #self.period_in = self.addLiteralInput(
            #identifier="period",
            #title="Select period",
            #abstract="Select between reference or projection period",
            #default="reference",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #allowedValues=['reference','projection']
            #)
        
        #self.paData = self.addComplexInput(
            #identifier="paData",
            #title="PA-Data",
            #abstract="Files: PA-Data.csv for reverence (5MB max)",
            #metadata=[],
            #minOccurs=1,
            #maxOccurs=1,
            #maxmegabites=5,
            #formats=[{"mimeType":"text/csv"}],
            #upload=True,
            #)

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
            
        self.SU = self.addLiteralInput(
            identifier="SU",
            title="SU",
            abstract="Nr of summer days (tasmax files as input files)",
            default="0",
            type=type(1),
            minOccurs=0,
            maxOccurs=0,
            )
  
        self.output = self.addComplexOutput(
            identifier="output",
            title="Indices Output tar",
            abstract="Indices Output file",
            metadata=[],
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )
        
    def execute(self):
      
      import subprocess
      from datetime import datetime, date


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

        #for nc_file in nc_files: 
            #ds = Dataset(nc_file)
            #if "tas" in ds.variables.keys():
                #tasFilePath = nc_file
            #elif "pr" in ds.variables.keys():
                #prFilePath = nc_file
            #else:
                #raise Exception("input netcdf file has not variable tas|pr")

        # call to icclim
       
        from flyingpigeon import tools
       
        indices_dic = { ('outdir':os.curdir() , ('nc_files': nc_files ), ('TG':self.TG.getValue()) ,('TX': self.TX.getValue()),
            ('TN':self.TN.getValue()),('RR': self.RR.getValue()),('SU':self.SU.getValue()) }
       
        indices_out, indices_log = tools.indices( indices_dic )
        
        # calculation of indice avg for reverence period 
        
        time_frame= 
        cal = 
        cal_group = 
        # ocgis execute : 
        indices_ref = []
        indices_names = []
        
        indices_ref.append(ocgis_output)

        # train historical run 
        
        (fp_rwork, rwork) = tempfile.mkstemp(dir=".", suffix='.RData')
        
        paData = self.paData.getValue()
        
        c_nr = str(len(c_files))
        names = string.join(c_names," ")
        files = string.join(c_files," ")
        kappa = string.join(c_kappa," ")
        
        cmd=["R --no-save --args %s %s %s %s %s %s < %s/sdm.r " % (rworkspace, self.paData.getValue(), indices_ref, indices_names, kappa, workdir)]
        
        self.output.setValue( rwork )
        
  
