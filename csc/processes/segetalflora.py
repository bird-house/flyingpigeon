"""
Processes for Anopheles Gambiae population dynamics 
Author: Nils Hempelmann (nils.hempelmann@hzg)
"""

from datetime import datetime, date
import tempfile
import subprocess

#from malleefowl.process import WorkerProcess
import malleefowl.process 

class SegetalfloraProcess(malleefowl.process.WorkerProcess):
    """This process calculates the evapotranspiration following the Pennan Monteith equation"""

    def __init__(self):
        # definition of this process
        malleefowl.process.WorkerProcess.__init__(self, 
            identifier = "de.csc.esgf.segetalflora",
            title="Number of Segetal flora species",
            version = "0.1",
            metadata= [
                       {"title": "Climate Service Center", "href": "http://www.climate-service-center.de/"}
                      ],
            abstract="Just testing a nice script for estimation the number of segetal flora species",
            extra_metadata={ 
                  'esgquery': 'variable:tas OR variable:sftlf AND domain:EUR-11i OR domain:EUR-44i' , 
                  'esgfilter': 'project:CORDEX'  # data_node:esg-dn1.nsc.liu.se
                  },
            )
            
        # Literal Input Data
        # ------------------

        self.output = self.addComplexOutput(
            identifier="output",
            title="Segetalflora",
            abstract="Calculated number of segetal flora species",
            formats=[{"mimeType":"application/netcdf"}],
            asReference=True,
            )         
        from netCDF4 import Dataset
        from os import curdir, path
        import numpy as np
        from cdo import *
        import datetime 
        import tarfile

        cdo = Cdo()
        
        # get the appropriate files
        #nc_files = self.get_nc_files()
        #for nc_file in nc_files: 
            #ds = Dataset(nc_file)
            #if "tas" in ds.variables.keys():
                #tasFilePath = nc_file
            #else:
                #raise Exception("input netcdf file has not variable tas|pr")

        ##tasFilePath = '/home/main/sandbox/climdaps/parts/files/tas_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'       
        ##prFilePath = '/home/main/sandbox/climdaps/parts/files/pr_AFR-44_MPI-ESM-LR_rcp85_r1i1p1_MPI-RCSM-v2012_v1_day_20060101_20101231.nc'        

        #tasFile = Dataset(tasFilePath , 'r')        
        #output_files = list()
        
        ## get the dimensions
        ## dimNames = tasFile.dimensions.keys()

        ## 
        #(_, nc_gr1) = tempfile.mkstemp(suffix='_gr1.nc')
        #expr = "expr,\'gr1=((tas-273)/62.2)\'" % ('tas')
        #cmd = ['cdo', expr, merged_ps_huss, nc_e]
        #self.cmd(cmd=cmd, stdout=True)
        
        #output_files.append(nc_gr1)

        ## close inFiles
        #tasFile.close()

        ## make tar archive
        #tar_archive = self.mktempfile(suffix='.tar')
        #tar = tarfile.open(tar_archive, "w")
        #for name in output_files:
            #tar.add(name, arcname = name.replace(self.working_dir, ""))
        #tar.close()
        
        ##mystring.replace('\r\n', '')
        
        ## output
        #self.status.set(msg="processing done", percentDone=90, propagate=True)
        #self.output.setValue( tar_archive )
