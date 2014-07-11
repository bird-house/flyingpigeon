"""
Processes with cdo commands

Author: Carsten Ehbrecht (ehbrecht@dkrz.de)
"""

#from malleefowl.process import WorkerProcess
import malleefowl.process
import os 

class nc2geotiff(malleefowl.process.WorkerProcess):
    """This process calls cdo with operation on netcdf file"""
    def __init__(self):
        malleefowl.process.WorkerProcess.__init__(
            self,
            identifier = "de.dkrz.cdo.nc2geotiff",
            title = "Transform netCDF to geoTiff",
            version = "0.1",
            metadata=[
                {"title":"CDO","href":"https://code.zmaw.de/projects/cdo"},
                ],
            abstract="Transform netCDF format to geoTiff format",
            )

        # complex output
        # -------------

        self.output = self.addComplexOutput(
            identifier="geotiff",
            title="geoTiff Output",
            abstract="geoTiff Output",
            metadata=[],
            formats=[{"mimeType":"image/tiff"}],
            asReference=True,
            )

    def execute(self):
        self.status.set(msg="starting gdal operator", percentDone=10, propagate=True)

        nc_file = self.get_nc_files()[0]
        out_file = self.mktempfile(suffix='.tif')
       
        from osgeo import gdal

        #Open existing dataset
        src_ds = gdal.Open( nc_file )

        #Open output format driver, see gdal_translate --formats for list
        format = "GTiff"
        driver = gdal.GetDriverByName( format )

        #Output to new format
        dst_ds = driver.CreateCopy( out_file , src_ds, 0 )

        #Properly close the datasets to flush to disk
        dst_ds = None
        src_ds = None
        
        
 
        #try:
            #os.system("gdal_translate -of GTiff -sds %s %s" % ( nc_file[0] , out_filename) )
            ##cmd = ["gdal_translate -of GTiff -sds", nc_file[0] , out_filename]
            ##self.cmd(cmd=cmd, stdout=True)
        #except:
            #self.message(msg='transformation failed', force=True)

        self.status.set(msg="gdal transforamtion done", percentDone=90, propagate=True)
        self.output.setValue(  out_file )

