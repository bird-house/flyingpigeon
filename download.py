from malleefowl.process import WorkerProcess
from malleefowl import utils, tokenmgr 
import os
import tarfile
from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class Download(WorkerProcess):
    """Publish netcdf files to thredds server"""
    def __init__(self):
        WorkerProcess.__init__(
            self,
            identifier = "org.malleefowl.download",
            title = "Download netCDF files from source",
            version = "0.1",
            metadata=[
                ],
            abstract="Preparing compressed data archive for download to local storge",
            )
            
            
        self.archformat = self.addLiteralInput(
            identifier="archformat",
            title="archive format",
            abstract="Choose the format of your archive",
            default="tar",
            type=type(''),
            minOccurs=0,
            maxOccurs=1,
            allowedValues=['tar', 'tar.gz']
            )
            
        self.token = self.addLiteralInput(
            identifier = "token",
            title = "Token",
            abstract = "Your unique token to publish data",
            minOccurs = 1,
            maxOccurs = 1,
            type = type('')
            )

        #self.basename = self.addLiteralInput(
            #identifier="basename",
            #title="Basename",
            #abstract="Basename of files",
            #type=type(''),
            #minOccurs=1,
            #maxOccurs=1,
            #)

        self.output = self.addComplexOutput(
            identifier="output",
            title="Output archive",
            abstract="Output archive file",
            metadata=[],
            formats=[{"mimeType":"application/x-tar"}],
            asReference=True,
            )
   
    def execute(self):
        self.show_status("starting download data", 10)

        #token = self.token.getValue()
        nc_files = self.get_nc_files()

        #userid = tokenmgr.get_userid(tokenmgr.sys_token(), token)        
        #outdir = os.path.join(self.files_path, userid)
        #utils.mkdir(outdir)
        
        count = 0
        
        # make tar archive
        if (self.archformat.getValue() == 'tar'):
            archive = self.mktempfile(suffix='.tar')
            tar = tarfile.open(archive, "w")
            for name in nc_files:
                tar.add(name, arcname = name.replace(self.working_dir, ""))
            tar.close()
    
        if (self.archformat.getValue() == 'tar.gz'):
            archive = self.mktempfile(suffix='tar.gz')
            tar = tarfile.open(archive, "w:gz")
            for name in nc_files:
                tar.add(name, arcname = name.replace(self.working_dir, ""))
            tar.close()
            
        self.show_status("make tar archive ... done", 50)
        
        # output
        self.output.setValue(archive)
        
        self.show_status("download done", 92)
        logger.debug('current tar archive = %s' %(archive))

        
