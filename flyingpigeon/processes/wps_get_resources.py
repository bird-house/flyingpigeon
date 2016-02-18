from pywps.Process import WPSProcess
import logging

class GetResourceProcess(WPSProcess):

    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "get_resources",
            title="Download Resources",
            version = "0.1",
            abstract="This process downloads resources (limited to 50GB) to the local file system and returns a textfile with appropriate pathes",
            statusSupported=True,
            storeSupported=True
            )

        self.resource = self.addComplexInput(
            identifier="resource",
            title="Resource",
            abstract="NetCDF File",
            minOccurs=0,
            maxOccurs=1000,
            maxmegabites=50000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.output = self.addComplexOutput(
          identifier="output",
          title="Pathes File",
          abstract="File containing the local pathes to downloades files",
          formats=[{"mimeType":"text/plain"}],
          asReference=True,
          )

        def execute(self):

            from flyingpigeon import get_resources
            from tempfile import mkstemp
            from os.path import realpath

            self.status.set('starting write path files process', 0)
#            urls = self.getInputValues(identifier='resource')

            # if type(urls) == str: 
            #     urls = [urls]

            logger.info('got list of URLS ')

            try: 
                pid , textfile = mkstemp(suffix='.txt', prefix='pathes_', dir='.', text=True)
                pf = open(textfile, mode='w')

                logger.info('created the filepathes.txt file ')
                pf.write('###############################################\n')
                pf.write('###############################################\n')
                pf.write('Following files are stored to your local discs: \n')
                pf.write('\n')

                # try: 
                #   for url in urls: 
                #     pathes.write('%s \n' % realpath(url))
                # except Exception as e: 
                #   logger.error('failed to write path to file')

                # textfile = pathes.name
                pf.close()

                self.status.set('text file written', 90)
            except Exception as e: 
                logger.error('failed to write file pathes ')

            self.output.setValue( textfile )
            self.status.set('End of wps_get_resources process', 100)