from pywps.Process import WPSProcess

import logging

class GetResourceProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(
            self, 
            identifier = "wps_get_resources",
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
            minOccurs=1,
            maxOccurs=1000,
            maxmegabites=50000,
            formats=[{"mimeType":"application/x-netcdf"}],
            )

        self.output_text = self.addComplexOutput(
            identifier="output_text",
            title="Sourcefiles",
            abstract="text file with a list of the used input data sets",
            formats=[{"mimeType":"text/plain"}],
            asReference=True,
            )


        def execute(self):

            from flyingpigeon import get_resources

            self.status.set('starting write path files process', 0)

            urls = self.getInputValues(identifier='resource')

            logger.info('urls: %s ' % urls)

            self.status.set('Path for %s fiels ' % len(urls), 20)

            try: 
               resource_txt = get_resources.write_file(urls)
            except Exception as e: 
                logger.error('failed to write file pathes ')
                

            self.status.set('text file written', 90)

            self.output_text.setValue( resource_txt )

            self.status.set('End of wps_get_resources process', 100)