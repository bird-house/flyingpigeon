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
            abstract="test file with a list of the used input data sets",
            formats=[{"mimeType":"text/plain"}],
            asReference=True,
            )


        def execute(self):

            self.status.set('starting write path files process', 0)

            urls = self.getInputValues(identifier='resource')

            self.status.set('Path for %s fiels ' % len(urls), 20)

            pathes = open("config_%s.txt" % (date_stamp), "w")
            pathes.write('###############################################\n')
            pathes.write('###############################################\n')
            pathes.write('Following files are stored to your local discs: \n')
            pathes.write('\n')
            for url in urls: 
                pathes.write('%s \n' % url)
            
            pathes.close()

            self.status.set('text file written', 90)

            self.output_text.setValue( pathes )

            self.status.set('End of wps_get_resources process', 100)