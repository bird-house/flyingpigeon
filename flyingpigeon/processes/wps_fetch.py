import os

from pywps.Process import WPSProcess
import logging

logger = logging.getLogger(__name__)


class FetchProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self,
                            identifier="fetch",
                            title="Download Resources",
                            version="0.9",
                            abstract="This process downloads resources (limited to 50GB) \
                            to the local file system of the birdhouse compute provider",
                            statusSupported=True,
                            storeSupported=True)

        self.resource = self.addComplexInput(
            identifier="resource",
            title="NetCDF File",
            abstract="NetCDF File",
            minOccurs=1,
            maxOccurs=100,
            maxmegabites=5000,
            formats=[{"mimeType": "application/x-netcdf"}],
            )

        self.output = self.addComplexOutput(
            identifier="output",
            title="Fetched Files",
            abstract="File containing the local pathes to downloades files",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
            )

    def execute(self):
        resources = self.getInputValues(identifier='resource')
        filename = 'out.txt'
        with open(filename, 'w') as fp:
            fp.write('###############################################\n')
            fp.write('###############################################\n')
            fp.write('Following files are stored to your local discs: \n')
            fp.write('\n')
            for resource in resources:
                fp.write('%s \n' % os.path.realpath(resource))
        self.output.setValue(filename)
