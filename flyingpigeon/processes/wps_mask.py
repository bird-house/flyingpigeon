import os

from pywps.Process import WPSProcess
import logging
from flyingpigeon.log import init_process_logger

logger = logging.getLogger(__name__)


class MaskProcess(WPSProcess):
    def __init__(self):
        WPSProcess.__init__(self,
                            identifier="mask",
                            title="Mask Land/Sea",
                            version="0.1",
                            abstract="Find the appropriate land_area fraction file and perform a CDO division",
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

        self.threshold = self.addLiteralInput(
            identifier="threshold",
            title="Threshold",
            abstract="Percentage of Land Area",
            default=50,
            type=type(1),
            minOccurs=1,
            maxOccurs=1,
            )

        self.mask = self.addComplexInput(
            identifier="mask",
            title="Land Area Fraction File",
            abstract="optional provide a url to an appropriate Land Area Fraction File.\
                     if no file is provided, the process will search an appropriate mask in the local cache.\
                     Make sure the land area fraction are allready fetched (use 'Download Resources' Process)",
            minOccurs=0,
            maxOccurs=100,
            # maxmegabites=50,
            formats=[{"application/x-netcdf"}],
        )

        ###########
        # output
        ###########

        self.output = self.addComplexOutput(
            title="Masked Files Archive",
            abstract="Tar file of the masked netCDF files",
            metadata=[],
            formats=[{"mimeType": "application/x-tar"}],
            asReference=True,
            identifier="output",
            )

        self.output_example = self.addComplexOutput(
            title="Example",
            abstract="one example file to display in the WMS",
            formats=[{"mimeType": "application/x-netcdf"}],
            asReference=True,
            identifier="output_example",
            )

        self.output_log = self.addComplexOutput(
            identifier="output_log",
            title="Logging information",
            abstract="Collected logs during process run.",
            formats=[{"mimeType": "text/plain"}],
            asReference=True,
        )

    def execute(self):

        init_process_logger('log.txt')
        self.output_log.setValue('log.txt')

        from flyingpigeon.utils import searchfile
        from flyingpigeon.subset import masking
        from flyingpigeon.utils import archive, archiveextract

        from flyingpigeon import config
        from os import path

        resources = archiveextract(self.getInputValues(identifier='resource'))
        base_dir = config.cache_path()

        ncs = []
        sftlf = []
        for nc in resources:
            basename = path.basename(nc)
            pattern
            bs = basename.split('_')
            pattern = 'sftlf_' + '_'.join(bs[1:-2]) + '_fx.nc'
            pattern = pattern.replace('historical',
                                      '*').replace('rcp85',
                                                   '*').replace('rcp65',
                                                                '*').replace('rcp45',
                                                                             '*').replace('rcp26', '*')
            sftlf.append(searchfile(pattern, path.curdir))
            sftlf.append(searchfile(pattern, base_dir))
            if len(sftlf) > 1:
                logger.warn('more than one sftlf file is found fitting to the pattern, first one will be taken')
            nc_mask = maksing(nc, sftlf[0])
            ncs.extend([nc_mask])
        nc_archive = archive(ncs)

        self.output.setValue(nc_archive)
        i = next((i for i, x in enumerate(ncs) if x), None)
        self.output_example.setValue(ncs[i])
