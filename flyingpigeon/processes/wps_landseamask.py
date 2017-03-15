from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import rename_complexinputs

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

import logging
LOGGER = logging.getLogger("PYWPS")


class LandseamaskProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'Resource',
                         abstract="NetCDF Files or archive (tar/zip) containing netCDF files",
                         min_occurs=1,
                         max_occurs=1000,
                         #  maxmegabites=5000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput("threshold", "Threshold",
                         abstract="Percentage of Land Area",
                         default="50",
                         data_type='integer',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            # ComplexInput("mask", "Land Area Fraction File",
            #              abstract="optional provide a url to an appropriate Land Area Fraction File. If no file is\
            #               provided, the process will search an appropriate mask in the local cache.\
            #               Make sure the land area fraction are allready fetched (use 'Download Resources' Process)",
            #              min_occurs=0,
            #              max_occurs=100,
            #              # maxmegabites=50,
            #              supported_formats=[
            #                   Format('application/x-netcdf'),
            #                   Format('application/x-tar'),
            #                   Format('application/zip'),
            #                   ]),

            LiteralInput("land_area", "Land/Sea",
                         abstract="If land_area (default) is checked, sea areas will be set to missing value",
                         default='1',
                         data_type='boolean',
                         )
            ]

        outputs = [
            ComplexOutput("output_archive", "Masked Files Archive",
                          abstract="Tar file of the masked netCDF files",
                          supported_formats=[Format("application/x-tar")],
                          as_reference=True,
                          ),

            ComplexOutput("output_example", "Example",
                          abstract="one example file to display in the WMS",
                          supported_formats=[Format("application/x-netcdf")],
                          as_reference=True,
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format("text/plain")])
            ]

        super(LandseamaskProcess, self).__init__(
            self._handler,
            identifier="landseamask",
            title="Masking Land-Sea",
            version="0.2",
            abstract="Find the appropriate land_area fraction file and perform a\
                      CDO division to mask either land or sea areas",
            metadata=[
                {"title": "Doc", "href": "http://flyingpigeon.readthedocs.io/en/latest/"},
                ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True
            )

    def _handler(self, request, response):
        from flyingpigeon.utils import searchfile
        from flyingpigeon.subset import masking
        from flyingpigeon.utils import archive, archiveextract

        from flyingpigeon import config
        from os import path

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        resources = archiveextract(
            resource=rename_complexinputs(request.inputs['resource']))
        masks = archiveextract(
            resource=rename_complexinputs(request.inputs['mask']))
        land_area = request.inputs['land_area']

        fp_cache = config.cache_path().split('/')
        base_dir = '/'.join(fp_cache[0:-1])  # base dir for all birds

        LOGGER.debug('base dir of directory tree: %s' % base_dir)

        ncs = []
        sftlf = []
        for nc in resources:
            try:
                basename = path.basename(nc)
                bs = basename.split('_')
                pattern = 'sftlf_' + '_'.join(bs[1:-2]) + '_fx.nc'
                pattern = pattern.replace('historical',
                                          '*').replace('rcp85',
                                                       '*').replace('rcp65',
                                                                    '*').replace('rcp45',
                                                                                 '*').replace('rcp26', '*')
                LOGGER.debug('searching for %s ' % pattern)
                sftlf.extend(searchfile(pattern, path.curdir))
                sftlf.extend(searchfile(pattern, base_dir))
                LOGGER.debug('lenght of sftlf: %s' % len(sftlf))
                if len(sftlf) >= 1:
                    if len(sftlf) > 1:
                        LOGGER.warn(
                            'more than one sftlf file is found fitting to the pattern, first one will be taken %s'
                            % sftlf[0])
                    prefix = 'masked%s' % basename.replace('.nc', '')
                    nc_mask = masking(nc, sftlf[0], land_area=land_area, prefix=prefix)
                    ncs.extend([nc_mask])
                    LOGGER.info('masking processed for %s' % basename)
                else:
                    LOGGER.warn('no masked found. Please perform a "Download Resources"\
                     to make sure the land_area file is in cache')
            except:
                LOGGER.exception('failed to mask file: %s' % basename)
        nc_archive = archive(ncs)

        response.outputs['output_archive'].file = nc_archive
        i = next((i for i, x in enumerate(ncs) if x), None)
        response.outputs['output_example'].file = ncs[i]

        response.update_status("done", 100)
        return response
