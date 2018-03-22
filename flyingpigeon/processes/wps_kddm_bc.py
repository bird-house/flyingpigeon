"""
KDDM Bias correction.

Author: Seth McGinnis (KDDM algorithm), David Huard (WPS wrapper)
"""
from pywps import Process, Format, LiteralInput, LiteralOutput
from pywps import ComplexInput, ComplexOutput
import flyingpigeon
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archiveextract, rename_complexinputs
import ocgis
import logging
LOGGER = logging.getLogger("PYWPS")


class KDDM_BC_Process(Process):
    """
    References
    ----------
    McGinnis S., Nychka D., Mearns L.O. (2015) A New Distribution Mapping Technique for Climate Model Bias Correction. In: Lakshmanan V., Gilleland E., McGovern A., Tingley M. (eds) Machine Learning and Data Mining Approaches to Climate Science. Springer, Cham, DOI: https://doi.org/10.1007/978-3-319-17220-0_9


    """
    def __init__(self):
        inputs = [
            ComplexInput('obs', 'Observation netCDF dataset',
                         abstract="netCDF files storing the data with the desired statistics, typically observations.",
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                        ]),
            ComplexInput('ref', 'Reference netCDF dataset',
                         abstract="netCDF files storing the reference data whose statistics are going to be mapped to "
                                  "the obs, typically simulations over the observed period.",
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[Format('application/x-netcdf'),
                                            Format('application/x-tar'),
                                            Format('application/zip'),
                                            ]),
            ComplexInput('fut', 'Future netCDF dataset',
                         abstract="netCDF files storing the data to be downscaled using the mapping defined between "
                                  "ref and obs.",
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[Format('application/x-netcdf'),
                                            Format('application/x-tar'),
                                            Format('application/zip'),
                                            ]),
        ]
        """
                   LiteralInput('dedrizzle', 'De-drizzle',
                                abstract="Whether to remove the precipitation drizzle.",
                                min_occurs=0,
                                max_occurs=1,
                                default='False',
                                data_type='bool'),
                   LiteralInput('norm', 'Normalization transformation',
                                abstract="Transformation to apply before mapping.",
                                default='identity',
                                allowed_values=["identifty", "zscore", "boxcox"],
                                data_type='string'),
        """
        outputs = [
            ComplexOutput('output_netcdf_ref', 'Bias-corrected ref dataset.',
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf'),]
                          ),
            ComplexOutput('output_netcdf_fut', 'Bias-corrected fut dataset.',
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf'),]
                          ),
            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain'),]
                          ),
        ]

        super(KDDM_BC_Process, self).__init__(
            self._handler,
            identifier='kddm_bc',
            title="Kernel Density Distribution Mapping Bias Correction",
            abstract="Bias correction method using Kernel Density Distribution Mapping (KDDM).",
            version="1.0",
            inputs=inputs,
            outputs=outputs,
            store_supported=True,
            status_supported=True
        )

    def _handler(self, request, response):
        from rpy2 import robjects
        from rpy2.robjects.packages import importr

        import os
        import datetime as dt

        tic = dt.datetime.now()
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        LOGGER.info('Start process')
        response.update_status('Execution started at : {}'.format(tic), 1)

        ######################################
        # Read inputs
        ######################################
        try:
            obs = archiveextract(resource=rename_complexinputs(
                request.inputs['obs']))
            ref = archiveextract(resource=rename_complexinputs(
                request.inputs['ref']))
            fut = archiveextract(resource=rename_complexinputs(
                request.inputs['fut']))
            #dedrizzle = request.inputs['dedrizzle'][0].data
            #norm = request.inputs['norm'][0].data
        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            msg += "obs: " + request.inputs['obs']
            msg += "ref: " + request.inputs['ref']
            msg += "fut: " + request.inputs['fut']
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        rp, ext = os.path.splitext(ref[0])
        ref_out = rp + '_kddm-bc' + ext

        fp, ext = os.path.splitext(fut[0])
        fut_out = fp + '_kddm-bc' + ext

        # Assuming all files share the same variable.
        rd = ocgis.RequestDataset(ref)
        varname = rd.variable

        # Calling the R code
        Rsrc = os.path.join(flyingpigeon.config.Rsrc_dir(), 'bc.kddm.R')
        devtools = importr("devtools")

        rfunc = robjects.r(open(Rsrc).read())
        rfunc(varname, obs[0], ref[0], fut[0], ref_out, fut_out, False)

        response.outputs['output_netcdf_ref'].file = ref_out
        response.outputs['output_netcdf_fut'].file = fut_out

        return response

