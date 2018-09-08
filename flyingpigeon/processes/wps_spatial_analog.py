"""
Process for spatial analog calculations.

Author: David Huard (huard.david@ouranos.ca),
"""

from eggshell.log import init_process_logger
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import get_values
from flyingpigeon.ocgis_module import call
from shapely.geometry import Point
import netCDF4 as nc

from datetime import datetime as dt

import netCDF4 as nc
from ocgis import FunctionRegistry, RequestDataset, OcgOperations
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata
from shapely.geometry import Point

from flyingpigeon.log import init_process_logger
from flyingpigeon.ocgisDissimilarity import Dissimilarity, metrics
from flyingpigeon.ocgis_module import call
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs

FunctionRegistry.append(Dissimilarity)

import logging

LOGGER = logging.getLogger("PYWPS")


class SpatialAnalogProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('candidate', 'Candidate netCDF dataset',
                         abstract='NetCDF files or archive (tar/zip)  '
                                  'storing the candidate indices. The output will be stored on this grid.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            ComplexInput('target', 'Target netCDF dataset',
                         abstract='NetCDF files or archive (tar/zip).'
                                  'containing netcdf files storing the '
                                  'target indices.',
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1000,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),

            LiteralInput('location', 'Target coordinates (lon,lat)',
                         abstract="Geographical coordinates (lon,lat) of the target location.",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=1,
                         ),

            LiteralInput('indices', 'Indices',
                         abstract="One or more climate indices to use for the comparison.",
                         data_type='string',
                         min_occurs=1,
                         max_occurs=5,
                         ),

            LiteralInput('dist', "Distance",
                         abstract="Dissimilarity metric comparing distributions.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='kldiv',
                         allowed_values=metrics,
                         ),

            LiteralInput('dateStartCandidate', 'Candidate start date',
                         abstract="Beginning of period (YYYY-MM-DD) for candidate data. "
                                  "Defaults to first entry.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='',
                         ),

            LiteralInput('dateEndCandidate', 'Candidate end date',
                         abstract="End of period (YYYY-MM_DD) for candidate data. Defaults to last entry.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='',
                         ),

            LiteralInput('dateStartTarget', 'Target start date',
                         abstract="Beginning of period (YYYY-MM-DD) for target "
                                  "data. "
                                  "Defaults to first entry.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='',
                         ),

            LiteralInput('dateEndTarget', 'Target end date',
                         abstract="End of period (YYYY-MM_DD) for target data. "
                                  "Defaults to last entry.",
                         data_type='string',
                         min_occurs=0,
                         max_occurs=1,
                         default='',
                         ),
        ]

        outputs = [

            ComplexOutput('output_netcdf', 'Dissimilarity values',
                          abstract="Dissimilarity between target at selected "
                                   "location and candidate distributions over the entire grid.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
                          ),

            ComplexOutput('output_log', 'Logging information',
                          abstract="Collected logs during process run.",
                          as_reference=True,
                          supported_formats=[Format('text/plain')]
                          ),
        ]

        super(SpatialAnalogProcess, self).__init__(
            self._handler,
            identifier="spatial_analog",
            title="Spatial analog of a target climate.",
            abstract="Spatial analogs based on the comparison of climate "
                     "indices. The algorithm compares the distribution of the "
                     "target indices with the distribution of spatially "
                     "distributed candidate indices and returns a value  "
                     "measuring the dissimilarity between both distributions over the candidate grid.",
            version="0.1",
            metadata=[
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        tic = dt.now()
        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        LOGGER.info('Start process')
        response.update_status('Execution started at : {}'.format(tic), 1)

        ######################################
        # Read inputs
        ######################################
        try:
            candidate = archiveextract(resource=rename_complexinputs(
                request.inputs['candidate']))
            target = archiveextract(resource=rename_complexinputs(
                request.inputs['target']))
            location = request.inputs['location'][0].data
            indices = [el.data for el in request.inputs['indices']]
            dist = request.inputs['dist'][0].data
            dateStartCandidate = request.inputs['dateStartCandidate'][0].data
            dateEndCandidate = request.inputs['dateEndCandidate'][0].data
            dateStartTarget = request.inputs['dateStartTarget'][0].data
            dateEndTarget = request.inputs['dateEndTarget'][0].data

        except Exception as ex:
            msg = 'Failed to read input parameter {}'.format(ex)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        ######################################
        # Process inputs
        ######################################

        try:
            point = Point(*map(float, location.split(',')))
            dateStartCandidate = dt.strptime(dateStartCandidate, '%Y-%m-%d')
            dateEndCandidate = dt.strptime(dateEndCandidate, '%Y-%m-%d')
            dateStartTarget = dt.strptime(dateStartTarget, '%Y-%m-%d')
            dateEndTarget = dt.strptime(dateEndTarget, '%Y-%m-%d')

        except Exception as ex:
            msg = 'failed to process inputs {}'.format(ex)
            LOGGER.error(msg)
            raise Exception(msg)

        LOGGER.debug("init took {}".format(dt.now() - tic))
        response.update_status('Processed input parameters', 3)

        ######################################
        # Extract target time series
        ######################################
        savetarget = False
        try:
            # Using `call` creates a netCDF file in the tmp directory.
            #
            # Here we keep this stuff in memory
            if savetarget:
                prefix = 'target_ts'
                target_ts = call(resource=target, geom=point, variable=indices,
                                 time_range=[dateStartTarget, dateEndTarget],
                                 select_nearest=True, prefix=prefix)

                # target_ts = [get_values(prefix+'.nc', ind) for ind in indices]

            else:
                trd = RequestDataset(target, variable=indices,
                                     time_range=[dateStartTarget, dateEndTarget])

                op = OcgOperations(trd, geom=point, select_nearest=True,
                                   search_radius_mult=1.75)
                out = op.execute()
                target_ts = out.get_element()

        except Exception as ex:
            msg = 'Target extraction failed {}'.format(ex)
            LOGGER.debug(msg)
            raise Exception(msg)

        response.update_status('Extracted target series', 5)

        ######################################
        # Compute dissimilarity metric
        ######################################

        response.update_status('Computing spatial analog', 6)
        try:
            output = call(resource=candidate,
                          calc=[{'func': 'dissimilarity', 'name': 'spatial_analog',
                                 'kwds': {'dist': dist, 'target': target_ts,
                                          'candidate': indices}}],
                          time_range=[dateStartCandidate, dateEndCandidate],
                          )

        except Exception as ex:
            msg = 'Spatial analog failed: {}'.format(ex)
            LOGGER.exception(msg)
            raise Exception(msg)

        add_metadata(output,
                     dist=dist,
                     indices=",".join(indices),
                     target_location=location,
                     candidate_time_range="{},{}".format(dateStartCandidate,
                                                         dateEndCandidate),
                     target_time_range="{},{}".format(dateStartTarget,
                                                      dateEndTarget)
                     )

        response.update_status('Computed spatial analog', 95)

        response.outputs['output_netcdf'].file = output

        response.update_status('Execution completed', 100)
        LOGGER.debug("Total execution took {}".format(dt.now() - tic))
        return response


def add_metadata(ncfile, **kwds):
    """Add metadata to the dissimilarity variable."""
    D = nc.Dataset(ncfile, 'a')
    V = D.variables['dissimilarity']
    for key, val in kwds.items():
        V.setncattr(key, val)
    D.close()
