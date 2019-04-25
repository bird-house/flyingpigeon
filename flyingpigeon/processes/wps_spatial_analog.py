"""
Process for spatial analog calculations.

Author: David Huard (huard.david@ouranos.ca),
"""

import logging
import datetime as dt

import netCDF4 as nc
import ocgis

from ocgis import FunctionRegistry, RequestDataset, OcgOperations
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata
from shapely.geometry import Point

from eggshell.nc.ocg_utils import call
from eggshell.utils import extract_archive
# from eggshell.utils import rename_complexinputs
# from eggshell.log import init_process_logger

from flyingpigeon.ocgisDissimilarity import Dissimilarity, metrics

LOGGER = logging.getLogger("PYWPS")

FunctionRegistry.append(Dissimilarity)


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
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('dateEndCandidate', 'Candidate end date',
                         abstract="End of period (YYYY-MM-DD) for candidate data. Defaults to last entry.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('dateStartTarget', 'Target start date',
                         abstract="Beginning of period (YYYY-MM-DD) for target "
                                  "data. "
                                  "Defaults to first entry.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         ),

            LiteralInput('dateEndTarget', 'Target end date',
                         abstract="End of period (YYYY-MM-DD) for target data. "
                                  "Defaults to last entry.",
                         data_type='dateTime',
                         min_occurs=0,
                         max_occurs=1,
                         ),
        ]

        outputs = [

            ComplexOutput('output', 'Dissimilarity values',
                          abstract="Dissimilarity between target at selected "
                                   "location and candidate distributions over the entire grid.",
                          as_reference=True,
                          supported_formats=[Format('application/x-netcdf')]
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
            version="0.2",
            metadata=[
                Metadata('Doc', 'http://flyingpigeon.readthedocs.io/en/latest/'),
            ],
            inputs=inputs,
            outputs=outputs,
            status_supported=True,
            store_supported=True,
        )

    def _handler(self, request, response):

        ocgis.env.DIR_OUTPUT = self.workdir
        ocgis.env.OVERWRITE = True
        tic = dt.datetime.now()

        LOGGER.info('Start process')
        response.update_status('Execution started at : {}'.format(tic), 1)

        ######################################
        # Read inputs
        ######################################
        try:
            candidate = extract_archive(
                resources=[inpt.file for inpt in request.inputs['candidate']],
                dir_output=self.workdir)
            target = extract_archive(
                resources=[inpt.file for inpt in request.inputs['target']],
                dir_output=self.workdir)
            location = request.inputs['location'][0].data
            indices = [el.data for el in request.inputs['indices']]
            dist = request.inputs['dist'][0].data
            start_candidate = request.inputs['dateStartCandidate'][0].data
            end_candidate = request.inputs['dateEndCandidate'][0].data
            start_target = request.inputs['dateStartTarget'][0].data
            end_target = request.inputs['dateEndTarget'][0].data
            point = Point(*map(float, location.split(',')))
        except Exception as ex:
            msg = 'Failed to parse input parameter {}'.format(ex)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Parsed input parameters', 2)

        LOGGER.debug("init took {}".format(dt.datetime.now() - tic))
        response.update_status('Processed input parameters', 3)

        ######################################
        # Extract target time series
        ######################################

        savetarget = False  # For debugging
        try:
            # Using `call` creates a netCDF file in the tmp directory.
            #
            # Here we keep this stuff in memory
            if savetarget:
                prefix = 'target_ts'
                target_ts = call(resource=target, geom=point, variable=indices,
                                 time_range=[start_target, end_target],
                                 select_nearest=True, prefix=prefix, dir_output=self.workdir)

            else:
                trd = RequestDataset(target, variable=indices,
                                     time_range=[start_target, end_target])

                op = OcgOperations(trd, geom=point, select_nearest=True,
                                   search_radius_mult=1.75, dir_output=self.workdir)
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
                          time_range=[start_candidate, end_candidate],
                          dir_output=self.workdir,
                          )

        except Exception as ex:
            msg = 'Spatial analog failed: {}'.format(ex)
            LOGGER.exception(msg)
            raise Exception(msg)

        add_metadata(output,
                     dist=dist,
                     indices=",".join(indices),
                     target_location=location,
                     candidate_time_range="{},{}".format(start_candidate,
                                                         end_candidate),
                     target_time_range="{},{}".format(start_target,
                                                      end_target)
                     )

        response.update_status('Computed spatial analog', 95)

        response.outputs['output'].file = output

        response.update_status('Execution completed', 100)
        LOGGER.debug("Total execution took {}".format(dt.datetime.now() - tic))
        return response


def add_metadata(ncfile, **kwds):
    """Add metadata to the dissimilarity variable."""
    ds = nc.Dataset(ncfile, 'a')
    v = ds.variables['dissimilarity']
    for key, val in kwds.items():
        v.setncattr(key, val)
    ds.close()
