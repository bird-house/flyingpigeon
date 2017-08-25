from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import GROUPING
from flyingpigeon.log import init_process_logger

import ocgis
from os.path import join, abspath, dirname, getsize, curdir
from ocgis import OcgOperations, RequestDataset, env
import uuid

import logging
LOGGER = logging.getLogger("PYWPS")

class OuranosPublicIndicatorProcess(Process, object):
    identifier = "ouranos_public_indicators"
    title = "Climate indicators for the public web site."
    abstract = "Compute climate indicators: mean daily temp., min daily temp., max daily temp., growing degree days, number of days above 30C, freeze thaw cycles, total precipitation, and max 5-day precip."

    ##########
    # Inputs #
    ##########
    inputs = [
        ComplexInput('tasmin', 'Minimum daily temperature',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files storing minimum daily temperature.',
                     metadata=[Metadata('Info')],
                     min_occurs=1,
                     max_occurs=1000,
                     supported_formats=[
                         Format('application/x-netcdf'),
                         Format('application/x-tar'),
                         Format('application/zip'),
                     ]),

        ComplexInput('tasmax', 'Maximum daily temperature',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files storing maximum daily temperature.',
                     metadata=[Metadata('Info')],
                     min_occurs=1,
                     max_occurs=1000,
                     supported_formats=[
                         Format('application/x-netcdf'),
                         Format('application/x-tar'),
                         Format('application/zip'),
                     ]),

        ComplexInput('tas', 'Mean daily temperature',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files storing mean daily temperature.',
                     metadata=[Metadata('Info')],
                     min_occurs=0,
                     max_occurs=1000,
                     supported_formats=[
                         Format('application/x-netcdf'),
                         Format('application/x-tar'),
                         Format('application/zip'),
                     ]),

        ComplexInput('pr', 'Total daily precipitation',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files storing total daily precipitation.',
                     metadata=[Metadata('Info')],
                     min_occurs=1,
                     max_occurs=1000,
                     supported_formats=[
                         Format('application/x-netcdf'),
                         Format('application/x-tar'),
                         Format('application/zip'),
                     ]),

        LiteralInput("grouping", "Grouping",
                     abstract="Temporal group over which the index is computed.",
                     default='yr',
                     data_type='string',
                     min_occurs=0,
                     max_occurs=1,  # len(GROUPING),
                     allowed_values=GROUPING
                     )]

    ############################
    # Function-specific inputs #
    ############################
    extra_inputs = []

    ###########
    # Outputs #
    ###########
    outputs = [
        ComplexOutput('output_netcdf', 'Function output in netCDF',
                      abstract="The indicator values computed on the original input grid.",
                      as_reference=True,
                      supported_formats=[Format('application/x-netcdf')]
                      ),

        ComplexOutput('output_log', 'Logging information',
                      abstract="Collected logs during process run.",
                      as_reference=True,
                      supported_formats=[Format('text/plain')]),
    ]

    def __init__(self):

        super(OuranosPublicIndicatorProcess, self).__init__(
            self._handler,
            identifier=self.identifier,
            title=self.title,
            abstract=self.abstract,
            inputs=self.inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )

    def _extra_input_handler(self, request):
        out = {}
        for obj in self.extra_inputs:
            out[obj.identifier] = request.inputs[obj.identifier][0].data
        return out

    def call(self, resource, calc, calc_grouping):

        LOGGER.info('Start ocgis module call function')

        # Prepare the environment
        env.OVERWRITE = True
        dir_output = abspath(curdir)

        prefix = str(uuid.uuid1())
        env.PREFIX = prefix

        rd = RequestDataset(resource)

        ops = OcgOperations(dataset=rd,
                            calc=calc,
                            calc_grouping=calc_grouping,
                            dir_output=dir_output,
                            prefix=prefix,
                            add_auxiliary_files=False,
                            output_format='ocgis')

        return ops.execute()

    def _handler(self, request, response):
        from flyingpigeon.utils import calc_grouping

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        ######################################
        # Process standard inputs
        ######################################
        try:
            res = {}
            res['tasmin'] = archiveextract(resource=rename_complexinputs(
                request.inputs['tasmin']))
            res['tasmax'] = archiveextract(resource=rename_complexinputs(
                request.inputs['tasmax']))
            res['tas'] = archiveextract(resource=rename_complexinputs(
                request.inputs['tas']))
            res['pr'] = archiveextract(resource=rename_complexinputs(
                request.inputs['pr']))

            grouping = request.inputs['grouping'][0].data
            calc_group = calc_grouping(grouping)

        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        ######################################
        # Process extra inputs
        ######################################
        try:
            extras = self._extra_input_handler(request)

        except Exception as e:
            msg = 'Failed to read inputs {} '.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Processed input parameters', 3)

        ######################################
        # Run all calculations
        ######################################
        calc = {}
        calc['tas'] = [{'func':'icclim_TG', 'name':'TG'},
                    {'func': 'icclim_GD4', 'name': 'GD4'}]

        calc['tasmax'] = [{'func': 'icclim_TX', 'name': 'TX'},
                       #{'func': 'icclim_custom', 'name': 'HWN',
                       # 'kwds':{'indice_name': 'HWN',
                       #         'calc_operation': 'nb_events',
                       #         'logical_operation': 'gt',
                       #         'thresh': 30+273.15,}},
                       ]

        calc['tasmin'] = [{'func': 'icclim_TN', 'name': 'TN'},]

        calc['pr'] = [{'func':'icclim_PRCPTOT', 'name':'PRCPTOT'},
                   {'func': 'icclim_RX5day', 'name': 'RX5day'},]

        out = ocgis.SpatialCollection()
        for key, val in calc.items():
            rd = RequestDataset(res[key])
            ops = OcgOperations(dataset=rd,
                                calc=val,
                                calc_grouping=calc_group,
                                )

            out.add_field(ops.execute().get_element(), None)


        # Prepare the environment
        env.OVERWRITE = True
        dir_output = abspath(curdir)
        prefix = str(uuid.uuid1())
        env.PREFIX = prefix

        # Write the collection to file
        ops = OcgOperations(dataset=out,
                            dir_output=dir_output,
                            prefix=prefix,
                            add_auxiliary_files=False,
                            output_format='nc')

        response.outputs['output_netcdf'].file = ops.execute()

        response.update_status('Execution completed', 100)

        return response
