import logging
import os
import shutil
import tempfile
import uuid
from os.path import abspath, curdir

import numpy as np
import ocgis
from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import GROUPING
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from ocgis import FunctionRegistry, OcgOperations, RequestDataset, env
from ocgis.calc import base
from ocgis.conv.nc import NcConverter
from ocgis.util.units import get_are_units_equal_by_string_or_cfunits
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps import LiteralInput
from pywps import Process
from pywps.app.Common import Metadata

LOGGER = logging.getLogger("PYWPS")


class Average(base.AbstractMultivariateFunction):
    """
    OCGIS class to compute the average of two variables.
    """
    key = 'average'
    long_name = 'Average of two variables'
    standard_name = 'average'
    description = 'Average of two variables'
    required_variables = ['v1', 'v2']

    def calculate(self, v1=None, v2=None):
        return (v1 + v2) / 2.

    def validate_units(self):
        from ocgis.util.units import get_are_units_equal_by_string_or_cfunits
        for i, required_variable in enumerate(self.required_variables):
            alias_variable = self.parms[required_variable]
            variable = self.field[alias_variable]
            if i == 0:
                source = variable.units
            else:
                target = variable.units
                match = get_are_units_equal_by_string_or_cfunits(source, target,
                                                                 try_cfunits=env.USE_CFUNITS)
                if not match:
                    raise UnitsValidationError(variable, target, self.key)

            self.units = source

    def get_output_units(self, *args, **kwargs):
        """
        Get the output units.

        :type variable: :class:`ocgis.interface.base.variable.Variable`
        :rtype: str
        """
        return self.units


FunctionRegistry.append(Average)


class FreezeThawOura(base.AbstractMultivariateFunction):
    """Freeze-thaw cycles"""
    key = 'freezethaw_oura'
    long_name = 'Freeze-thaw cycles'
    standard_name = 'freeze_thaw_cycles'
    description = 'Whether the temperature crossed 0 deg. C'
    required_variables = ['tasmin', 'tasmax']

    def sum_dim(self):
        return self.field.data_variables[0].dimension_names.index('time')

    def aggregate_temporal(self, values, **kwargs):
        """
        Optional method to overload for temporal aggregation.

        :param values: The input five-dimensional array.
        :type values: :class:`numpy.ma.core.MaskedArray`
        """

        return np.ma.sum(values, axis=self.sum_dim())

    def calculate(self, tasmin=None, tasmax=None):
        units_tasmin = self.field.data_variables[0].units
        units_tasmax = self.field.data_variables[1].units
        c1 = get_are_units_equal_by_string_or_cfunits(
            units_tasmin, 'K', try_cfunits=env.USE_CFUNITS)
        c2 = get_are_units_equal_by_string_or_cfunits(
            units_tasmax, 'K', try_cfunits=env.USE_CFUNITS)
        if not (c1 and c2):
            raise NotImplementedError("Units not in kelvin.")
        crosses_zero = np.bitwise_and(tasmin < 273.15, tasmax > 273.15)
        return crosses_zero.sum(self.sum_dim())


FunctionRegistry.append(FreezeThawOura)


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

        #        ComplexInput('tas', 'Mean daily temperature',
        #                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files storing mean daily temperature.',
        #                     metadata=[Metadata('Info')],
        #                     min_occurs=0,
        #                     max_occurs=1000,
        #                     supported_formats=[
        #                         Format('application/x-netcdf'),
        #                         Format('application/x-tar'),
        #                         Format('application/zip'),
        #                     ]),

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

    def _handler(self, request, response):
        from flyingpigeon.utils import calc_grouping

        ocgis.env.DIR_OUTPUT = tempfile.mkdtemp(dir=os.getcwd())
        env.OVERWRITE = True

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
            # res['tas'] = archiveextract(resource=rename_complexinputs(
            #    request.inputs['tas']))
            res['pr'] = archiveextract(resource=rename_complexinputs(
                request.inputs['pr']))

            grouping = request.inputs['grouping'][0].data
            calc_group = calc_grouping(grouping)

        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        response.update_status('Processed input parameters', 3)

        ######################################
        # Run all calculations
        ######################################
        # Compute tas from tasmin and tasmax average
        rdn = RequestDataset(res['tasmin'])
        rdx = RequestDataset(res['tasmax'])
        ops = OcgOperations(dataset=[rdn, rdx],
                            calc=[{'func': 'average', 'name': 'tas', 'kwds': {'v1': 'tasmin', 'v2': 'tasmax'}}],
                            output_format='nc')
        res['tas'] = ops.execute()

        # Indices computation
        calc = {}
        calc['tas'] = [{'func': 'icclim_TG', 'name': 'TG'},
                       {'func': 'icclim_GD4', 'name': 'GD4'},
                       ]

        calc['tasmax'] = [{'func': 'icclim_TX', 'name': 'TX'},
                          {'func': 'threshold', 'name': 'ND>30', 'kwds': {'threshold': 30 + 273.15, 'operation': 'gt'}}
                          ]

        calc['tasmin'] = [{'func': 'icclim_TN', 'name': 'TN'}, ]

        calc['pr'] = [{'func': 'icclim_PRCPTOT', 'name': 'PRCPTOT'},
                      {'func': 'icclim_RX5day', 'name': 'RX5day'}, ]

        calc['freezethaw'] = [{'func': 'freezethaw_oura', 'name': 'freezethaw',
                               'kwds': {'tasmin': 'tasmin',
                                        'tasmax': 'tasmax'}}]

        scs = []
        for key, val in calc.items():
            if key in ['pr', ]:
                rd = RequestDataset(res[key], conform_units_to='mm/day')
            elif key in ['freezethaw']:
                rdmin = RequestDataset(res['tasmin'])
                rdmax = RequestDataset(res['tasmax'])
                ops = OcgOperations(dataset=[rdmin, rdmax], calc=val,
                                    calc_grouping=calc_group)
                scs.append(ops.execute())
                continue
            else:
                rd = RequestDataset(res[key])

            ops = OcgOperations(dataset=rd,
                                calc=val,
                                calc_grouping=calc_group,
                                )

            scs.append(ops.execute())

        out = scs[0]
        outfield = out.get_element()

        for idx in range(1, len(scs)):
            for field, container in scs[idx].iter_fields(yield_container=True):
                for dv in field.data_variables:
                    outfield.add_variable(dv.extract(), is_data=True)

        # Prepare the environment
        dir_output = abspath(curdir)
        prefix = str(uuid.uuid1())
        env.PREFIX = prefix
        conv = NcConverter([out], outdir=dir_output, prefix=prefix)
        conv.write()
        shutil.rmtree(ocgis.env.DIR_OUTPUT)

        response.outputs['output_netcdf'].file = conv.path

        response.update_status('Execution completed', 100)

        return response
