"""
The idea with this set of processes is to create one individual process for each
function. The advantage of using this approach rather than parameterizing the
function is that it allow platforms to parse the metadata of each function to
find those matching user search criteria.

These generic processes apply on a full dataset, that is, we assume that they
have been spatially and temporally cropped beforehand.


TODO: Add keywords to each function description once pyWPS implements support
for it.

Author: David Huard, Ouranos, 2017
"""

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
from ocgis.calc.library import register
from ocgis.contrib import library_icclim as libclim
from collections import OrderedDict

import logging
LOGGER = logging.getLogger("PYWPS")

# Register ocgis functions, including icclim
fr = register.FunctionRegistry()
register.register_icclim(fr)
icclim_classes = [k for k in fr.keys() if isinstance(k, str) and k.startswith('icclim')]


class IndicatorProcess(Process, object):
    """A Process class wrapping OCGIS functions."""
    key = 'to_be_subclassed'
    version = ocgis.__version__

    #################
    # Common inputs #
    #################
    resource_inputs = [
        ComplexInput('resource', 'Resource',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
                     metadata=[Metadata('Info')],
                     min_occurs=1,
                     max_occurs=1000,
                     supported_formats=[
                         Format('application/x-netcdf'),
                         Format('application/x-tar'),
                         Format('application/zip'),
                     ]), ]

    option_inputs = [
        LiteralInput("grouping", "Grouping",
                     abstract="Temporal group over which the index is computed.",
                     default='yr',
                     data_type='string',
                     min_occurs=0,
                     max_occurs=1,  # len(GROUPING),
                     allowed_values=GROUPING
                     ), ]

    ############################
    # Function-specific inputs #
    ############################
    extra_inputs = []

    ##################
    # Common outputs #
    ##################
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
        self.load_meta()
        super(IndicatorProcess, self).__init__(
            self._handler,
            identifier=self.identifier,
            title=self.title,
            abstract=self.abstract,
            version=self.version,
            inputs=self.resource_inputs + self.option_inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )

    def load_meta(self):
        """Extract process meta data from underlying object."""
        self.ocgis_cls = fr[self.key]
        self.identifier = self.ocgis_cls.key
        self.title = self.ocgis_cls.long_name
        self.abstract = self.ocgis_cls.description

    def _resource_input_handler(self, request):
        out = OrderedDict()

        for obj in self.resource_inputs:
            key = obj.identifier
            out[key] = archiveextract(resource=rename_complexinputs(
                        request.inputs[key]))
        return out

    def _option_input_handler(self, request):
        from flyingpigeon.utils import calc_grouping
        out = {'calc_grouping': None}

        for obj in self.option_inputs:
            key = obj.identifier
            val = request.inputs[key][0].data

            if key == 'grouping':
                out['calc_grouping'] = calc_grouping(val)
            else:
                out[key] = val

        return out

    def _extra_input_handler(self, request):
        out = {}

        for obj in self.extra_inputs:
            key = obj.identifier
            out[key] = request.inputs[key][0].data
        return out

    def _handler(self, request, response):

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'

        ######################################
        # Process inputs
        ######################################

        try:
            resources = self._resource_input_handler(request)
            options = self._option_input_handler(request)
            extras = self._extra_input_handler(request)

        except Exception as e:
            msg = 'Failed to read input parameter {}'.format(e)
            LOGGER.error(msg)
            raise Exception(msg)

        response.update_status('Input parameters ingested', 2)

        ######################################
        # Call ocgis function
        ######################################
        # Mapping for multivariate functions
        if getattr(self, 'has_required_variables', None):
            extras.update({k: k for k in resources.keys()})

        output = run_op(resource=resources,
                          calc=[{'func': self.identifier,
                                 'name': self.identifier,
                                 'kwds': extras}],
                          options=options)

        response.outputs['output_netcdf'].file = output

        response.update_status('Execution completed', 100)

        return response

def run_op(resource, calc, options):
    """Create an OCGIS operation, launch it and return the results."""
    from os.path import abspath, curdir
    from ocgis import OcgOperations, RequestDataset, env
    import uuid

    LOGGER.info('Start ocgis module call function')

    # Prepare the environment
    env.OVERWRITE = True
    dir_output = abspath(curdir)

    prefix = str(uuid.uuid1())
    env.PREFIX = prefix

    rd = [RequestDataset(val, variable=key if key != 'resource' else None) for key, val in resource.items()]

    ops = OcgOperations(dataset=rd,
                        calc=calc,
                        calc_grouping=options['calc_grouping'],
                        dir_output=dir_output,
                        prefix=prefix,
                        add_auxiliary_files=False,
                        output_format='nc')

    return ops.execute()

#############################################
#          Custom class definitions         #
#############################################


class FreezeThawProcess(IndicatorProcess):
    key = 'freezethaw'
    extra_inputs = [LiteralInput("threshold", "Threshold",
                                 abstract="The number of degree-days above or below the freezing point after which the"
                                          "ground is considered frozen or thawed.",
                                 data_type='float',
                                 default=15.0,
                                 min_occurs=0,
                                 max_occurs=1), ]


class Duration(IndicatorProcess):
    key = 'duration'
    extra_inputs = [LiteralInput("threshold", "Threshold",
                                 abstract="The threshold value to use for the logical operation.",
                                 data_type='float',
                                 min_occurs=1,
                                 max_occurs=1),
                    LiteralInput("operation", "Operation",
                                 abstract="The logical operation. One of 'gt','gte','lt', or 'lte'.",
                                 data_type='string',
                                 min_occurs=1,
                                 max_occurs=1),
                    LiteralInput("summary", "Summary",
                                 abstract="The summary operation to apply the durations. One of 'mean','median','std',"
                                          "'max', or 'min'.",
                                 data_type='string',
                                 default='mean',
                                 min_occurs=0,
                                 max_occurs=1), ]


#############################################
#    Automated ICCLIM class definitions     #
#############################################

# TODO: Implement check to make sure that the data is daily ?
class ICCLIMProcess(IndicatorProcess):
    """Process class instantiated using definitions from the ICCLIM library.
    """
    def load_meta(self):
        """Extract process meta data from underlying object."""
        self.icclim_func = libclim._icclim_function_map[self.key]['func']
        doc = self.icclim_func.func_doc

        self.ocgis_cls = fr[self.key]
        self.identifier = self.ocgis_cls.key
        self.title = self.ocgis_cls.key.split('_')[1]
        self.abstract = doc.split('\n')[1].strip()

        self.has_required_variables = hasattr(self.ocgis_cls, 'required_variables')

        if self.has_required_variables:
            self.resource_inputs = []  # No more resource input.
            for key in self.ocgis_cls.required_variables:
                self.resource_inputs.append(
                    ComplexInput(key, key,
                                 abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
                                 metadata=[Metadata('Info')],
                                 min_occurs=1,
                                 max_occurs=1000,
                                 supported_formats=[
                                     Format('application/x-netcdf'),
                                     Format('application/x-tar'),
                                     Format('application/zip'), ]
                                 ))


def create_icclim_process_class(key):
    """Create a subclass of an ICCLIMProcess for a given indicator."""
    name = key.upper()+'Process'
    clazz = type(name, (ICCLIMProcess,), {'key': key, '__name__': name})
    return clazz


def icclim_process_generator(keys):
    """Dynamically create derived classes for ICCLIM processes."""
    for key in keys:
        yield create_icclim_process_class(key)

ICCLIM_PROCESSES = [p for p in icclim_process_generator(icclim_classes)]
OCGIS_INDEX_PROCESSES = [FreezeThawProcess, Duration] + ICCLIM_PROCESSES
__all__ = [c.__name__ for c in OCGIS_INDEX_PROCESSES] + ['OCGIS_INDEX_PROCESSES']

# Add generated classes to namespace
for c in ICCLIM_PROCESSES:
    globals()[c.__name__] = c
