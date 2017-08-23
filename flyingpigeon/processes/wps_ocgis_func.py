"""
The idea with this set of processes is to create one individual process for each
function. The advantage of using this approach rather than parameterizing the
function is that it allow platforms to parse the metadata of each function to
find those matching user search criteria.

These generic processes apply on a full dataset, that is, we assume that they
have been spatially and temporally cropped beforehand.


TODO: Add keywords to each function description once pyWPS implements support for it.

Author: David Huard (Ouranos)
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
from ocgis.calc import base
from ocgis import RequestDataset, OcgOperations
from ocgis.calc.library import register


import logging
LOGGER = logging.getLogger("PYWPS")

# Register ocgis functions, including icclim
fr = register.FunctionRegistry()
register.register_icclim(fr)

class IndicatorProcess(Process, object):
    key = 'to_be_subclassed'
    version = '1.0'


    #####################################
    # Generic inputs for all subclasses #
    #####################################
    inputs = [
        ComplexInput('resource', 'Resource',
                     abstract='NetCDF Files or archive (tar/zip) containing netCDF files.',
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
                      supported_formats=[Format('text/plain')]),
    ]


    def __init__(self):
        self.ocgis_cls = fr[self.key]

        super(IndicatorProcess, self).__init__(
            self._handler,
            identifier=self.ocgis_cls.key,
            title=self.ocgis_cls.long_name,
            abstract=self.ocgis_cls.description,
            inputs=self.inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )

    def _extra_input_handler(self, request):
        out = {}
        for obj in self.extra_inputs:
            out[obj.identifier] = self.request.inputs[obj.identifier][0].data
        return out


    def call(self, resource, calc, calc_grouping):
        from os.path import join, abspath, dirname, getsize, curdir
        from ocgis import OcgOperations, RequestDataset, env
        import uuid

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
                            output_format='nc')

        return ops.execute()

    def _handler(self, request, response):
        from flyingpigeon.utils import calc_grouping

        init_process_logger('log.txt')
        response.outputs['output_log'].file = 'log.txt'


        ######################################
        # Process standard inputs
        ######################################
        try:
            resource = archiveextract(resource=rename_complexinputs(
                request.inputs['resource']))
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
        # Call ocgis function
        ######################################

        output = self.call(resource=resource,
                           calc=[{'func': self.identifier,
                             'name': self.identifier,
                             'kwds': extras}],
                           calc_grouping=calc_group,
                      )

        response.outputs['output_netcdf'].file = output

        response.update_status('Execution completed', 100)

        return response

class ICCLIMProcess(IndicatorProcess):
    def __init__(self):
        # Scrape the meta data from the docstring
        self.ocgis_cls = fr[self.key]
        self.icclim_func = self.ocgis_cls._get_icclim_func_(self.ocgis_cls())
        doc = self.icclim_func.func_doc

        super(IndicatorProcess, self).__init__(
            self._handler,
            identifier=self.ocgis_cls.key,
            title=self.ocgis_cls.key.split('_')[1],
            abstract=doc.split('\n')[1].strip(),
            inputs=self.inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )

class IcclimTNProcess(ICCLIMProcess):
    key = 'icclim_TN'

class IcclimTXProcess(ICCLIMProcess):
    key = 'icclim_TX'


class FreezeThawProcess(IndicatorProcess):
    key = 'freezethaw'
    extra_inputs = [LiteralInput("treshold", "Threshold",
                                abstract="The number of degree-days above or below the freezing point after which the ground is considered frozen or thawed.",
                                data_type='float',
                                default=15.0,
                                min_occurs=0,
                                max_occurs=1),]


class Duration(IndicatorProcess):
    key = 'duration'
    extra_inputs = [LiteralInput("treshold", "Threshold",
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
                                 abstract="The summary operation to apply the durations. One of 'mean','median','std','max', or 'min'.",
                                 data_type='string',
                                 default='mean',
                                 min_occurs=0,
                                 max_occurs=1), ]



p = IcclimTNProcess()

ocgis_processes = [IcclimTNProcess,IcclimTXProcess]
