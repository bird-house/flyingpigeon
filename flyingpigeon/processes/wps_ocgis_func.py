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

from ocgis.calc.library import register
from ocgis.contrib import library_icclim as libclim
from collections import OrderedDict

import logging
LOGGER = logging.getLogger("PYWPS")

# Register ocgis functions, including icclim
fr = register.FunctionRegistry()
register.register_icclim(fr)
icclim_classes = {k:v for (k,v) in fr.items() if type(k) == str and k.startswith('icclim')}

class IndicatorProcess(Process, object):
    """A Process class wrapping OCGIS functions."""
    key = 'to_be_subclassed'
    version = '1.0'

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
                     ]),]

    option_inputs = [
        LiteralInput("grouping", "Grouping",
                     abstract="Temporal group over which the index is computed.",
                     default='yr',
                     data_type='string',
                     min_occurs=0,
                     max_occurs=1,  # len(GROUPING),
                     allowed_values=GROUPING
                     ),]

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
        self.ocgis_cls = fr[self.key]

        super(IndicatorProcess, self).__init__(
            self._handler,
            identifier=self.ocgis_cls.key,
            title=self.ocgis_cls.long_name,
            abstract=self.ocgis_cls.description,
            inputs=self.resource_inputs + self.option_inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )


    def _resource_input_handler(self, request):
        out = OrderedDict()

        for obj in self.resource_inputs:
            key = obj.identifier
            out[key] = archiveextract(resource=rename_complexinputs(
                        request.inputs[key]))
        return out

    def _option_input_handler(self, request):
        from flyingpigeon.utils import calc_grouping
        out = {'calc_grouping':None}

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
        if self.has_required_variables:
            extras.update( {k:k for k in resources.keys()} )

        output = self._run(resource=resources,
                           calc=[{'func': self.identifier,
                             'name': self.identifier,
                             'kwds': extras}],
                           options=options
                      )

        response.outputs['output_netcdf'].file = output

        response.update_status('Execution completed', 100)

        return response

    def _run(self, resource, calc, options):
        from os.path import join, abspath, dirname, getsize, curdir
        from ocgis import OcgOperations, RequestDataset, env
        import uuid

        LOGGER.info('Start ocgis module call function')

        # Prepare the environment
        env.OVERWRITE = True
        dir_output = abspath(curdir)

        prefix = str(uuid.uuid1())
        env.PREFIX = prefix

        rd = [RequestDataset(val, variable=key if key != 'resource' else None   ) for key, val in resource.items()]

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
                                abstract="The number of degree-days above or below the freezing point after which the ground is considered frozen or thawed.",
                                data_type='float',
                                default=15.0,
                                min_occurs=0,
                                max_occurs=1),]

# Untested
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
                                 abstract="The summary operation to apply the durations. One of 'mean','median','std','max', or 'min'.",
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
    def __init__(self):
        # Scrape the meta data from the docstring
        self.ocgis_cls = fr[self.key]

        #self.icclim_func = self.ocgis_cls._get_icclim_func_(self.ocgis_cls())
        self.icclim_func = libclim._icclim_function_map[self.key]['func']
        doc = self.icclim_func.func_doc

        self.has_required_variables = hasattr(self.ocgis_cls, 'required_variables')

        if self.has_required_variables:
            self.resource_inputs = [] # No more resource input.
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
                                 Format('application/zip'),
                             ])
                    )

        super(IndicatorProcess, self).__init__(
            self._handler,
            identifier=self.ocgis_cls.key,
            title=self.ocgis_cls.key.split('_')[1],
            abstract=doc.split('\n')[1].strip(),
            inputs=self.resource_inputs + self.option_inputs + self.extra_inputs,
            outputs=self.outputs,
            status_supported=True,
            store_supported=True,
        )

def _generate_icclim_classes():
    """Automatically generate code definining Process classes for ICCLIM
    indicators.
    """
    import operator
    import ocgis
    pat = """
class {0}Process(ICCLIMProcess):
    key = '{1}'
    """
    txt = ""
    names = []
    for key, cls in sorted(icclim_classes.items(), key=operator.itemgetter(0)):
        txt = txt + (pat.format(key.upper(), key))
        names.append( "{}Process".format(key.upper()) )

    return txt, names

# Run the class generator. The output has been copied below.
cls_definition, cls_names = _generate_icclim_classes()



#############################################
# Automatically generated icclim processes  #
# Univariate functions only so far          #
# No automated support for extra parameters #
#############################################

class ICCLIM_CDDProcess(ICCLIMProcess):
    key = 'icclim_CDD'


class ICCLIM_CFDProcess(ICCLIMProcess):
    key = 'icclim_CFD'


class ICCLIM_CSDIProcess(ICCLIMProcess):
    key = 'icclim_CSDI'


class ICCLIM_CSUProcess(ICCLIMProcess):
    key = 'icclim_CSU'


class ICCLIM_CWDProcess(ICCLIMProcess):
    key = 'icclim_CWD'


class ICCLIM_DTRProcess(ICCLIMProcess):
    key = 'icclim_DTR'


class ICCLIM_ETRProcess(ICCLIMProcess):
    key = 'icclim_ETR'


class ICCLIM_FDProcess(ICCLIMProcess):
    key = 'icclim_FD'


class ICCLIM_GD4Process(ICCLIMProcess):
    key = 'icclim_GD4'


class ICCLIM_HD17Process(ICCLIMProcess):
    key = 'icclim_HD17'


class ICCLIM_IDProcess(ICCLIMProcess):
    key = 'icclim_ID'


class ICCLIM_PRCPTOTProcess(ICCLIMProcess):
    key = 'icclim_PRCPTOT'


class ICCLIM_R10MMProcess(ICCLIMProcess):
    key = 'icclim_R10mm'


class ICCLIM_R20MMProcess(ICCLIMProcess):
    key = 'icclim_R20mm'


class ICCLIM_R75PProcess(ICCLIMProcess):
    key = 'icclim_R75p'


class ICCLIM_R75PTOTProcess(ICCLIMProcess):
    key = 'icclim_R75pTOT'


class ICCLIM_R95PProcess(ICCLIMProcess):
    key = 'icclim_R95p'


class ICCLIM_R95PTOTProcess(ICCLIMProcess):
    key = 'icclim_R95pTOT'


class ICCLIM_R99PProcess(ICCLIMProcess):
    key = 'icclim_R99p'


class ICCLIM_R99PTOTProcess(ICCLIMProcess):
    key = 'icclim_R99pTOT'


class ICCLIM_RR1Process(ICCLIMProcess):
    key = 'icclim_RR1'


class ICCLIM_RX1DAYProcess(ICCLIMProcess):
    key = 'icclim_RX1day'


class ICCLIM_RX5DAYProcess(ICCLIMProcess):
    key = 'icclim_RX5day'


class ICCLIM_SDProcess(ICCLIMProcess):
    key = 'icclim_SD'


class ICCLIM_SD1Process(ICCLIMProcess):
    key = 'icclim_SD1'


class ICCLIM_SD50CMProcess(ICCLIMProcess):
    key = 'icclim_SD50cm'


class ICCLIM_SD5CMProcess(ICCLIMProcess):
    key = 'icclim_SD5cm'


class ICCLIM_SDIIProcess(ICCLIMProcess):
    key = 'icclim_SDII'


class ICCLIM_SUProcess(ICCLIMProcess):
    key = 'icclim_SU'


class ICCLIM_TGProcess(ICCLIMProcess):
    key = 'icclim_TG'


class ICCLIM_TG10PProcess(ICCLIMProcess):
    key = 'icclim_TG10p'


class ICCLIM_TG90PProcess(ICCLIMProcess):
    key = 'icclim_TG90p'


class ICCLIM_TNProcess(ICCLIMProcess):
    key = 'icclim_TN'


class ICCLIM_TN10PProcess(ICCLIMProcess):
    key = 'icclim_TN10p'


class ICCLIM_TN90PProcess(ICCLIMProcess):
    key = 'icclim_TN90p'


class ICCLIM_TNNProcess(ICCLIMProcess):
    key = 'icclim_TNn'


class ICCLIM_TNXProcess(ICCLIMProcess):
    key = 'icclim_TNx'


class ICCLIM_TRProcess(ICCLIMProcess):
    key = 'icclim_TR'


class ICCLIM_TXProcess(ICCLIMProcess):
    key = 'icclim_TX'


class ICCLIM_TX10PProcess(ICCLIMProcess):
    key = 'icclim_TX10p'


class ICCLIM_TX90PProcess(ICCLIMProcess):
    key = 'icclim_TX90p'


class ICCLIM_TXNProcess(ICCLIMProcess):
    key = 'icclim_TXn'


class ICCLIM_TXXProcess(ICCLIMProcess):
    key = 'icclim_TXx'


class ICCLIM_WSDIProcess(ICCLIMProcess):
    key = 'icclim_WSDI'


class ICCLIM_VDTRProcess(ICCLIMProcess):
    key = 'icclim_vDTR'


########################################

# List of all Process classes used in __init__
D = locals()
ocgis_processes = [FreezeThawProcess, Duration] + [D[k] for k in cls_names]
