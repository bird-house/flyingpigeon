import ocgis
from ocgis.util.helpers import get_sorted_uris_by_time_dimension
from netCDF4 import Dataset

from malleefowl.dispel import BaseWPS
from dispel4py.workflow_graph import WorkflowGraph
from dispel4py import simple_process
from dispel4py.core import GenericPE, NAME

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

def indice_workflow(url, resources, indices=['SU'], grouping='year', monitor=None):
    graph = WorkflowGraph()

    wget = TestOneInOneOut()

    collect = CollectResults(num_inputs = len(indices))

    count = 0
    for indice in indices:
        calc_indice = CalcSimpleIndice(url, indice=indice, grouping=grouping)
        calc_indice.set_monitor(monitor)

        graph.connect(wget, 'output', calc_indice, 'resource')
        graph.connect(calc_indice, 'output', collect, 'input%s' % count )
        count = count + 1

    result = simple_process.process(graph, inputs={ wget : [{'input' : resources }] })
    return result

def calc_indice(resources, indice="SU", variable="tasmax", grouping="year", out_dir=None):
    """
    Calculates given indice for variable and grouping.

    resources: single filename or list of filenames (netcdf)
    out_dir: output directory for result file (netcdf)

    result: netcdf files with calculated indices
    """
    prefix = variable + '_' + indice
        
    calc_icclim = [{'func' : 'icclim_' + indice, 'name' : indice}]
    try:
        rd = ocgis.RequestDataset(uri=_sort_by_time(resources), variable=variable) # TODO: time_range=[dt1, dt2]
        result = ocgis.OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=_calc_grouping(grouping),
            prefix=prefix,
            output_format='nc',
            dir_output=out_dir,
            add_auxiliary_files=False).execute()
    except:
        logger.exception('Could not calc indice %s with variable %s for file %s.', indice, variable, uri)

    return result

def _calc_grouping(grouping):
    calc_grouping = ['year'] # default year
    if grouping == 'sem':
        calc_grouping = [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] 
    elif grouping in ['year', 'month']:
        calc_grouping = [grouping]
    else:
        msg = 'Unknown calculation grouping: %s' % grouping
        logger.error(msg)
        raise Exception(msg)
    return calc_grouping

def _sort_by_time(resources):
    if type(resources) is list:
        sorted_list = get_sorted_uris_by_time_dimension(resources)
    else:
        sorted_list = [resources]
    return sorted_list


class CalcSimpleIndice(BaseWPS):
    def __init__(self, url, indice, grouping):
        BaseWPS.__init__(self, url, 'simple_indice', output='output')
        self.grouping = grouping
        self.indice = indice
        
    def _process(self, inputs):
        self.wps_inputs.append( ('grouping', self.grouping) )
        self.wps_inputs.append( ('indice', self.indice) )
        
        return self.execute()

class CollectResults(GenericPE):
    '''
    This PE takes num_inputs inputs and it merges into one oputput.  
    '''
    def __init__(self, num_inputs):
        GenericPE.__init__(self)
        if num_inputs == 1:
            self.inputconnections = { 'input' : { NAME : 'input' } }
        else:
            for i in range(num_inputs):
                self.inputconnections['input%s' % i] = { NAME : 'input%s' % i } 
        self.outputconnections = { 'output' : { NAME : 'output' } }
    def process(self, inputs):
        # print '%s: inputs %s' % (self.id, inputs)
        result = ''
        for inp in self.inputconnections:
            if inp in inputs:
                result += '%s' % (inputs[inp])
        if result:
            # print '%s: result %s' % (self.id, result)
            return { 'output' : result }


class TestOneInOneOut(GenericPE):
    '''
    This PE copies the input to an output. 
    '''
    def __init__(self, delay=1):
        GenericPE.__init__(self)
        self.inputconnections = { 'input' : { NAME : 'input' } }
        self.outputconnections = { 'output' : { NAME : 'output' } }
    def process(self, inputs):
        # self.log('Processing inputs %s' % inputs)
        print inputs
        return { 'output' : inputs['input'] }
