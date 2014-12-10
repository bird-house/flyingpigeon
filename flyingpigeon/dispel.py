from dispel4py.workflow_graph import WorkflowGraph
from dispel4py import simple_process
from dispel4py.core import GenericPE, NAME

from malleefowl.dispel import BaseWPS

from flyingpigeon import indices_calculator

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

class CalcSimpleIndice(BaseWPS):
    """
    This PE calls the simple_indice Web Processing Service to calculate a climate indice.
    """
    def __init__(self, url, indice, grouping):
        BaseWPS.__init__(self, url, 'simple_indice', output='output')
        self.grouping = grouping
        self.indice = indice
        
    def _process(self, inputs):
        self.wps_inputs.append( ('grouping', self.grouping) )
        self.wps_inputs.append( ('indice', self.indice) )

        result = self.execute()
        return result

class GroupByExperiment(GenericPE):
    '''
    This PE takes gets netcdf input files and groups them by experiment.
    '''
    def __init__(self):
        GenericPE.__init__(self)
        self.inputconnections = { 'input' : { NAME : 'resource' } }
        self.outputconnections = { 'output' : { NAME : 'output' } }
    def process(self, inputs):
        from urllib2 import urlparse
        # TODO: Dataset doest not like file:// urls
        local_files = [ urlparse.urlparse(url).path for url in inputs['resource'] ]
        exp_groups = indices_calculator.group_by_experiment( local_files )
        for key in exp_groups.keys():
            # TODO: wps needs file://
            nc_files = [ "file://%s" % path for path in exp_groups[key] ]
            self.write('output', nc_files)

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

def climate_indice_workflow(url, resources, indices=['SU'], grouping='year', monitor=None):
    graph = WorkflowGraph()

    # nodes
    wget = TestOneInOneOut()
    group_by_experiment = GroupByExperiment()
    collect = CollectResults(num_inputs = len(indices))
    
    graph.connect(wget, 'output', group_by_experiment, 'resource')

    count = 0
    for indice in indices:
        calc_indice = CalcSimpleIndice(url, indice=indice, grouping=grouping)
        calc_indice.set_monitor(monitor)

        graph.connect(group_by_experiment, 'output', calc_indice, 'resource')
        graph.connect(calc_indice, 'output', collect, 'input%s' % count )
        count = count + 1

    result = simple_process.process(graph, inputs={ wget : [{'input' : resources }] })
    return result


