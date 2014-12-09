from malleefowl.dispel import BaseWPS
from dispel4py.workflow_graph import WorkflowGraph
from dispel4py import simple_process
from dispel4py.core import GenericPE, NAME

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

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


