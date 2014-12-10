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
        # TODO: fix file:// url troubles ...
        from urllib2 import urlparse
        result = {}
        variable = indices_calculator.indice_variable(self.indice)
        if indices_calculator.has_variable(urlparse.urlparse(inputs['resource'][0]).path, variable):
            self.wps_inputs.append( ('grouping', self.grouping) )
            self.wps_inputs.append( ('indice', self.indice) )

            result = self.execute()
        else:
            result['output'] = None
        return result

class GroupByExperiment(GenericPE):
    '''
    This PE takes gets netcdf input files and groups them by experiment.
    '''
    def __init__(self, resources):
        GenericPE.__init__(self)
        self.resources = resources
        self.outputconnections = { 'output' : { NAME : 'output' } }
    def process(self, inputs):
        from urllib2 import urlparse
        # TODO: Dataset doest not like file:// urls
        local_files = [ urlparse.urlparse(url).path for url in self.resources ]
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


def climate_indice_workflow(url, resources, indices=['SU'], grouping='year', monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    graph = WorkflowGraph()
    group_by = GroupByExperiment(resources)
    #collect = CollectResults()

    count = 0
    for indice in indices:
        calc_indice = CalcSimpleIndice(url, indice=indice, grouping=grouping)
        calc_indice.set_monitor(monitor)

        graph.connect(group_by, 'output',  calc_indice, 'resource')
        #graph.connect(calc_indice, 'output', collect, 'input' )
        count = count + 1

    result = multiprocess(graph, 8, [{}])
    return result


