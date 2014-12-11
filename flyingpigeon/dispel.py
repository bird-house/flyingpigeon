from dispel4py.core import GenericPE, NAME

from malleefowl.dispel import BaseWPS

from flyingpigeon import indices_calculator

import logging
logger = logging.getLogger(__name__)

class BasePE(GenericPE):
    def __init__(self):
        GenericPE.__init__(self)
    def debug(self, message):
        with open('dispel.log', 'a') as fp:
            fp.write('DEBUG: %s\n' % (message))
            fp.flush()

class CalcSimpleIndice(BasePE):
    """
    This PE calls the simple_indice Web Processing Service to calculate a climate indice.
    """
    def __init__(self, indice, grouping, out_dir=None):
        BasePE.__init__(self)
        self.grouping = grouping
        self.indice = indice
        self.out_dir = out_dir

        self.inputconnections = { 'resource' : { NAME : 'resource' } }
        self.outputconnections = { 'output' : { NAME : 'output' } }

    def process(self, inputs):
        # TODO: fix file:// url troubles ...
        from urllib2 import urlparse
        variable = indices_calculator.indice_variable(self.indice)
        filename = urlparse.urlparse(inputs['resource'][0]).path

        self.debug('filename=%s, variable=%s, indice=%s' % ( filename, variable, self.indice))
        
        if indices_calculator.has_variable(filename, variable):
            self.debug('start calculation ...')
            output = indices_calculator.calc_indice(
                resources=inputs['resource'],
                indice=self.indice,
                grouping=self.grouping,
                out_dir=self.out_dir)
            self.debug('calc_indice output=%s' % output)
            self.write('output', output)
        else:
            self.debug('skip file: has not variable %s' % (variable))

class GroupByExperiment(BasePE):
    '''
    This PE takes gets netcdf input files and groups them by experiment.
    '''
    def __init__(self, resources):
        BasePE.__init__(self)
        self.resources = resources
        self.outputconnections = { 'output' : { NAME : 'output' } }

    def process(self, inputs):
        from urllib2 import urlparse
        # TODO: Dataset doest not like file:// urls
        local_files = [ urlparse.urlparse(url).path for url in self.resources ]
        self.debug("%s" % local_files)
        exp_groups = indices_calculator.group_by_experiment( local_files )
        for key in exp_groups.keys():
            # TODO: wps needs file://
            #from os.path import abspath
            #nc_files = [ "file://%s" % abspath(path) for path in exp_groups[key] ]
            self.debug('start with experiment group %s' % key)
            self.write('output', exp_groups[key])

class Results(BasePE):
    def __init__(self, out_dir='.'):
        BasePE.__init__(self)
        from os.path import join
        self.outfile = join(out_dir, 'outputs.json')
        self.inputconnections = { 'input' : { NAME : 'input'} }
        
    def process(self, inputs):
        self.debug('write result')
        with open(self.outfile, 'a') as fp:
            fp.write("%s\n" % (inputs['input']['output']))
            fp.flush()

    def get_outputs(self):
        outputs = []
        with open(self.outfile, 'r') as fp:
            for line in fp.readlines():
                outputs.append(line.strip())
        return outputs

def climate_indice_workflow(resources, indices=['SU'], grouping='year', out_dir=None, monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    graph = WorkflowGraph()
    group_by = GroupByExperiment(resources)
    results = Results(out_dir)

    # make indices list unique
    indices = set(indices)
    
    for indice in indices:
        calc_indice = CalcSimpleIndice(indice=indice, grouping=grouping, out_dir=out_dir)

        graph.connect(group_by, 'output',  calc_indice, 'resource')
        graph.connect(calc_indice, 'output', results, 'input')

    from multiprocessing import cpu_count
    numProcesses = cpu_count()

    logger.debug('run multiprocess')
    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)
    logger.debug('done')
    
    return results.get_outputs()


