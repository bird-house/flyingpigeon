from dispel4py.core import GenericPE, NAME

from malleefowl.dispel import BaseWPS

from flyingpigeon import indices_calculator

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class CalcSimpleIndice(GenericPE):
    """
    This PE calls the simple_indice Web Processing Service to calculate a climate indice.
    """
    def __init__(self, indice, grouping, out_dir=None):
        GenericPE.__init__(self)
        self.grouping = grouping
        self.indice = indice
        self.out_dir = out_dir

        self.inputconnections = { 'resource' : { NAME : 'resource' } }
        self.outputconnections = { 'output' : { NAME : 'output' } }

    def _process(self, inputs):
        # TODO: fix file:// url troubles ...
        from urllib2 import urlparse
        result = dict(output='ProcessFailed')
        variable = indices_calculator.indice_variable(self.indice)

        filename = urlparse.urlparse(inputs['resource'][0]).path
        
        if indices_calculator.has_variable(filename, variable):
            output = indices_calculator.calc_indice(
                resources=inputs['resource'],
                indice=self.indice,
                grouping=self.grouping,
                out_dir=self.out_dir)
                
            logger.info('output %s', output)

            # TODO: fix output collection
            if output is not None:
                result['output'] = output
            from os.path import join, curdir
            if self.out_dir is None:
                self.out_dir = curdir
            outfile = join(self.out_dir, 'output.txt')
            with open(outfile, 'a') as fp: 
                fp.write(result['output'] + '\n')
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
            #from os.path import abspath
            #nc_files = [ "file://%s" % abspath(path) for path in exp_groups[key] ]
            self.write('output', exp_groups[key])

class CollectResults(GenericPE):
    def __init__(self):
        GenericPE.__init__(self)
        self.results = []
        self.inputconnections = { 'input' : { NAME : 'input'} }
    def process(self, inputs):
        logger.info('add result %s', inputs)
        self.results.append(inputs['input'])
        logger.info("num results = %s", len(self.results))
    def get_results(self):
        logger.info('num results = %s', len(self.results))
        return self.results

def climate_indice_workflow(resources, indices=['SU'], grouping='year', out_dir=None, monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    #from os.path import abspath
    #new_resources = [abspath(resource) for resource in resources]

    from os.path import curdir
    if out_dir is None:
        out_dir = curdir

    graph = WorkflowGraph()
    group_by = GroupByExperiment(resources)
    collect = CollectResults()

    for indice in indices:
        calc_indice = CalcSimpleIndice(indice=indice, grouping=grouping, out_dir=out_dir)

        graph.connect(group_by, 'output',  calc_indice, 'resource')
        graph.connect(calc_indice, 'output', collect, 'input')

    from multiprocessing import cpu_count
    numProcesses = 2 * cpu_count()

    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)

    # TODO: fix output collection
    from os.path import join
    outfile = join(out_dir, 'output.txt')
    result = []
    
    with open(outfile, 'r') as fp:
        for line in fp.readlines():
            output = line.strip()
            result.append(dict(output=output))
    return collect.get_results()


