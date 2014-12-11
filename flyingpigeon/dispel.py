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

    def process(self, inputs):
        # TODO: fix file:// url troubles ...
        from urllib2 import urlparse
        variable = indices_calculator.indice_variable(self.indice)
        filename = urlparse.urlparse(inputs['resource'][0]).path
        
        if indices_calculator.has_variable(filename, variable):
            output = indices_calculator.calc_indice(
                resources=inputs['resource'],
                indice=self.indice,
                grouping=self.grouping,
                out_dir=self.out_dir)
            self.write('output', output)

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

class Results(GenericPE):
    def __init__(self, out_dir='.'):
        GenericPE.__init__(self)
        from os.path import join
        self.outfile = join(out_dir, 'outputs.json')
        self.outputs = []
        self.inputconnections = { 'input' : { NAME : 'input'} }
    def process(self, inputs):
        self.outputs.append(inputs['input'])

    def postprocess(self):
        import json
        with open(self.outfile, 'w') as fp:
            json.dump(obj=self.outputs, fp=fp, indent=4, sort_keys=True)

    def get_outputs(self):
        outputs = []
        import json
        with open(self.outfile, 'r') as fp:
            outputs = json.load(fp=fp)
        return outputs
        

def climate_indice_workflow(resources, indices=['SU'], grouping='year', out_dir=None, monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    graph = WorkflowGraph()
    group_by = GroupByExperiment(resources)
    results = Results(out_dir)

    for indice in indices:
        calc_indice = CalcSimpleIndice(indice=indice, grouping=grouping, out_dir=out_dir)

        graph.connect(group_by, 'output',  calc_indice, 'resource')
        graph.connect(calc_indice, 'output', results, 'input')

    from multiprocessing import cpu_count
    numProcesses = 2 * cpu_count()
    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)

    return results.get_outputs()


