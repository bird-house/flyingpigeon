from dispel4py.core import GenericPE, NAME

from malleefowl.dispel import BaseWPS

from flyingpigeon import indices_calculator

class CalcSimpleIndice(BaseWPS):
    """
    This PE calls the simple_indice Web Processing Service to calculate a climate indice.
    """
    def __init__(self, url, indice, grouping, out_dir=None):
        BaseWPS.__init__(self, url, 'simple_indice', output='output')
        self.grouping = grouping
        self.indice = indice
        self.out_dir = out_dir
        
    def _process(self, inputs):
        # TODO: fix file:// url troubles ...
        from urllib2 import urlparse
        result = None
        variable = indices_calculator.indice_variable(self.indice)
        if indices_calculator.has_variable(urlparse.urlparse(inputs['resource'][0]).path, variable):
            self.wps_inputs.append( ('grouping', self.grouping) )
            self.wps_inputs.append( ('indice', self.indice) )

            result = self.execute()

            # TODO: fix output collection
            from os.path import join, curdir
            if self.out_dir is None:
                self.out_dir = curdir
            outfile = join(self.out_dir, 'status_locations.txt')
            with open(outfile, 'a') as fp: 
                fp.write(result['status_location'] + '\n')
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
            self.log('starting experiment: %s' % key)
            nc_files = [ "file://%s" % path for path in exp_groups[key] ]
            self.write('output', nc_files)

def climate_indice_workflow(url, resources, indices=['SU'], grouping='year', out_dir=None, monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    from os.path import curdir
    if out_dir is None:
        out_dir = curdir

    graph = WorkflowGraph()
    group_by = GroupByExperiment(resources)

    for indice in indices:
        calc_indice = CalcSimpleIndice(url, indice=indice, grouping=grouping, out_dir=out_dir)
        calc_indice.set_monitor(monitor)

        graph.connect(group_by, 'output',  calc_indice, 'resource')

    from multiprocessing import cpu_count
    numProcesses = 2 * cpu_count()

    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)

    # TODO: fix output collection
    from os.path import join
    outfile = join(out_dir, 'status_locations.txt')
    result = []
    with open(outfile, 'r') as fp:
        for line in fp.readlines():
            status_location = line.strip()
            if not 'http://' in status_location:
                continue
            from owslib.wps import WPSExecution
            execution = WPSExecution()
            execution.checkStatus(url=status_location, sleepSecs=0)
            result.append(dict(
                output=execution.processOutputs[0].reference,
                status=execution.status,
                status_location=execution.statusLocation))
    return result


