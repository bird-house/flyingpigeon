from os.path import basename
from dispel4py.core import GenericPE, NAME

#from malleefowl.dispel import BaseWPS

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

class BasePE(GenericPE):
    def __init__(self, monitor=None):
        GenericPE.__init__(self)
        def internal_monitor(message, progress):
            logger.info("%s, progress=%s/100", message, progress)
        if monitor is not None:
            self.monitor = monitor
        else:
            self.monitor = internal_monitor

class Aggregate(BasePE):
    '''
    This PE takes gets netcdf input files and aggregates them by experiment.
    '''
    def __init__(self, resources, monitor=None):
        BasePE.__init__(self, monitor)
        self.resources = resources
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )
        #self.outputconnections['metadata'] = dict( NAME='metadata' )
        self.count = 0

    def process(self, inputs):
        from urllib2 import urlparse
        # TODO: Dataset doest not like file:// urls
        local_files = [ urlparse.urlparse(url).path for url in self.resources ]
        from flyingpigeon.utils import aggregations
        aggs = aggregations( local_files )
        max_count = len(aggs)
        for key in aggs.keys():
            # TODO: wps needs file://
            #from os.path import abspath
            #nc_files = [ "file://%s" % abspath(path) for path in exp_groups[key] ]
            self.monitor('experiment=%s' % key, self.count * 100 / max_count)
            self.count = self.count + 1
            self.write('output', aggs[key])
        
class CalcIndice(BasePE):
    """
    This PE calls indices.calc_indice().
    """
    def __init__(self, indice, grouping, out_dir=None, monitor=None):
        BasePE.__init__(self, monitor)
        self.grouping = grouping
        self.indice = indice
        self.out_dir = out_dir

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )
        self.outputconnections['status_log'] = dict( NAME='status_log' ) 

    def process(self, inputs):
        from flyingpigeon.indices import calc_indice, indice_variable
        variable = inputs['resource'].get('variable')
        filename = inputs['resource'].get('filename')

        logger.debug('filename=%s, variable=%s, indice=%s' % ( filename, variable, self.indice))

        # does variable fit to indice?
        if variable == indice_variable(self.indice):
            logger.debug('start calc_indice ...')
            success = 'Failed'
            try:
                output = calc_indice(
                    resources=inputs['resource'].get('files', []),
                    indice=self.indice,
                    grouping=self.grouping,
                    out_dir=self.out_dir)
                filename = basename(output)
                success = 'Succeeded'
                logger.debug('calc_indice done. output=%s', output)
                self.write('output', output)
            except:
                logger.exception('indice calculation failed: indice=%s', self.indice)
            status_log = "process:calc_indice, filename:%s, params:indice=%s, status:%s" % (filename, self.indice, success)
            self.write('status_log', status_log)
        else:
            logger.warn('skip file: variable=%s', variable)

class CalcClipping(BasePE):
    """
    This PE calls clipping.calc_region_clipping()
    """
    def __init__(self, region, out_dir=None, monitor=None):
        BasePE.__init__(self, monitor)
        self.region = region
        self.out_dir = out_dir

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )
        self.outputconnections['status_log'] = dict( NAME='status_log' ) 

    def process(self, inputs):
        logger.debug('start clipping ...')
        success = 'Failed'
        filename = basename(inputs['resource'])
        try:
            from flyingpigeon.clipping import calc_region_clipping
            output = calc_region_clipping(
                resource=inputs['resource'],
                region=self.region,
                out_dir=self.out_dir)
            success = 'Succeeded'
            logger.debug('clipping done. output=%s', output)
            self.write('output', output)
        except:
            logger.exception('clipping failed for region %s' % self.region)
        status_log = "process:clipping, filename:%s, params:region=%s, status:%s" % (filename, self.region, success)
        self.write('status_log', status_log)

class Normalize(BasePE):
    """
    This PE calls clipping.normalize().
    """
    def __init__(self, region, start_date, end_date, out_dir=None, monitor=None):
        BasePE.__init__(self, monitor)
        self.region = region
        self.start_date = start_date
        self.end_date = end_date
        self.out_dir = out_dir

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )
        self.outputconnections['status_log'] = dict( NAME='status_log' ) 

    def process(self, inputs):
        logger.debug('start normalize ...')
        success = 'Failed'
        filename = basename(inputs['resource'])
        try:
            from flyingpigeon.clipping import normalize
            output = normalize(
                resource=inputs['resource'],
                region=self.region,
                start_date=self.start_date,
                end_date=self.end_date,
                out_dir=self.out_dir)
            success = 'Succeeded'
            logger.debug('clipping done. output=%s', output)
            self.write('output', output)
        except:
            logger.exception('clipping failed for region %s' % self.region)
        status_log = "process:normalize, filename:%s, params:, status:%s" % (filename, success)
        self.write('status_log', status_log)

class StatusLog(BasePE):
    def __init__(self, out_dir='.', monitor=None):
        BasePE.__init__(self, monitor)
        from os.path import join
        self.outfile = join(out_dir, 'status.log')
        self.inputconnections = { 'status_log' : { NAME : 'status_log'} }

    def process(self, inputs):
        from os.path import basename
        output = inputs['status_log']
        with open(self.outfile, 'a') as fp:
            fp.write("%s\n" % (inputs['status_log']))
            fp.flush()

    def get_status_log(self):
        logs = []
        with open(self.outfile, 'r') as fp:
            for line in fp.readlines():
                logs.append(line.strip())
        return logs
            
class Results(BasePE):
    def __init__(self, max_results, out_dir='.', monitor=None):
        BasePE.__init__(self, monitor)
        from os.path import join
        self.outfile = join(out_dir, 'outputs.txt')
        self.inputconnections = { 'input' : { NAME : 'input'} }
        self.max_results = max_results
        self.count = 0
        
    def process(self, inputs):
        output = inputs['input']
        self.count = self.count + 1
        self.monitor('output=%s' % basename(output), self.count * 100 / self.max_results)
        with open(self.outfile, 'a') as fp:
            fp.write("%s\n" % (inputs['input']))
            fp.flush()

    def get_outputs(self):
        outputs = []
        with open(self.outfile, 'r') as fp:
            for line in fp.readlines():
                outputs.append(line.strip())
        return outputs

def climate_indice_workflow(
        resources,
        indices=['SU'],
        grouping='year',
        regions=['FRA'],
        start_date=None,
        end_date=None,
        out_dir=None,
        monitor=None):
    from dispel4py.workflow_graph import WorkflowGraph
    from dispel4py.multi_process import multiprocess

    # make indice and region list unique
    indices = set(indices)
    regions = set(regions)

    # build workflow graph
    graph = WorkflowGraph()

    # start with experiment aggregation
    aggregate = Aggregate(resources, monitor=monitor)
    status_log = StatusLog(out_dir=out_dir, monitor=monitor)

    # need result node to collect results
    results = Results(max_results=len(resources), out_dir=out_dir, monitor=monitor)
    
    # loop all indices
    for indice in indices:
        # calc indice with status log
        calc_indice = CalcIndice(indice=indice, grouping=grouping, out_dir=out_dir, monitor=monitor)
        graph.connect(aggregate, 'output',  calc_indice, 'resource')
        graph.connect(calc_indice, 'status_log', status_log, 'status_log')

        # loop all regions
        for region in regions:
            # clipping with status log
            clipping = CalcClipping(region=region, monitor=monitor)
            graph.connect(calc_indice, 'output', clipping, 'resource')
            graph.connect(clipping, 'status_log', status_log, 'status_log')

            # normalize with status log
            #normalize = Normalize(region=region, start_date=start_date, end_date=end_date, monitor=monitor)
            #graph.connect(clipping, 'output', normalize, 'resource')
            #graph.connect(clipping, 'status_log', status_log, 'status_log')

            # collect results
            graph.connect(clipping, 'output', results, 'input')
            
    # ... now let's run the workflow on max 4 CPUs
    from multiprocessing import cpu_count
    numProcesses = min(cpu_count(), 4)  # max 4 cpus

    logger.debug('start multiprocessesing workflow')
    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)
    logger.debug('workflow done')
    
    return results.get_outputs(), status_log.get_status_log()


