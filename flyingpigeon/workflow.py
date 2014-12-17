from dispel4py.workflow_graph import WorkflowGraph
from dispel4py.multi_process import multiprocess

from .base import Aggregate, Results, CalcIndice, Clipping, Normalize, StatusLog

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

def climate_indice_workflow(
        resources,
        indices=['SU'],
        grouping='year',
        regions=['FRA'],
        start_date=None,
        end_date=None,
        out_dir=None,
        monitor=None):
    
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
            clipping = Clipping(region=region, monitor=monitor)
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


