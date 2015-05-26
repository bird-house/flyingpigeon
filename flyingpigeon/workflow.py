from dispel4py.workflow_graph import WorkflowGraph
from dispel4py.multi_process import multiprocess

from .base import Aggregate, Results, CalcIndice, Clipping, Normalize, get_status_logger

from malleefowl import wpslogging as logging
logger = logging.getLogger(__name__)

def run(graph):
    # ... now let's run the workflow
    from multiprocessing import cpu_count
    numProcesses = min(2*cpu_count(), 8)  # max 8 processes

    logger.debug('start multiprocessesing workflow')
    multiprocess(graph, numProcesses=numProcesses, inputs=[{}], simple=False)
    logger.debug('workflow done')

def calc_indice(
        resource,
        indices=['SU'],
        grouping='year',
        out_dir=None,
        status_log=None,
        monitor=None):
    
    # make indice and region list unique
    indices = set(indices)

    # status logger
    status_logger = get_status_logger(filename=status_log)

    # build workflow graph
    graph = WorkflowGraph()

    # start with experiment aggregation
    aggregate = Aggregate(resource, out_dir=out_dir, monitor=monitor)
    aggregate.set_status_logger(status_logger)

    # need result node to collect results
    results = Results(max_results=len(resource), out_dir=out_dir, monitor=monitor)
    results.set_status_logger(status_logger)
    
    # loop all indices
    for indice in indices:
        # calc indice with status log
        calc_indice = CalcIndice(indice=indice, grouping=grouping, out_dir=out_dir, monitor=monitor)
        calc_indice.set_status_logger(status_logger)
        graph.connect(aggregate, 'output',  calc_indice, 'resource')
        graph.connect(calc_indice, 'output', results, 'input')
            
    run(graph)    
    return results.get_outputs()


def calc_indice_with_clipping(
        resource,
        indices=['SU'],
        grouping='year',
        regions=['FRA'],
        start_date=None,
        end_date=None,
        out_dir=None,
        status_log=None,
        monitor=None):
    
    # make indice and region list unique
    indices = set(indices)
    regions = set(regions)

    # status logger
    status_logger = get_status_logger(filename=status_log)

    # build workflow graph
    graph = WorkflowGraph()
    # start with experiment aggregation
    aggregate = Aggregate(resource, monitor=monitor)
    aggregate.set_status_logger(status_logger)
    # need result node to collect results
    results = Results(max_results=len(resource), out_dir=out_dir, monitor=monitor)
    results.set_status_logger(status_logger)
    
    # loop all indices
    for indice in indices:
        # calc indice with status log
        calc_indice = CalcIndice(indice=indice, grouping=grouping, out_dir=out_dir, monitor=monitor)
        calc_indice.set_status_logger(status_logger)
        graph.connect(aggregate, 'output',  calc_indice, 'resource')

        # loop all regions
        for region in regions:
            # clipping with status log
            clipping = Clipping(region=region, monitor=monitor)
            clipping.set_status_logger(status_logger)
            graph.connect(calc_indice, 'output', clipping, 'resource')

            # normalize with status log
            # normalize = Normalize(region=region, start_date=start_date, end_date=end_date, monitor=monitor)
            # graph.connect(clipping, 'output', normalize, 'resource')

            # collect results
            graph.connect(clipping, 'output', results, 'input')
            
    run(graph)
    
    return results.get_outputs()
   
def calc_cordexviewer(
        resource,
        indices=['SU'],
        grouping='year',
        regions=['FRA'],
        #start_date=None,
        #end_date=None,
        out_dir=None,
        status_log=None,
        monitor=None
        ):
  
  results = calc_indice_with_clipping(
    resource,
    indices=indices,
    grouping=grouping,
    regions=regions,
    #start_date=None,
    #end_date=None,
    #out_dir=None,
    status_log=status_log,
    monitor=monitor
    )    
  
  return results
  
        



