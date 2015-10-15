from os.path import basename
from dispel4py.core import GenericPE, NAME

from malleefowl import wpslogging as wpslogging
logger = wpslogging.getLogger(__name__)

try:
    from cloghandler import ConcurrentRotatingFileHandler as RFHandler
except ImportError:
    # Next 2 lines are optional:  issue a warning to the user
    from warnings import warn
    warn("ConcurrentLogHandler package not installed. Using builtin log handler")
    from logging.handlers import RotatingFileHandler as RFHandler

def get_status_logger(filename="status.log"):
    """
    :param filename: path to status log file.
    :return: a status logger for concurrent logging.
    """
    import logging as logging       
    status_logger = logging.getLogger('status_log')
    status_logger.setLevel(logging.INFO)

    formatter = logging.Formatter('%(asctime)-15s - %(message)s')

    # log handler
    fh = RFHandler(filename, "a", 10*1024*1024, 1) 
    fh.setLevel(logging.DEBUG)
    fh.setFormatter(formatter)
    status_logger.addHandler(fh)
    return status_logger

class BasePE(GenericPE):
    def __init__(self, out_dir='.', monitor=None):
        GenericPE.__init__(self)
        self.out_dir = out_dir
        def internal_monitor(message, progress):
            logger.info("%s, progress=%s/100", message, progress)
        if monitor is not None:
            self.monitor = monitor
        else:
            self.monitor = internal_monitor
        def internal_status(message, success=False):
            logger.info("%s, success=%s", message, success)
        self.status = internal_status

    def set_status_logger(self, status_logger):
        def log_status(message, success=False):
            status_logger.info("%s, success=%s", message, success)
        self.status = log_status

class Aggregate(BasePE):
    '''
    This PE takes gets netcdf input files and aggregates them by experiment.
    '''
    def __init__(self, resources, out_dir=None, monitor=None):
        BasePE.__init__(self, out_dir, monitor)
        self.resources = resources
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )
        self.count = 0

    def process(self, inputs):
        from urllib2 import urlparse
        # TODO: Dataset doest not like file:// urls
        local_files = [ urlparse.urlparse(url).path for url in self.resources ]
        from flyingpigeon.utils import aggregations
        aggs = aggregations( local_files )
        max_count = len(aggs)
        for key in aggs.keys():
            self.monitor('experiment=%s' % key, self.count * 100 / max_count)
            self.count = self.count + 1
            self.write('output', aggs[key])
        
class CalcIndice(BasePE):
    """
    This PE calls indices.calc_indice().
    """
    def __init__(self, indice, grouping, out_dir=None, monitor=None):
        BasePE.__init__(self, out_dir, monitor)
        self.grouping = grouping
        self.indice = indice

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )

    def process(self, inputs):
        from flyingpigeon.indices import calc_indice, indice_variable
        variable = inputs['resource'].get('variable')
        filename = inputs['resource'].get('filename')

        logger.debug('filename=%s, variable=%s, indice=%s' % ( filename, variable, self.indice))

        # does variable fit to indice?
        if variable == indice_variable(self.indice):
            logger.debug('start calc_indice ...')
            success = False
            try:
                output = calc_indice(
                    resources=inputs['resource'].get('files', []),
                    indice=self.indice,
                    grouping=self.grouping,
                    out_dir=self.out_dir)
                filename = basename(output)
                success = True
                logger.debug('calc_indice done. output=%s', output)
                self.write('output', output)
            except:
                logger.exception('indice calculation failed: indice=%s', self.indice)
            message = "process:calc_indice, filename:%s, params:indice=%s" % (filename, self.indice)
            self.status(message, success)
        else:
            logger.warn('skip file: variable=%s', variable)

class Clipping(BasePE):
    """
    This PE calls clipping.calc_region_clipping()
    """
    def __init__(self, region, out_dir=None, monitor=None):
        BasePE.__init__(self, out_dir, monitor)
        self.region = region

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )

    def process(self, inputs):
        logger.debug('start clipping ...')
        success = False
        filename = basename(inputs['resource'])
        try:
            from flyingpigeon.clipping import calc_region_clipping
            output = calc_region_clipping(
                resource=inputs['resource'],
                region=self.region,
                out_dir=self.out_dir)
            success = True
            logger.debug('clipping done. output=%s', output)
            self.write('output', output)
        except:
            logger.exception('clipping failed for region %s' % self.region)
        message = "process:clipping, filename:%s, params:region=%s" % (filename, self.region)
        self.status(message, success)

class Normalize(BasePE):
    """
    This PE calls clipping.normalize().
    """
    def __init__(self, region, start_date, end_date, out_dir=None, monitor=None):
        BasePE.__init__(self, out_dir, monitor)
        self.region = region
        self.start_date = start_date
        self.end_date = end_date

        self.inputconnections = {}
        self.inputconnections['resource'] = dict( NAME='resource' )
        self.outputconnections = {}
        self.outputconnections['output'] = dict( NAME='output' )

    def process(self, inputs):
        logger.debug('start normalize ...')
        success = False
        filename = basename(inputs['resource'])
        try:
            from flyingpigeon.clipping import normalize
            output = normalize(
                resource=inputs['resource'],
                region=self.region,
                start_date=self.start_date,
                end_date=self.end_date,
                out_dir=self.out_dir)
            success = True
            logger.debug('clipping done. output=%s', output)
            self.write('output', output)
        except:
            logger.exception('clipping failed for region %s' % self.region)
        message = "process:normalize, filename:%s, params:" % (filename)
        self.message(message, success)

class Results(BasePE):
    def __init__(self, max_results, out_dir='.', monitor=None):
        BasePE.__init__(self, out_dir, monitor)
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



