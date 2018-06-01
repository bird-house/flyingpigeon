from netCDF4 import Dataset
import logging
LOGGER = logging.getLogger("PYWPS")


from eggshell.ocgis_utils import call

def has_Lambert_Conformal(resource):
    """
    Check if grid is organised as Lambert_Conformal

    :param resource: file to be checked

    :return Boolean: True/False
    """
    if type(resource) != list:
        resource = [resource]
    for nc in resource:
        ds = Dataset(nc)
        if 'Lambert_Conformal' not in ds.variables.keys():
            return False
    return True

def eval_timerange(resource, time_range):
    """
    quality checker if given time_range is covered by timesteps in resource files

    :param resource: input netCDF files
    :param time_range: start and end date of time range [datetime,datetime]

    :returns [datetime,datetime]: time_range
    """
    from flyingpigeon.utils import get_time

    LOGGER.info('time_range: %s' % time_range)

    if type(resource) != str:
        resource.sort()
    time = get_time(resource)
    start = time[0]
    end = time[-1]

    if (time_range[0] > start or time_range[0] < end):
        LOGGER.debug('time range start %s not in input dataset covering: %s to %s' % (time_range[0], start, end))
        time_range[0] = start
    LOGGER.debug('time_range start changed to first timestep of dataset')
    if (time_range[1] > end or time_range[1] < start):
        LOGGER.debug('time range end %s not in input dataset covering: %s to %s' % (time_range[0], start, end))
        time_range[1] = end
    LOGGER.debug('time_range end changed to last timestep of dataset')
    if (time_range[0] > time_range[1]):
        time_range = reversed(time_range)
        LOGGER.debug('time range reversed! start was later than end ')
    LOGGER.info('time range start and end set')
    return time_range

# # check memory load
# from os import stat
#   if memory_limit == None:
#     f = FreeMemory()
#     mem_kb = f.user_free
#     mem_mb = mem_kb / 1024.
#     mem_limit = mem_mb / 2. # set limit to half of the free memory
#   else:
#     mem_limit = memory_limit
#
#   if mem_limit >= 1024. * 4:
#     mem_limit = 1024. * 4
#     # 475.0 MB for openDAP
#
#   #if type(resource) == list :
#     #data_kb =  stat(resource[0]).st_size * len(resource)
#   #else:
#     #data_kb =  stat(resource).st_size
#   size = ops.get_base_request_size()['total']
#   data_kb = size['total']/reduce(lambda x,y: x*y,size['variables'][variable]['value']['shape'])
#   data_mb = data_kb / 1024.
#
#   if variable == None:
#     variable = rd.variable
#     LOGGER.info('%s as variable dedected' % (variable))
