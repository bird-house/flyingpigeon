import ocgis

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

def drs_filename( nc_file, skip_timestamp=False, skip_format=False ):
    """
    generates filename according to the data reference syntax (DRS).
    
    http://cmip-pcmdi.llnl.gov/cmip5/docs/cmip5_data_reference_syntax.pdf
    https://pypi.python.org/pypi/drslib

    :param nc_file: netcdf file
    :param skip_timestamp: if True then from/to timestamp is not added to the filename
                           (default: False)
    :return: DRS filename
    """
    from netCDF4 import Dataset

    ds = Dataset(nc_file)
    rd = ocgis.RequestDataset(nc_file)

    # CORDEX example: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    cordex_pattern = "{variable}_{domain}_{driving_model}_{experiment}_{ensemble}_{model}_{version}_{frequency}"
    # CMIP5 example: tas_MPI-ESM-LR_historical_r1i1p1
    cmip5_pattern = "{variable}_{model}_{experiment}_{ensemble}"

    filename = nc_file
    try:
        if ds.project_id == 'CORDEX':
            filename = cordex_pattern.format(
                variable = rd.variable,
                domain = ds.CORDEX_domain,
                driving_model = ds.driving_model_id,
                experiment = ds.experiment_id,
                ensemble = ds.driving_model_ensemble_member,
                model = ds.model_id,
                version = ds.rcm_version_id,
                frequency = ds.frequency)
        elif ds.project_id == 'CMIP5':
            # TODO: attributes missing in netcdf file for name generation?
            filename = cmip5_pattern.format(
                variable = rd.variable,
                model = ds.model_id,
                experiment = ds.experiment,
                ensemble = ds.parent_experiment_rip
                )
        else:
            raise Exception('unknown project %s' % ds.project_id)

        # add from/to timestamp if not skipped
        if skip_timestamp == False:
            time_list = ds.variables['time']
            from datetime import datetime, timedelta
            reftime = datetime.strptime('1949-12-01', '%Y-%m-%d')
            from_timestamp = datetime.strftime(reftime + timedelta(days=time_list[0]), '%Y%m%d') 
            to_timestamp = datetime.strftime(reftime + timedelta(days=time_list[-1]), '%Y%m%d')
            filename = "%s_%s-%s" % (filename, int(from_timestamp), int(to_timestamp))

        # add format extension
        if skip_format == False:
            filename = filename + '.nc'
    except:
        logger.exception('Could not generate DRS filename for %s', nc_file)
    
    return filename

def group_by_experiment(nc_files):
    """
    groups nc_files by experiment name. Experiment examples:
    
    CORDEX: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    CMIP5:

    We collect for each experiment all files on the time axis:
    200101-200512, 200601-201012, ...

    Time axis is sorted by time.

    :param nc_files: list of netcdf files
    :return: dictonary with key=experiment name and value=list of netcdf files
    """
    
    groups = {}
    for nc_file in nc_files:
        key = drs_filename(nc_file, skip_timestamp=True, skip_format=True)

        # collect files of each group (time axis)
        if groups.has_key(key):
            groups[key].append(nc_file)
        else:
            groups[key] = [nc_file]

        # sort files by time
        for key in groups.keys():
            groups[key] = sort_by_time(groups[key])   
    return groups

def sort_by_time(resources):
    from ocgis.util.helpers import get_sorted_uris_by_time_dimension
    if type(resources) is list:
        sorted_list = get_sorted_uris_by_time_dimension(resources)
    else:
        sorted_list = [resources]
    return sorted_list

def has_variable(resource, variable):
    success = False
    try:
        rd = ocgis.RequestDataset(uri=resource)
        success = rd.variable == variable
    except:
        logger.exception('has_variable failed.')
    return success

