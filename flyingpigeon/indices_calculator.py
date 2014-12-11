import ocgis
from ocgis.util.helpers import get_sorted_uris_by_time_dimension
from netCDF4 import Dataset

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

_INDICES_ = dict(
    SU=dict(variable='tasmax', description='Nr of summer days (tasmax as input files)'),
    TG=dict(variable='tas', description='Mean of mean temperatur (tas as input files)'),
)

def indices():
    """
    :return: a list of all climate indices.
    """
    return _INDICES_.keys()

def indices_description():
    """
    :return: a discription of all climate indices.
    """
    description = ''
    for indice in indices():
        description = description + "%s: %s\n" % (indice, _INDICES_[indice]['description'])
    return description

def indice_variable(indice):
    """
    :return: variable (tasmax, tas, ...) which can be used for the climate indice.
    """
    variable = None
    try:
        variable = _INDICES_[indice]['variable']
    except:
        logger.error('unknown indice %s', indice)
    return variable

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

def calc_grouping(grouping):
    calc_grouping = ['year'] # default year
    if grouping == 'sem':
        calc_grouping = [ [12,1,2], [3,4,5], [6,7,8], [9,10,11], 'unique'] 
    elif grouping in ['year', 'month']:
        calc_grouping = [grouping]
    else:
        msg = 'Unknown calculation grouping: %s' % grouping
        logger.error(msg)
        raise Exception(msg)
    return calc_grouping

def sort_by_time(resources):
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

def calc_indice(resources=[], indice="SU", grouping="year", out_dir=None):
    """
    Calculates given indice for variable and grouping.

    :param resources: list of filenames (netcdf)
    :param out_dir: output directory for result file (netcdf)

    :return: netcdf files with calculated indices
    """

    ## ocgis.env.OVERWRITE = True
    ## ocgis.env.DIR_DATA = os.path.curdir
    ## ocgis.env.DIR_OUTPUT = outdir    
    ## output_crs = None

    result = None
    calc_icclim = [{'func' : 'icclim_' + indice, 'name' : indice}]
    filename = None
    try:
        groups = group_by_experiment(resources)
        if len(groups) > 1:
            logger.warning('more than one expermint group selected: %s', groups.keys())
        if len(groups) == 0:
            raise Exception('no valid input data found!')
        group_name = groups.keys()[0]
        logger.debug('group = %s', group_name)
        nc_files = groups[group_name]
        variable = indice_variable(indice)
        import uuid
        #prefix = '%s_%s_%s' % (indice, variable, uuid.uuid4().get_hex())
        prefix = '%s_%s' % (indice, group_name)
        filename = prefix + '.nc'
        logger.debug('calculating %s', filename)
        rd = ocgis.RequestDataset(uri=nc_files, variable=variable) # TODO: time_range=[dt1, dt2]
        output = ocgis.OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=calc_grouping(grouping),
            prefix=prefix,
            output_format='nc',
            dir_output=out_dir,
            add_auxiliary_files=False).execute()
    except:
        # TODO: should raise exception?
        logger.exception('Could not calc indice %s', indice)

    return dict(output=output, drs_filename=filename)

