import ocgis
from ocgis.util.helpers import get_sorted_uris_by_time_dimension
from netCDF4 import Dataset

from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

_INDICES_ = dict(
    SU=dict(variable='tasmax', description='Nr of summer days (tasmax as input files)'),
    TG=dict(variable='tas', description='Mean of mean temperatur (tas as input files)'),
)

def indices():
    """
    returns a list of all climate indices.
    """
    return _INDICES_.keys()

def indices_description():
    """
    returns a discription of all climate indices.
    """
    description = ''
    for indice in indices():
        description = description + "%s: %s\n" % (indice, _INDICES_[indice]['description'])
    return description

def indice_variable(indice):
    """
    returns variable (tasmax, tas, ...) which can be used for the climate indice.
    """
    variable = None
    try:
        variable = _INDICES_[indice]['variable']
    except:
        logger.error('unknown indice %s', indice)
    return variable

def group_by_experiment(nc_files):
    """
    groups nc_files by experiment name. Experiment examples:
    
    CORDEX: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
    CMIP5:

    We collect for each experiment all files on the time axis:
    200101-200512, 200601-201012, ...

    Time axis is sorted by time.
    """
    
    groups = {}
    for nc_file in nc_files:
        ds = Dataset(nc_file)
        rd = ocgis.RequestDataset(nc_file)
        
        # CORDEX example: EUR-11_ICHEC-EC-EARTH_historical_r3i1p1_DMI-HIRHAM5_v1_day
        cordex_pattern = "{variable}_{domain}_{driving_model}_{experiment}_{ensemble}_{model}_{version}_{frequency}"
        key = cordex_pattern.format(
            variable = rd.variable,
            domain = ds.CORDEX_domain,
            driving_model = ds.driving_model_id,
            experiment = ds.experiment_id,
            ensemble = ds.driving_model_ensemble_member,
            model = ds.model_id,
            version = ds.rcm_version_id,
            frequency = ds.frequency)

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

def calc_indice(resources, indice="SU", grouping="year", out_dir=None):
    """
    Calculates given indice for variable and grouping.

    param: resources: single filename or list of filenames (netcdf)
    param: out_dir: output directory for result file (netcdf)

    result: netcdf files with calculated indices
    """

    ## ocgis.env.OVERWRITE = True
    ## ocgis.env.DIR_DATA = os.path.curdir
    ## ocgis.env.DIR_OUTPUT = outdir    
    ## output_crs = None

    result = None
    calc_icclim = [{'func' : 'icclim_' + indice, 'name' : indice}]
    try:
        variable = indice_variable(indice)
        prefix = variable + '_' + indice
        rd = ocgis.RequestDataset(uri=sort_by_time(resources), variable=variable) # TODO: time_range=[dt1, dt2]
        result = ocgis.OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=calc_grouping(grouping),
            prefix=prefix,
            output_format='nc',
            dir_output=out_dir,
            add_auxiliary_files=False).execute()
    except:
        logger.exception('Could not calc indice %s with variable %s for files %s.', indice, variable, resources)

    return result

