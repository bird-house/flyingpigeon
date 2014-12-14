import ocgis

from malleefowl import wpslogging as logging
#import logging
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

    from utils import aggregations

    output = None
    calc_icclim = [{'func' : 'icclim_' + indice, 'name' : indice}]
    filename = None
    try:
        aggs = aggregations(resources)
        if len(aggs) > 1:
            logger.warning('more than one expermint group selected: %s', aggs.keys())
        if len(aggs) == 0:
            raise Exception('no valid input data found!')
        agg_name = aggs.keys()[0]
        logger.debug('aggregation = %s', agg_name)
        nc_files = aggs[agg_name]['files']
        variable = indice_variable(indice)
        prefix = '%s_%s' % (indice, agg_name)
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

