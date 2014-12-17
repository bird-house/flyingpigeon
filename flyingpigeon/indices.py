import ocgis

from .exceptions import CalculationException

from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

_INDICES_ = dict(
    TG=dict(variable='tas', description='Mean of mean temperatur (tas as input files)'),
    TX=dict(variable='tasmax', description='Mean of max temperatur (tasmax as input files)'),
    TN=dict(variable='tasmin', description='Mean of daily min temperatur (tasmin as input files)'),
    TXn=dict(variable='tasmax', description='Min of daily min temperatur (tasmax as input files)'),
    TXx=dict(variable='tasmax', description='Max of daily max temperatur (tasmax as input files)'),
    TNn=dict(variable='tasmin', description='Min of daily min temperatur (tasmin as input files)'),
    TNx=dict(variable='tasmin', description='Max of daily min temperatur (tasmin as input files)'),
    SU=dict(variable='tasmax', description='Nr of summer days (tasmax as input files)'),
    CSU=dict(variable='tasmax', description='Nr of consecutive summer days (tasmax as input files)'),
    FD=dict(variable='tasmin', description='Nr of frost days (tasmin as input files)'),
    CFD=dict(variable='tasmin', description='Nr of consecutive frost days (tasmin as input files)'),
    TR=dict(variable='tasmin', description='... (tasmin as input files)'),
    ID=dict(variable='tasmax', description='Nr of Ice days (tasmax as input files)'),
    HD17=dict(variable='tas', description='Heating degree days [sum of 17 degrees - mean temperature] (tas as input files)'),
    GD4=dict(variable='tas', description='Growing degree days [sum of TG > 4 degrees] (tas as input files)'),
    RR=dict(variable='pr', description='Precipitation flux mean (mon / year) (pr as input files)'),
    RR1=dict(variable='pr', description='Nr of days with precipitation > 1 mm  (pr as input files)'),
    CWD=dict(variable='pr', description='Consecutive wet days (pr as input files)'),
    CDD=dict(variable='pr', description='Consecutive dry days (pr as input files)'),
    SDII=dict(variable='pr', description='Simple daily intensity index for wet days [mm/wet day] (pr as input files)'),
    R10mm=dict(variable='pr', description='Nr of days >10mm (pr as input files)'),
    R20mm=dict(variable='pr', description='Nr of days with precipitation > 20 mm (pr as input files)'),
    RX1day=dict(variable='pr', description='Highest 1-day precipitation amount (pr as input files)'),
    RX5day=dict(variable='pr', description='Highest 5-day precipitation amount (pr as input files)'),
    SD=dict(variable='prsn', description='Nr of snow days (prsn as input files)'),
    SD1=dict(variable='prsn', description='Nr of days with snow > 1cm  (prsn as input files)'),
    SD5cm=dict(variable='prsn', description='Nr of days with snow > 5cm (prsn as input files)'),
    SD50cm=dict(variable='prsn', description='Nr of days with snow > 50 cm (prsn as input files)'),
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

    from utils import aggregations, calc_grouping

    output = None
    calc_icclim = [{'func' : 'icclim_' + indice, 'name' : indice}]
    try:
        aggs = aggregations(resources)
        if len(aggs) > 1:
            logger.warning('more than one experiment group selected: %s', aggs.keys())
        if len(aggs) == 0:
            raise Exception('no valid input data found!')
        agg_name = aggs.keys()[0]
        logger.debug('aggregation = %s', agg_name)
        nc_files = aggs[agg_name]['files']
        variable = indice_variable(indice)
        prefix = '%s_%s' % (indice, agg_name)
        filename = prefix + '.nc'
        logger.debug('calculating %s', filename)
        rd = ocgis.RequestDataset(uri=nc_files, variable=variable)
        output = ocgis.OcgOperations(
            dataset=rd,
            calc=calc_icclim,
            calc_grouping=calc_grouping(grouping),
            prefix=prefix,
            output_format='nc',
            dir_output=out_dir,
            add_auxiliary_files=False).execute()
    except:
        msg = 'Could not calc indice %s' % indice
        logger.exception(msg)
        raise CalculationException(msg)

    return output

