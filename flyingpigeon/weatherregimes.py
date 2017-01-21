from flyingpigeon import utils
from flyingpigeon.ocgis_module import call
from tempfile import mkstemp

import logging
logger = logging.getLogger(__name__)


_TIMEREGIONS_ = {'JJA': {'month': [6, 7, 8]},
                 'SON': {'month': [9, 10, 11]},
                 'OND': {'month': [10, 11, 12]},
                 'DJF': {'month': [12, 1, 2]},
                 'FMA': {'month': [2, 3, 4]},
                 'MAM': {'month': [3, 4, 5]},
                 'JJAS': {'month': [6, 7, 8, 9]},
                 'DJFM': {'month': [12, 1, 2, 3]},
                 'MAMJ': {'month': [3, 4, 5, 6]},
                 'SOND': {'month': [9, 10, 11, 12]},
                 'SONDJF': {'month': [9, 10, 11, 12, 1, 2]},
                 'MAMJJA': {'month': [3, 4, 5, 6, 7, 8]},
                 'all': None}


def get_anomalies(nc_file, frac=0.2, reference=None):
    """
    Anomalisation of data subsets for weather classification.
    Anomalisation is done by subtracting a smoothed annual cycle

    :param nc_file: input netCDF file
    :param frac: Number between 0-1 for strength of smoothing
               (0 = close to the original data, 1 = flat line)
               default = 0.2
    :param reference: Period to calculate annual cycle

    :returns str: path to output netCDF file
    """
    try:
        variable = utils.get_variable(nc_file)
        calc = [{'func': 'mean', 'name': variable}]
        calc_grouping = calc_grouping = ['day', 'month']
        nc_anual_cycle = call(nc_file, calc=calc, calc_grouping=calc_grouping, time_range=reference)
        logger.info('annual cycle calculated')
    except Exception as e:
        msg = 'failed to calcualte annual cycle %s' % e
        logger.error(msg)
        raise Exception(msg)

    # spline for smoothing
    import statsmodels.api as sm
    from numpy import tile, empty, linspace
    from netCDF4 import Dataset
    from cdo import Cdo
    cdo = Cdo()

    try:
        # variable = utils.get_variable(nc_file)
        ds = Dataset(nc_anual_cycle, mode='a')
        vals = ds.variables[variable]
        vals_sm = empty(vals.shape)
        ts = vals.shape[0]
        x = linspace(1, ts*3, num=ts*3, endpoint=True)
        for lat in range(vals.shape[1]):
            for lon in range(vals.shape[2]):
                try:
                    y = tile(vals[:, lat, lon], 3)
                    # ys = smooth(y, window_size=91, order=2, deriv=0, rate=1)[ts:ts*2]
                    ys = sm.nonparametric.lowess(y, x, frac=frac)[ts:ts*2, 1]
                    vals_sm[:, lat, lon] = ys
                except:
                    msg = 'failed for lat %s lon %s' % (lat, lon)
                    logger.exception(msg)
                    raise Exception(msg)
            print 'done for %s - %s ' % (lat, lon)
        vals[:, :, :] = vals_sm[:, :, :]
        ds.close()
        logger.info('smothing of annual cycle done')
    except:
        msg = 'failed smothing of annual cycle'
        logger.exception(msg)
        raise Exception(msg)
    try:
        ip, nc_anomal = mkstemp(dir='.', suffix='.nc')
        nc_anomal = cdo.sub(input=[nc_file, nc_anual_cycle], output=nc_anomal)
        logger.info('cdo.sub; anomalisation done: %s ' % nc_anomal)
    except:
        msg = 'failed substraction of annual cycle'
        logger.exception(msg)
        raise Exception(msg)
    return nc_anomal


def get_season(nc_file, season='DJF'):
    """
    extacting of selected months

    :param nc_file: input netCDF
    :param season: month to be extracted (default = 'DJF')

    :returns str: netCDF with time subset
    """
    try:
        time_region = _TIMEREGIONS_[season]
        logger.info('time_region: %s ' % time_region)
        nc_season = call(nc_file, time_region=time_region)
        logger.info('season selection done %s ' % nc_season)
    except:
        msg = 'failed to select season, input file is passed '
        logger.exception(msg)
        nc_season = nc_file
    return nc_season
