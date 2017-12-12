import statsmodels.api as sm
from numpy import tile, empty, linspace

from flyingpigeon import utils
from flyingpigeon.ocgis_module import call
from tempfile import mkstemp

import logging
LOGGER = logging.getLogger("PYWPS")


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


def _smooth(ts_latlon):
    y = tile(ts_latlon,3)
    ts = len(ts_latlon)
    x = linspace(1, ts*3, num=ts*3, endpoint=True)
    ys = sm.nonparametric.lowess(y, x, frac=0.2)[ts:ts*2, 1]
    return ys

def get_anomalies(nc_file, frac=0.2, reference=None, method='ocgis', sseas='serial'):
    """
    Anomalisation of data subsets for weather classification by subtracting a smoothed annual cycle

    :param nc_file: input netCDF file
    :param frac: Number between 0-1 for strength of smoothing
               (0 = close to the original data, 1 = flat line)
               default = 0.2
    :param reference: Period to calculate annual cycle

    :returns str: path to output netCDF file
    """
    try:
        if (method == 'cdo'):
            from cdo import Cdo
            from os import system
            variable = utils.get_variable(nc_file)

            cdo = Cdo()
            ip, nc_anual_cycle_tmp = mkstemp(dir='.', suffix='.nc')
            ip2, nc_anual_cycle = mkstemp(dir='.', suffix='.nc')
            # TODO: if reference is none, use utils.get_time for nc_file to set the ref range
            #       But will need to fix 360_day issue (use get_time_nc from analogs)
            com = 'seldate'
            comcdo = 'cdo %s,%s-%s-%s,%s-%s-%s %s %s' % (com, reference[0].year, reference[0].month, reference[0].day,
                                                         reference[1].year, reference[1].month, reference[1].day,
                                                         nc_file, nc_anual_cycle_tmp)
            LOGGER.debug('CDO: %s' % (comcdo))
            system(comcdo)

            nc_anual_cycle = cdo.ydaymean(input=nc_anual_cycle_tmp, output=nc_anual_cycle)
        else:
            variable = utils.get_variable(nc_file)
            calc = [{'func': 'mean', 'name': variable}]
            calc_grouping = calc_grouping = ['day', 'month']
            nc_anual_cycle = call(nc_file,
                                  calc=calc,
                                  calc_grouping=calc_grouping,
                                  time_range=reference)
        LOGGER.info('annual cycle calculated: %s' % (nc_anual_cycle))
    except Exception as e:
        msg = 'failed to calcualte annual cycle %s' % e
        LOGGER.error(msg)
        raise Exception(msg)

    try:
        # spline for smoothing
        #import statsmodels.api as sm
        #from numpy import tile, empty, linspace
        from netCDF4 import Dataset
        from cdo import Cdo
        cdo = Cdo()
        # variable = utils.get_variable(nc_file)
        ds = Dataset(nc_anual_cycle, mode='a')
        vals = ds.variables[variable]
        vals_sm = empty(vals.shape)
        ts = vals.shape[0]
        x = linspace(1, ts*3, num=ts*3, endpoint=True)

        if ('serial' not in sseas):
            # Multiprocessing =======================

            from multiprocessing import Pool
            pool = Pool()

            valex = [0.]
            valex = valex*vals.shape[1]*vals.shape[2]

            # TODO redo with reshape
            ind = 0
            for lat in range(vals.shape[1]):
                for lon in range(vals.shape[2]):
                    valex[ind] = vals[:, lat, lon]
                    ind += 1

            LOGGER.debug('Start smoothing with multiprocessing')
            # TODO fraction option frac=... is not used here
            tmp_sm = pool.map(_smooth, valex)
            pool.close()
            pool.join()

            # TODO redo with reshape
            ind=0
            for lat in range(vals.shape[1]):
                for lon in range(vals.shape[2]):
                    vals_sm[:, lat, lon] = tmp_sm[ind]
                    ind+=1
        else:
            # Serial ==================================
            vals_sm = empty(vals.shape)
            for lat in range(vals.shape[1]):
                for lon in range(vals.shape[2]):
                    try:
                        y = tile(vals[:, lat, lon], 3)
                        # ys = smooth(y, window_size=91, order=2, deriv=0, rate=1)[ts:ts*2]
                        ys = sm.nonparametric.lowess(y, x, frac=frac)[ts:ts*2, 1]
                        vals_sm[:, lat, lon] = ys
                    except:
                        msg = 'failed for lat %s lon %s' % (lat, lon)
                        LOGGER.exception(msg)
                        raise Exception(msg)
                LOGGER.debug('done for %s - %s ' % (lat, lon))

        vals[:, :, :] = vals_sm[:, :, :]
        ds.close()
        LOGGER.info('smothing of annual cycle done')
    except:
        msg = 'failed smothing of annual cycle'
        LOGGER.exception(msg)
        raise Exception(msg)
    try:
        ip, nc_anomal = mkstemp(dir='.', suffix='.nc')
        nc_anomal = cdo.sub(input=[nc_file, nc_anual_cycle], output=nc_anomal)
        LOGGER.info('cdo.sub; anomalisation done: %s ' % nc_anomal)
    except:
        msg = 'failed substraction of annual cycle'
        LOGGER.exception(msg)
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
        LOGGER.info('time_region: %s ' % time_region)
        nc_season = call(nc_file, time_region=time_region)
        LOGGER.info('season selection done %s ' % nc_season)
    except:
        msg = 'failed to select season, input file is passed '
        LOGGER.exception(msg)
        nc_season = nc_file
    return nc_season
