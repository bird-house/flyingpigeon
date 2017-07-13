from flyingpigeon import utils
from flyingpigeon.ocgis_module import call


def get_anomalies(nc_file, frac=0.2, reference=None):
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
        variable = utils.get_variable(nc_file)
        calc = [{'func': 'mean'}]  # ,'name': variable
        calc_grouping = ['day', 'month']
        nc_anual_cycle = call(nc_file,
                              calc=calc,
                              calc_grouping=calc_grouping,
                              time_range=reference)
        print('annual cycle calculated')
    except Exception as e:
        msg = 'failed to calcualte annual cycle %s' % e
        print(msg)
        # raise Exception(msg)
    #
    # try:
    #     # spline for smoothing
    #     import statsmodels.api as sm
    #     from numpy import tile, empty, linspace
    #     from netCDF4 import Dataset
    #     from cdo import Cdo
    #     cdo = Cdo()
    #     # variable = utils.get_variable(nc_file)
    #     ds = Dataset(nc_anual_cycle, mode='a')
    #     vals = ds.variables[variable]
    #     vals_sm = empty(vals.shape)
    #     ts = vals.shape[0]
    #     x = linspace(1, ts*3, num=ts*3, endpoint=True)
    #     for lat in range(vals.shape[1]):
    #         for lon in range(vals.shape[2]):
    #             try:
    #                 y = tile(vals[:, lat, lon], 3)
    #                 # ys = smooth(y, window_size=91, order=2, deriv=0, rate=1)[ts:ts*2]
    #                 ys = sm.nonparametric.lowess(y, x, frac=frac)[ts:ts*2, 1]
    #                 vals_sm[:, lat, lon] = ys
    #             except:
    #                 msg = 'failed for lat %s lon %s' % (lat, lon)
    #                 print(msg)
    #                 raise Exception(msg)
    #         print('done for %s - %s ' % (lat, lon))
    #     vals[:, :, :] = vals_sm[:, :, :]
    #     ds.close()
    #     print('smothing of annual cycle done')
    # except:
    #     msg = 'failed smothing of annual cycle'
    #     print(msg)
    #     raise Exception(msg)
    # try:
    #     ip, nc_anomal = mkstemp(dir='.', suffix='.nc')
    #     nc_anomal = cdo.sub(input=[nc_file, nc_anual_cycle], output=nc_anomal)
    #     print('cdo.sub; anomalisation done: %s ' % nc_anomal)
    # except:
    #     msg = 'failed substraction of annual cycle'
    #     print(msg)
    #     raise Exception(msg)
    return nc_anual_cycle


nc_file = '/home/nils/data/409f9cdc-67b0-11e7-9260-9cb6d0d3acd7.nc'


nc_an = get_anomalies(nc_file)


print nc_an
