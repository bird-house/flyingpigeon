
from flyingpigeon.calculation import fieldmean
from matplotlib import pyplot as plt
from flyingpigeon import utils


def fieldmean(resource):
    """
    calculating of a weighted field mean

    :param resource: str or list of str containing the netCDF files pathes

    :return list: timeseries of the averaged values per timepstep
    """
    from flyingpigeon.utils import get_values, get_coordinates, get_index_lat
    from numpy import radians, average, cos, sqrt, array

    data = get_values(resource)  # np.squeeze(ds.variables[variable][:])
    dim = data.shape

    if len(data.shape) == 3:
        # TODO if data.shape == 2 , 4 ...
        lats, lons = get_coordinates(resource, unrotate=False)
        lats = array(lats)
        if len(lats.shape) == 2:
            lats = lats[:, 0]
        else:
            print('Latitudes not reduced to 1D')
        # TODO: calculat weighed average with 2D lats (rotated pole coordinates)
        # lats, lons = get_coordinates(resource, unrotate=False)
        # if len(lats.shape) == 2:
        #     lats, lons = get_coordinates(resource)

        lat_index = get_index_lat(resource)
        # LOGGER.debug('lats dimension %s ' % len(lats.shape))
        # LOGGER.debug('lats index %s' % lat_index)

        lat_w = sqrt(cos(lats * radians(1)))
        meanLon = average(data, axis=lat_index, weights=lat_w)
        meanTimeserie = average(meanLon, axis=1)
        # LOGGER.debug('fieldmean calculated')
    else:
        print('not 3D shaped data. Average can not be calculated')
    return meanTimeserie



fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
# LOGGER.debug('Start visualisation spaghetti plot')

# === prepare invironment
if type(resouces) != list:
    resouces = [resouces]
variable = utils.get_variable(resouces[0])
title = "Field mean of %s " % variable

for c, nc in enumerate(resouces):
    # get timestapms
    dt = utils.get_time(nc)  # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
    ts = fieldmean(nc)
    plt.plot(ts)
    # fig.line( dt,ts )
