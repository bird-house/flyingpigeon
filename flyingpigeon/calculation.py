
import logging
LOGGER = logging.getLogger("PYWPS")


def fieldmean(resource):
    """
    calculating of a weighted field mean

    :param resource: str or list of str containing the netCDF files pathes

    :return list: averaged values
    """
    from flyingpigeon.utils import get_values, unrotate_pole  # get_coordinates
    from numpy import radians, average, cos, sqrt

    try:
        data = get_values(resource)  # np.squeeze(ds.variables[variable][:])
        if len(data.shape) == 3:
            # get the index of the latitude (var could be Latitude, lat, latx or whatever)
            # ds.variables[variable].dimensions.index('lat')
            # nla = ds.variables[variable].dimensions
            # lat_index = [i for i, j in enumerate(nla) if 'lat' in j or 'Lat' in j][0]
            # If lat_index is [] we got an exception...
            # lat_units = ds.variables[nla[lat_index]].units
            # lat_val = ds.variables[nla[lat_index]][:]
            # if 'Degree' in lat_units or 'degree' in lat_units:
            #     # lat_w = np.cos(lat_val * np.radians(1))
            #     # may be add selection of the weighting method in process: cos/sqrt(cos)/none
            #     lat_w = np.sqrt(np.cos(lat_val * np.radians(1)))
            #     meanData = np.average(data, axis=lat_index, weights=lat_w)
            #     title = title + ' weighted by square root of the cosine of the latitude'

            lons, lats = unrotate_pole(resource[0], write_to_file=False)
            if dim[0] == len(lats):
                lat_index = 0
            elif dim[1] == len(lats):
                lat_index = 1
            elif dim[2] == len(lats):
                lat_index = 2
            else:
                LOGGER.exception('length of latitude is not matching values dimensions')

            lat_w = sqrt(cos(lats * radians(1)))
            meanData = np.average(data, axis=lat_index, weights=lat_w)
            LOGGER.info('fieldmean calculated')
        else:
            # TODO if data.shape == 2 , 4 ...
            LOGGER.error('not 3D shaped data. Average can not be calculated')
    except:
        LOGGER.exception('failed to calculate weighted average')
    return meanData
