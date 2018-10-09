import logging
LOGGER = logging.getLogger("PYWPS")

from flyingpigeon.config import testdata_path

from os import path
from os import listdir

import numpy as np

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
    LOGGER.debug(data.shape)

    if len(data.shape) == 3:
        # TODO if data.shape == 2 , 4 ...
        lats, lons = get_coordinates(resource, unrotate=False)
        lats = array(lats)
        if len(lats.shape) == 2:
            lats = lats[:, 0]
        else:
            LOGGER.debug('Latitudes not reduced to 1D')
        # TODO: calculat weighed average with 2D lats (rotated pole coordinates)
        # lats, lons = get_coordinates(resource, unrotate=False)
        # if len(lats.shape) == 2:
        #     lats, lons = get_coordinates(resource)

        lat_index = get_index_lat(resource)
        LOGGER.debug('lats dimension %s ' % len(lats.shape))
        LOGGER.debug('lats index %s' % lat_index)

        lat_w = sqrt(cos(lats * radians(1)))
        meanLon = average(data, axis=lat_index, weights=lat_w)
        meanTimeserie = average(meanLon, axis=1)
        LOGGER.debug('fieldmean calculated')
    else:
        LOGGER.error('not 3D shaped data. Average can not be calculated')
    return meanTimeserie

def remove_mean_trend(fana, varname):
    """
    Removing the smooth trend from 3D netcdf file
    """
    from cdo import Cdo
    from netCDF4 import Dataset
    import uuid
    from scipy.interpolate import UnivariateSpline
    from os import system

    if type(fana) == list:
        fana = fana[0]

    backup_ana = 'orig_mod_'+path.basename(fana)

    cdo = Cdo()

    # create backup of input file
    # Again, an issue with cdo versioning.
    # TODO: Fix CDO versioning workaround...

    try:
        cdo_cp = getattr(cdo,'copy')
        cdo_cp(input=fana, output=backup_ana)
    except:
        if(path.isfile(backup_ana)==False):
            com='copy'
            comcdo = 'cdo -O %s %s %s' % (com, fana, backup_ana)
            system(comcdo)
        else:
            backup_ana = 'None'

    # create fmana - mean field
    fmana = '%s.nc' % uuid.uuid1()

    cdo_op = getattr(cdo,'fldmean')
    cdo_op(input=fana, output=fmana)

    mean_arc_dataset = Dataset(fmana)
    mean_arcvar = mean_arc_dataset.variables[varname][:]
    data=mean_arcvar[:,0,0]
    mean_arc_dataset.close()    
    x = np.linspace(0,len(data)-1,len(data))
    y = data

    # Very slow method. 
    # TODO: sub by fast one 
    # (there is one in R, but doesn't want to add R to analogs...)
    spl = UnivariateSpline(x,y)

    smf = (len(y)) * np.var(y)
    spl.set_smoothing_factor(smf)
    trend = np.zeros(len(y),dtype=np.float)
    trend[:] = spl(x)

#    orig_arc_dataset = Dataset(fana,'r+')
    orig_arc_dataset = Dataset(fana,'a')
    orig_arcvar = orig_arc_dataset.variables.pop(varname)
    orig_data = orig_arcvar[:]

    det = np.zeros(np.shape(orig_data),dtype=np.float)
    det = (orig_data.T - trend).T

    orig_arcvar[:] = det

    at = {k: orig_arcvar.getncattr(k) for k in orig_arcvar.ncattrs()}
    maxat = np.max(det)
    minat = np.min(det)
    act = np.zeros((2),dtype=np.float32)
    valid = np.zeros((2),dtype=np.float32)
    act[0] = minat
    act[1] = maxat
    valid[0] = minat - abs(0.2*minat)
    valid[1] = maxat + abs(0.2*maxat)
    act_attr = {}
    val_attr = {}

    act_attr['actual_range'] = act
    val_attr['valid_range'] = valid
    orig_arcvar.setncatts(act_attr)
    orig_arcvar.setncatts(val_attr)

    orig_arc_dataset.close()

    return backup_ana

