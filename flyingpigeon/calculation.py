from eggshell.nc.nc_utils import get_values, get_coordinates, get_index_lat, get_variable
# from eggshell.nc.ocg_utils import call

from os.path import basename, join
from datetime import datetime as dt
from shutil import copyfile
from netCDF4 import Dataset

import numpy as np
import logging
LOGGER = logging.getLogger("PYWPS")


def fieldmean(resource):
    """
    calculating of a weighted field mean

    :param resource: str or list of str containing the netCDF files paths

    :return list: timeseries of the averaged values per timestep
    """
    from numpy import radians, average, cos, sqrt, array

    data = get_values(resource)  # np.squeeze(ds.variables[variable][:])
    # dim = data.shape
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


def robustness_cc_signal(variable_mean, standard_deviation=None, variable=None):
    """
    Claculating the Climate Change signal based on the output of robustness_stats.

    :param variable_mean: list of two 2D spatial netCDF files
                          in the order of [refenence, projection]
    :param standard_deviation: according to variable_mean files 2D netCDF files of the standard deviation

    :return  netCDF files: cc_signal.nc, mean_std.nc
    """

    basename_ref = basename(variable_mean[0]).split('_')
    basename_proj = basename(variable_mean[1]).split('_')
    # ensstd_tg_mean_1981-01-01-2010-12-31.nc'

    if variable is None:
        variable = get_variable(variable_mean[0])

    ds = Dataset(variable_mean[0])
    vals_ref = np.squeeze(ds[variable][:])
    ds.close()

    ds = Dataset(variable_mean[1])
    vals_proj = np.squeeze(ds[variable][:])
    ds.close()

    if standard_deviation is not None:
        ds = Dataset(standard_deviation[0])
        std_ref = np.squeeze(ds[variable][:])
        ds.close()

        ds = Dataset(standard_deviation[1])
        std_proj = np.squeeze(ds[variable][:])
        ds.close()

        bn_mean_std = 'mean-std_{}_{}_{}'.format(basename_ref[1], basename_ref[-2], basename_proj[-1])
        out_mean_std = copyfile(standard_deviation[0], bn_mean_std)

        ds_median_std = Dataset(out_mean_std, mode='a')
        ds_median_std[variable][:] = (std_ref + std_proj) / 2
        ds_median_std.close()

    else:
        out_mean_std = None

    bn_cc_signal = 'cc-signal_{}_{}_{}'.format(basename_ref[1], basename_ref[-2], basename_proj[-1])

    out_cc_signal = copyfile(variable_mean[0], bn_cc_signal)

    ds_cc = Dataset(out_cc_signal, mode='a')
    ds_cc[variable][:] = np.squeeze(vals_proj - vals_ref)
    ds_cc.close()

    return out_cc_signal, out_mean_std


def robustness_stats(resources, time_range=[None, None], dir_output=None, variable=None):
    """
    calculating the spatial mean and corresponding standard deviation for an ensemble
    of consistent datasets containing one variableself.
    If a time range is given the statistical values are calculated only in the disired timeperiod.

    :param resources: str or list of str containing the netCDF files paths
    :param time_range: sequence of two datetime.datetime objects to mark start and end point
    :param dir_output: path to folder to store ouput files  (default= curdir)
    :param variable: variable name containing in netCDF file. If not set, variable name gets detected

    :return netCDF files: out_ensmean.nc, out_ensstd.nc
    """

    from ocgis import OcgOperations, RequestDataset, env
    env.OVERWRITE = True

    if variable is None:
        variable = get_variable(resources[0])

    out_means = []
    for resource in resources:

        rd = RequestDataset(resource, variable)
        prefix = basename(resource).replace('.nc', '')
        LOGGER.debug('processing mean of {}'.format(prefix))
        calc = [{'func': 'median', 'name': variable}]
        # {'func': 'median', 'name': 'monthly_median'}
        ops = OcgOperations(dataset=rd, calc=calc, calc_grouping=['all'],
                            output_format='nc', prefix='median_'+prefix, time_range=time_range, dir_output=dir_output)
        out_means.append(ops.execute())
    # nc_out = call(resource=resources, calc=[{'func': 'mean', 'name': 'ens_mean'}],
    #               calc_grouping='all', # time_region=time_region,
    #               dir_output=dir_output, output_format='nc')

    ####
    # read in numpy array

    for i, out_mean in enumerate(out_means):
        if i == 0:
            ds = Dataset(out_mean)
            var = ds[variable][:]
            dims = [len(out_means), var[:].shape[-2], var[:].shape[-1]]
            vals = np.empty(dims)
            vals[i, :, :] = np.squeeze(var[:])
            ds.close()
        else:
            ds = Dataset(out_mean)
            vals[i, :, :] = np.squeeze(ds[variable][:])
            ds.close()

    ####
    # calc median, std
    val_median = np.nanmedian(vals, axis=0)
    val_std = np.nanstd(vals, axis=0)

    #####
    # prepare files by copying ...
    ensmean_file = 'ensmean_{}_{}_{}.nc'.format(variable, dt.strftime(time_range[0], '%Y-%m-%d'),
                                                dt.strftime(time_range[1], '%Y-%m-%d'))
    out_ensmean = copyfile(out_means[0], join(dir_output, ensmean_file))

    ensstd_file = 'ensstd_{}_{}_{}.nc'.format(variable, dt.strftime(time_range[0], '%Y-%m-%d'),
                                              dt.strftime(time_range[1], '%Y-%m-%d'))
    out_ensstd = copyfile(out_means[0], join(dir_output, ensstd_file))

    ####
    # write values to files
    ds_median = Dataset(out_ensmean, mode='a')
    ds_median[variable][:] = val_median
    ds_median.close()

    ds_std = Dataset(out_ensstd, mode='a')
    ds_std[variable][:] = val_std
    ds_std.close()
    LOGGER.info('processing the overall ensemble statistical mean ')

    # prefix = 'ensmean_tg-mean_{}-{}'.format(dt.strftime(time_range[0], '%Y-%m-%d'),
    #                                         dt.strftime(time_range[1], '%Y-%m-%d'))
    # rd = RequestDataset(out_means, var)
    # calc = [{'func': 'mean', 'name': 'mean'}]  #  {'func': 'median', 'name': 'monthly_median'}
    # ops = OcgOperations(dataset=rd, calc=calc, calc_grouping=['all'],
    #                     output_format=output_format, prefix='mean_'+prefix, time_range=time_range)
    # ensmean = ops.execute()

    return out_ensmean, out_ensstd


# call(resource=[], variable=None, dimension_map=None, agg_selection=True,
#          calc=None, calc_grouping=None, conform_units_to=None, crs=None,
#          memory_limit=None, prefix=None,
#          regrid_destination=None, regrid_options='bil', level_range=None,  # cdover='python',
#          geom=None, output_format_options=None, search_radius_mult=2.,
#          select_nearest=False, select_ugid=None, spatial_wrapping=None,
#          t_calendar=None, time_region=None,
#          time_range=None, dir_output=None, output_format='nc'):


# CDO is disabled ...
# def remove_mean_trend(fana, varname):
#     """
#     Removing the smooth trend from 3D netcdf file
#     """
#     from cdo import Cdo
#     from netCDF4 import Dataset
#     import uuid
#     from scipy.interpolate import UnivariateSpline
#     from os import system
#
#     if type(fana) == list:
#         fana = fana[0]
#
#     backup_ana = 'orig_mod_' + path.basename(fana)
#
#     cdo = Cdo()
#
#     # create backup of input file
#     # Again, an issue with cdo versioning.
#     # TODO: Fix CDO versioning workaround...
#
#     try:
#         cdo_cp = getattr(cdo, 'copy')
#         cdo_cp(input=fana, output=backup_ana)
#     except Exception:
#         if(path.isfile(backup_ana) is False):
#             com = 'copy'
#             comcdo = 'cdo -O %s %s %s' % (com, fana, backup_ana)
#             system(comcdo)
#         else:
#             backup_ana = 'None'
#
#     # create fmana - mean field
#     fmana = '%s.nc' % uuid.uuid1()
#
#     cdo_op = getattr(cdo, 'fldmean')
#     cdo_op(input=fana, output=fmana)
#
#     mean_arc_dataset = Dataset(fmana)
#     mean_arcvar = mean_arc_dataset.variables[varname][:]
#     data = mean_arcvar[:, 0, 0]
#     mean_arc_dataset.close()
#     x = np.linspace(0, len(data) - 1, len(data))
#     y = data
#
#     # Very slow method.
#     # TODO: sub by fast one
#     # (there is one in R, but doesn't want to add R to analogs...)
#     spl = UnivariateSpline(x, y)
#
#     smf = (len(y)) * np.var(y)
#     spl.set_smoothing_factor(smf)
#     trend = np.zeros(len(y), dtype=np.float)
#     trend[:] = spl(x)
#
# #    orig_arc_dataset = Dataset(fana,'r+')
#     orig_arc_dataset = Dataset(fana, 'a')
#     orig_arcvar = orig_arc_dataset.variables.pop(varname)
#     orig_data = orig_arcvar[:]
#
#     det = np.zeros(np.shape(orig_data), dtype=np.float)
#     det = (orig_data.T - trend).T
#
#     orig_arcvar[:] = det
#
#     # at = {k: orig_arcvar.getncattr(k) for k in orig_arcvar.ncattrs()}
#     maxat = np.max(det)
#     minat = np.min(det)
#     act = np.zeros((2), dtype=np.float32)
#     valid = np.zeros((2), dtype=np.float32)
#     act[0] = minat
#     act[1] = maxat
#     valid[0] = minat - abs(0.2 * minat)
#     valid[1] = maxat + abs(0.2 * maxat)
#     act_attr = {}
#     val_attr = {}
#
#     act_attr['actual_range'] = act
#     val_attr['valid_range'] = valid
#     orig_arcvar.setncatts(act_attr)
#     orig_arcvar.setncatts(val_attr)
#     orig_arc_dataset.close()
#
#     return backup_ana
