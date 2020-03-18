"""
visualization of netCDF data
"""
from matplotlib import pyplot as plt
from matplotlib import colors
# from matplotlib.patches import Polygon
import matplotlib.patches as mpatches
import cartopy.feature as cfeature

import cartopy.crs as ccrs
from cartopy.util import add_cyclic_point

# from flyingpigeon.nc_statistic import fieldmean
from flyingpigeon.nc_utils import get_variable, get_coordinates
from flyingpigeon.nc_utils import get_time, sort_by_filename, get_values
from flyingpigeon.plt_utils import fig2plot

from numpy import meshgrid
from netCDF4 import Dataset
import numpy as np
import pandas as pd
from datetime import datetime as dt
from tempfile import mkstemp
# from matplotlib import use
# use('Agg')   # use this if no xserver is available

import logging
LOGGER = logging.getLogger("PYWPS")


class MidpointNormalize(colors.Normalize):
    def __init__(self, vmin=None, vmax=None, vcenter=None, clip=False):
        self.vcenter = vcenter
        colors.Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        # I'm ignoring masked values and all kinds of edge cases to make a
        # simple example...
        x, y = [self.vmin, self.vcenter, self.vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))


def add_colorbar(im, aspect=20, pad_fraction=0.5,):
    """Add a vertical color bar to an image plot."""
    from mpl_toolkits import axes_grid1

    divider = axes_grid1.make_axes_locatable(im.axes)
    width = axes_grid1.axes_size.AxesY(im.axes, aspect=1./aspect)
    pad = axes_grid1.axes_size.Fraction(pad_fraction, width)
    current_ax = plt.gca()
    cax = divider.append_axes("right", size=width, pad=pad)
    plt.sca(current_ax)

    return im.axes.figure.colorbar(im, cax=cax)


def plot_extend(resource, file_extension='png'):
    """
    plots the extend (domain) of the values stored in a netCDF file:

    :parm resource: path to netCDF file
    :param file_extension: file format of the graphic. if file_extension=None a matplotlib figure will be returned

    :return graphic: graphic in specified format
    """
    lats, lons = get_coordinates(resource, unrotate=True)

    # box_top = 45
    # x, y = [-20, -20, 45, 45, -44], [-45, box_top, box_top, -45, -45]

    xy = np.array([[np.min(lons), np.min(lats)],
                   [np.max(lons), np.min(lats)],
                   [np.max(lons), np.max(lats)],
                   [np.min(lons), np.max(lats)]])

    fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
    projection = ccrs.Robinson()

    #  ccrs.Orthographic(central_longitude=np.mean(xy[:, 0]),
    #  central_latitude=np.mean(xy[:, 1]),
    #  globe=None)  # Robinson()

    ax = plt.axes(projection=projection)
    ax.stock_img()
    ax.coastlines()
    ax.add_patch(mpatches.Polygon(xy, closed=True, transform=ccrs.PlateCarree(), color='coral', alpha=0.6))
    # ccrs.Geodetic()
    ax.gridlines()
    plt.show()

    if file_extension is None:
        map_graphic = fig
    else:
        map_graphic = fig2plot(fig=fig, file_extension=file_extension)
    plt.close()

    return map_graphic


def plot_ts_spaghetti(resource, variable=None, ylim=None, title=None,
                      file_extension='png', delta=0, dir_output='.',
                      figsize=(10, 10)):
    """
    creates a png file containing the appropriate spaghetti plot as a
    field mean of the values.

    :param resource: list of files containing the same variable
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title
    :param ylim: Y-axis limitations: tuple(min,max)
    :param figsize: figure size defult=(10,10)

    :retruns str: path to png file
    """

    try:
        fig = plt.figure(figsize=figsize, dpi=600, facecolor='w', edgecolor='k')
        LOGGER.debug('Start visualisation spaghetti plot')

        # === prepare invironment
        if type(resource) != list:
            resource = [resource]
        if variable is None:
            variable = get_variable(resource[0])

        LOGGER.info('plot values preparation done')
    except Exception as ex:
        msg = "plot values preparation failed {}".format(ex)
        LOGGER.exception(msg)
        raise Exception(msg)
    try:
        for c, nc in enumerate(resource):
            try:
                # dt = get_time(nc)
                # ts = fieldmean(nc)

                if 'historical' in nc:
                    col = 'grey'
                elif 'evaluation' in nc:
                    col = 'black'
                elif 'rcp26' in nc:
                    col = 'blue'
                elif 'rcp85' in nc:
                    col = 'red'
                else:
                    col = 'green'

                dt = get_time(nc)
                # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
                # ts = fieldmean(nc)

                ds = Dataset(nc)
                var = get_variable(nc)
                tg_val = np.squeeze(ds.variables[var][:])
                d2 = np.nanmean(tg_val, axis=1)
                ts = np.nanmean(d2, axis=1)

                plt.plot(dt, ts, col)
                plt.grid()
                plt.title(title)
                #
                # plt.plot(dt, ts)
                # fig.line( dt,ts )
            except Exception as e:
                msg = "spaghetti plot failed for {} : {}".format(nc, e)
                LOGGER.exception(msg)

        plt.title(title, fontsize=20)
        plt.ylim(ylim)
        plt.xticks(fontsize=16, rotation=45)
        plt.yticks(fontsize=16)
        plt.grid()

        output_png = fig2plot(fig=fig, file_extension=file_extension, dir_output=dir_output)

        plt.close()
        LOGGER.info('timeseries spaghetti plot done for %s with %s lines.' % (variable, c))
    except Exception as ex:
        msg = 'matplotlib spaghetti plot failed: {}'.format(ex)
        LOGGER.exception(msg)
    return output_png


def ts_data(datasets, delta=0):
    """
    Creates a pandas DataFrame out of the netcdt datasets

    :param datasets: a sort_by_filename dictionary of datasets
    :param delta: set a delta for the values e.g. -273.15 to convert Kelvin to Celsius

    :retruns DataFrame:  dates
    """
    # Create index out of existing timestemps
    for i, key in enumerate(datasets.keys()):
        for nc in datasets[key]:
            ds = Dataset(nc)
            ts = get_time(nc)
            if i == 0:
                dates = pd.DatetimeIndex(ts)
            else:
                dates = dates.union(ts)

    # create empty DataFrame according existing timestemps
    df = pd.DataFrame(columns=list(datasets.keys()), index=dates)
    for key in datasets.keys():
        try:
            for nc in datasets[key]:
                ds = Dataset(nc)
                var = get_variable(nc)
                ts = get_time(nc)
                tg_val = np.squeeze(ds.variables[var][:])
                d2 = np.nanmean(tg_val, axis=1)
                data = np.nanmean(d2, axis=1) + delta
                df[key].loc[ts] = data
                # data = fieldmean(dic[key])  # get_values(f)
                # ts = get_time(dic[key])
                # ds = pd.Series(data=data, index=ts, name=key)
                # # ds_yr = ds.resample('12M', ).mean()   # yearly mean loffset='6M'
                # df[key] = ds
            LOGGER.debug('read in pandas series timeseries for: {}'.format(key))
        except Exception:
            LOGGER.exception('failed to read data timeseries for %s ' % (key))
    return df


def plot_ts_uncertainty(resource, variable=None, ylim=None, title=None,
                        file_extension='png', delta=0, window=None, dir_output=None,
                        figsize=(10, 10)):
    """
    creates a png file containing the appropriate uncertainty plot.

    :param resource: list of files containing the same variable
    :param delta: set a delta for the values e.g. -273.15 to convert Kelvin to Celsius
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param ylim: Y-axis limitations: tuple(min,max)
    :param title: string to be used as title
    :param figsize: figure size defult=(10,10)
    :param window: windowsize of the rolling mean

    :returns str: path/to/file.png
    """
    LOGGER.debug('Start visualisation uncertainty plot')

    #
    # from flyingpigeon.utils import get_time, sort_by_filename
    # from flyingpigeon.nc_statistic import fieldmean
    # from flyingpigeon.metadata import get_frequency

    # === prepare invironment
    if type(resource) == str:
        resource = list([resource])
    if variable is None:
        variable = get_variable(resource[0])
    if title is None:
        title = "Field mean of %s " % variable

    LOGGER.info('variable %s found in resource.' % variable)

    try:
        fig = plt.figure(figsize=figsize, facecolor='w', edgecolor='k')

        dic = sort_by_filename(resource, historical_concatination=True)

        df = ts_data(dic, delta=delta)

        if window is None:
            window = 10  # TODO: include detection of frq = get_frequency(resource[0])

        if len(df.index.values) >= window * 2:
            # TODO: calculate windowsize according to timestapms (day,mon,yr ... with get_frequency)
            df_smooth = df.rolling(window=window, center=True).mean()
            LOGGER.info('rolling mean calculated for all input data')
        else:
            df_smooth = df.copy()
            LOGGER.debug('timeseries too short for moving mean')
            fig.text(0.95, 0.05, '!!! timeseries too short for moving mean over 30years !!!',
                     fontsize=20, color='red',
                     ha='right', va='bottom', alpha=0.5)

        try:
            rmean = np.squeeze(df_smooth.quantile([0.5], axis=1,).values)
            # skipna=False  quantile([0.5], axis=1, numeric_only=False )
            q05 = np.squeeze(df_smooth.quantile([0.10], axis=1,).values)  # numeric_only=False)
            q33 = np.squeeze(df_smooth.quantile([0.33], axis=1,).values)  # numeric_only=False)
            q66 = np.squeeze(df_smooth.quantile([0.66], axis=1,).values)  # numeric_only=False)
            q95 = np.squeeze(df_smooth.quantile([0.90], axis=1,).values)  # numeric_only=False)
            LOGGER.info('quantile calculated for all input data')
        except Exception as e:
            LOGGER.exception('failed to calculate quantiles: {}'.format(e))

        try:
            x = pd.to_datetime(df.index.values)
            x1 = x[x <= dt.strptime('2005-12-31', "%Y-%m-%d")]
            x2 = x[len(x1)-1:]  # -1 to catch up with the last historical value

            plt.fill_between(x, q05, q95, alpha=0.5, color='grey')
            plt.fill_between(x, q33, q66, alpha=0.5, color='grey')

            plt.plot(x1, rmean[:len(x1)], c='blue', lw=3)
            plt.plot(x2, rmean[len(x1)-1:], c='r', lw=3)
            # plt.xlim(min(df.index.values), max(df.index.values))
            plt.ylim(ylim)
            plt.xticks(fontsize=16, rotation=45)
            plt.yticks(fontsize=16,)
            plt.title(title, fontsize=20)
            plt.grid()  # .grid_line_alpha=0.3

            output_png = fig2plot(fig=fig, file_extension=file_extension, dir_output=dir_output)
            plt.close()
            LOGGER.debug('timeseries uncertainty plot done for %s' % variable)
        except Exception as e:
            raise Exception('failed to calculate quantiles. {}'.format(e))
    except Exception as e:
        LOGGER.exception('uncertainty plot failed for {}: {}'.format(variable, e))
        _, output_png = mkstemp(dir=dir_output, suffix='.png')
    return output_png


def plot_ts_uncertaintyrcp(resource, variable=None, ylim=None, title=None,
                           file_extension='png', delta=0, window=None, dir_output=None,
                           figsize=(10, 10)):
    """
    creates a png file containing the appropriate uncertainty plot.

    :param resource: list of files containing the same variable
    :param delta: set a delta for the values e.g. -273.15 to convert Kelvin to Celsius
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param ylim: Y-axis limitations: tuple(min,max)
    :param title: string to be used as title
    :param figsize: figure size defult=(10,10)
    :param window: windowsize of the rolling mean

    :returns str: path/to/file.png
    """
    LOGGER.debug('Start visualisation uncertainty plot')

    #
    # from flyingpigeon.utils import get_time, sort_by_filename
    # from flyingpigeon.nc_statistic import fieldmean
    # from flyingpigeon.metadata import get_frequency

    # === prepare invironment
    if type(resource) == str:
        resource = list([resource])
    if variable is None:
        variable = get_variable(resource[0])
    if title is None:
        title = "Field mean of %s " % variable

    LOGGER.info('variable %s found in resource.' % variable)

    try:
        fig = plt.figure(figsize=figsize, facecolor='w', edgecolor='k')

        dic = sort_by_filename(resource, historical_concatination=True)

        df = ts_data(dic, delta=delta)

        if window is None:
            # if frq == 'day':
            #     window = 1095  # 1
            # elif frq == 'man':
            #     window = 35  # 9
            # elif frq == 'sem':
            #     window = 11  # 9
            # elif frq == 'yr':
            #     window = 3  # 0
            # else:
            #     LOGGER.debug('frequency %s is not included' % frq)
            window = 10
            # TODO: include detection of frq = get_frequency(resource[0])

        if len(df.index.values) >= window * 2:
            # TODO: calculate windowsize according to timestapms (day,mon,yr ... with get_frequency)
            df_smooth = df.rolling(window=window, center=True).mean()
            LOGGER.info('rolling mean calculated for all input data')
        else:
            df_smooth = df.copy()
            LOGGER.debug('timeseries too short for moving mean')
            fig.text(0.95, 0.05, '!!! timeseries too short for moving mean over 30years !!!',
                     fontsize=20, color='red',
                     ha='right', va='bottom', alpha=0.5)

        # split into differnet RCPs:
        # TODO: inlcude rcp45 and 65
        rcp26 = [ds for ds in df_smooth.columns if 'rcp26' in ds]
        rcp85 = [ds for ds in df_smooth.columns if 'rcp85' in ds]

        df_rcp26 = df_smooth[rcp26]
        df_rcp85 = df_smooth[rcp85]

        # for rcp26:
        try:
            rcp26_rmean = np.squeeze(df_rcp26.quantile([0.5], axis=1,).values)
            # skipna=False  quantile([0.5], axis=1, numeric_only=False )
            rcp26_q05 = np.squeeze(df_rcp26.quantile([0.10], axis=1,).values)
            rcp26_q33 = np.squeeze(df_rcp26.quantile([0.33], axis=1,).values)
            rcp26_q66 = np.squeeze(df_rcp26.quantile([0.66], axis=1,).values)
            rcp26_q95 = np.squeeze(df_rcp26.quantile([0.90], axis=1,).values)
            LOGGER.info('quantile calculated for all input data')
        except Exception as e:
            LOGGER.exception('failed to calculate quantiles: {}'.format(e))

        try:
            rcp85_rmean = np.squeeze(df_rcp85.quantile([0.5], axis=1,).values)
            # skipna=False  quantile([0.5], axis=1, numeric_only=False )
            rcp85_q05 = np.squeeze(df_rcp85.quantile([0.10], axis=1,).values)
            rcp85_q33 = np.squeeze(df_rcp85.quantile([0.33], axis=1,).values)
            rcp85_q66 = np.squeeze(df_rcp85.quantile([0.66], axis=1,).values)
            rcp85_q95 = np.squeeze(df_rcp85.quantile([0.90], axis=1,).values)
            LOGGER.info('quantile calculated for all input data')
        except Exception as e:
            LOGGER.exception('failed to calculate quantiles: {}'.format(e))

        # plot for rcp26:
        try:
            x = pd.to_datetime(df.index.values)
            x1 = x[x <= dt.strptime('2005-12-31', "%Y-%m-%d")]
            x2 = x[len(x1)-1:]  # -1 to catch up with the last historical value

            plt.fill_between(x, rcp26_q05, rcp26_q95, alpha=0.5, color='grey')
            plt.fill_between(x, rcp26_q33, rcp26_q66, alpha=0.5, color='grey')

            plt.fill_between(x2, rcp85_q05[len(x1)-1:], rcp85_q95[len(x1)-1:],
                             alpha=0.5, color='grey')
            plt.fill_between(x2, rcp85_q33[len(x1)-1:], rcp85_q66[len(x1)-1:],
                             alpha=0.5, color='grey')

            plt.plot(x1, rcp26_rmean[:len(x1)], c='blue', lw=3)
            plt.plot(x2, rcp26_rmean[len(x1)-1:], c='green', lw=3)

            plt.plot(x1, rcp85_rmean[:len(x1)], c='blue', lw=3)
            plt.plot(x2, rcp85_rmean[len(x1)-1:], c='red', lw=3)

            # plt.xlim(min(df.index.values), max(df.index.values))

            plt.ylim(ylim)
            plt.xticks(fontsize=16, rotation=45)
            plt.yticks(fontsize=16,)
            plt.title(title, fontsize=20)
            plt.grid()  # .grid_line_alpha=0.3

            output_png = fig2plot(fig=fig, file_extension=file_extension, dir_output=dir_output)
            plt.close()
            LOGGER.debug('timeseries uncertainty plot done for %s' % variable)
        except Exception as e:
            raise Exception('failed to calculate quantiles. {}'.format(e))
    except Exception as e:
        LOGGER.exception('uncertainty plot failed for {}: {}'.format(variable, e))
        _, output_png = mkstemp(dir=dir_output, suffix='.png')
    return output_png


def plot_map_timemean(resource, variable=None, time_range=None,
                      title=None, delta=0, cmap=None, vmin=None, vmax=None, figsize=(15, 15),
                      file_extension='png', dir_output='.'):
    """
    creates a spatial map with the mean over the timestepps.
    If multiple files are provided, a mean over all files are condidered.

    :param resource: netCDF file(s) containng spatial values to be plotted.
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title
    :param delta: set a delta for the values e.g. -273.15 to convert Kelvin to Celsius
    :param figsize: figure size defult=(15,15)
    :param vmin: colorbar minimum
    :param vmax: colorbar maximum
    :param file_extension: file extinction for the graphic
    :param dir_output: output directory to store the output graphic

    :returns str: path/to/file.png
    """
    # from cartopy.mpl.gridliner import LONGITUDE_FORMATTER, LATITUDE_FORMATTER

    try:
        LOGGER.debug('plot_map function read in values for {}'.format(resource))

        # get values of netcdf file
        if type(resource) == str:

            ds = Dataset(resource)

            if variable is None:
                variable = get_variable(resource)

            var = ds.variables[variable]
            dims = var.dimensions

            lat = ds.variables[dims[-2]]
            lon = ds.variables[dims[-1]]
            lons, lats = meshgrid(lon, lat)

            var = get_values(resource, time_range=time_range, variable=variable).data
            var_mean = np.nanmean(var, axis=0) + delta
            # mean over whole periode 30 Years 1981-2010 and transform to Celsius
        else:
            for i, f in enumerate(resource):
                if i == 0:
                    if variable is None:
                        variable = get_variable(f)

                    ds = Dataset(f)
                    var = ds.variables[variable]
                    dims = var.dimensions

                    lat = ds.variables[dims[-2]]
                    lon = ds.variables[dims[-1]]
                    lons, lats = meshgrid(lon, lat)
                    vals = get_values(f, time_range=time_range, variable=variable).data
                else:
                    vals = np.append(vals, get_values(f, time_range=time_range, variable=variable).data, axis=0)
            var_mean = np.nanmean(vals, axis=0) + delta

        # prepare plot
        LOGGER.info('preparing matplotlib figure')

        fig = plt.figure(figsize=figsize, facecolor='w', edgecolor='k')
        ax = plt.axes(projection=ccrs.PlateCarree())

        cs = plt.pcolormesh(lons, lats, var_mean,
                            transform=ccrs.PlateCarree(), cmap=cmap,
                            vmin=vmin, vmax=vmax,
                            )
        # extent=(-0,17,10.5,24)
        # ax.set_extent(extent)

        ax.add_feature(cfeature.BORDERS, linewidth=2, linestyle='--')
        ax.add_feature(cfeature.COASTLINE, linewidth=2,)
        # ax.add_feature(cfeature.RIVERS)
        # ax.stock_img()
        # ax.gridlines(draw_labels=False)
        gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                          linewidth=2, color='gray', alpha=0.5, linestyle='--')

        gl.xlabels_top = False
        gl.ylables_right = False
        gl.ylables_left = False
        # gl.xlines = False
        # gl.xlocator = mticker.FixedLocator([0, 2,4,6,8,10,12,14,16] )
        # gl.xformatter = LONGITUDE_FORMATTER
        # gl.yformatter = LATITUDE_FORMATTER
        gl.xlabel_style = {'size': 15, 'color': 'black', 'weight': 'bold'}
        gl.ylabel_style = {'size': 15, 'color': 'black', 'weight': 'bold'}

        if cmap is None:
            if variable in ['pr', 'prAdjust',
                            'prcptot', 'rx1day', 'rx1day', 'wetdays',
                            'cdd', 'cwd', 'sdii']:
                cmap = 'Blues'
            if variable in ['tas', 'tasAdjust', 'tg', 'tg_mean']:
                cmap = 'seismic'

        plt.title(title, fontsize=25)

        cax = fig.add_axes([ax.get_position().x1 + 0.1, ax.get_position().y0,
                            0.02, ax.get_position().height])
        cbar = plt.colorbar(cs, cax=cax)
        cbar.ax.tick_params(labelsize=20)

        # ticklabs = cbar.ax.get_yticklabels()
        # cbar.ax.set_yticklabels(ticklabs, fontsize=15)
        # cb = add_colorbar(cs)

        LOGGER.info('Matplotlib pcolormesh plot done')

        output_png = fig2plot(fig=fig, file_extension='png',
                              dir_output=dir_output)
        plt.close()
        LOGGER.debug('Plot done for %s' % variable)
    except Exception as e:
        raise Exception('failed to plot netCDF file: {}'.format(e))

    return output_png


def plot_map_ccsignal(signal, robustness=None,
                      variable=None, cmap=None, title=None,
                      file_extension='png', vmin=None, vmax=None):  # 'seismic'
    """
    generates a graphic for the output of the ensembleRobustness process for a lat/long file.

    :param signal: netCDF file containing the signal difference over time
    :param robustness: netCDF file containing 1 and 0 corresponding to signal robustness
    :param variable: variable containing the netCDF files
    :param cmap: default='seismic',
    :param title: default='Model agreement of signal'
    :returns str: path/to/file.png
    """
    if variable is None:
        variable = get_variable(signal)

    print('found variable in file {}'.format(variable))

    try:
        ds = Dataset(signal)
        var_signal = ds.variables[variable]
        val_signal = np.squeeze(ds.variables[variable])

        lon_att = var_signal.dimensions[-1]
        lat_att = var_signal.dimensions[-2]

        lon = ds.variables[lon_att][:]
        lat = ds.variables[lat_att][:]

        lons, lats = meshgrid(lon, lat)
        ds.close()

        if robustness is not None:
            ds = Dataset(robustness)
            var_rob = get_variable(robustness)
            val_rob = np.squeeze(ds.variables[var_rob][:])
            ds.close()

            #  mask = val_signal[:]  #  [val_signal[:]<val_std[:]]

            # mask_h = np.empty(list(val_signal[:].shape))    # [[val_signal[:] > val_std[:]]] = 1
            # mask_h[(val_signal >= (val_std / 4.))] = 1   #[:]
            #
            # mask_l = np.empty(list(val_signal[:].shape))    # [[val_signal[:] > val_std[:]]] = 1
            # mask_l[mask_h != 1] = 1

            # cyclic_var, cyclic_lons = add_cyclic_point(var_signal, coord=lons)
            # mask, cyclic_lons = add_cyclic_point(mask, coord=lons)
            #
            # lons = cyclic_lons
            # var_signal = cyclic_var

        LOGGER.info('prepared data for plotting')
    except Exception as e:
        msg = 'failed to get data for plotting: {}'.format(e)
        LOGGER.exception(msg)
        raise Exception(msg)

    try:
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')
        ax = plt.axes(projection=ccrs.PlateCarree())
        # ax = plt.axes(projection=ccrs.Robinson(central_longitude=int(mean(lons))))

        # minval = round(np.nanmin(var_signal))
        if cmap is None:
            if variable in ['pr', 'prAdjust',
                            'prcptot', 'rx1day',
                            'wetdays',  # 'cdd',
                            'cwd', 'sdii',
                            'rx5day']:
                cmap = 'BrBG'
            if variable in ['tas', 'tasAdjust', 'tg', 'tg_mean']:
                cmap = 'seismic'
            else:
                cmap = 'viridis'
                LOGGER.debug('variable {} not found to set the colormap'.format(variable))

        maxval = round(np.nanmax(val_signal)+.5)
        minval = round(np.nanmin(val_signal))

        norm = MidpointNormalize(vmin=minval, vcenter=0, vmax=maxval)
        # )vcenter=0,,

        cs = plt.pcolormesh(lons, lats, val_signal,
                            transform=ccrs.PlateCarree(),
                            cmap=cmap, norm=norm, vmin=vmin, vmax=vmax)
        plt.colorbar(cs)

        if robustness is not None:
            plt.contourf(lons, lats, val_rob, transform=ccrs.PlateCarree(),
                         hatches=[None, '/', '.'], alpha=0, colors='none', cmap=None)    # colors='white'
            # cl = plt.contourf(lons, lats, mask_l, 1,
            # transform=ccrs.PlateCarree(), hatches=[None, '/'], alpha=0, colors='none', cmap=None)     # ,

            plt.annotate('// = low model ensemble agreement', (0, 0), (0, -10),
                         xycoords='axes fraction', textcoords='offset points', va='top')
            plt.annotate('..  = high model ensemble agreement', (0, 0), (0, -20),
                         xycoords='axes fraction', textcoords='offset points', va='top')

        ax.add_feature(cfeature.BORDERS, linewidth=2, linestyle='--')
        ax.add_feature(cfeature.COASTLINE, linewidth=2,)  # coastlines()
        gl = ax.gridlines(crs=ccrs.PlateCarree(), draw_labels=True,
                          linewidth=2, color='gray', alpha=0.5, linestyle='--')
        gl.xlabels_top = False
        gl.ylables_right = False
        gl.xlabel_style = {'size': 15, 'color': 'black'}
        gl.ylabel_style = {'size': 15, 'color': 'black'}

        if title is not None:
            plt.title(title, fontsize=20)

        plt.xticks(fontsize=16, rotation=45)
        plt.yticks(fontsize=16)

        graphic = fig2plot(fig=fig, file_extension=file_extension)
        plt.close()

        LOGGER.info('Plot created and figure saved')
    except Exception as e:
        msg = 'failed to plot graphic: {}'.format(e)
        LOGGER.exception(msg)
    return graphic


def plot_map_spatialanalog(ncfile, variable='dissimilarity',
                           cmap='viridis', title='Spatial analog'):
    """
    Return a matplotlib Figure instance showing a map of the dissimilarity measure.
    """
    import netCDF4 as nc
    from flyingpigeon import nc_utils
    from mpl_toolkits.axes_grid import make_axes_locatable
    import matplotlib.axes as maxes

    try:
        var = nc_utils.get_values(ncfile, variable)
        LOGGER.info('Data loaded')

        lats, lons = nc_utils.get_coordinates(ncfile, variable=variable, unrotate=False)

        if len(lats.shape) == 1:
            cyclic_var, cyclic_lons = add_cyclic_point(var, coord=lons)

            lons = cyclic_lons.data
            var = cyclic_var

        with nc.Dataset(ncfile) as D:
            V = D.variables[variable]
            lon, lat = map(float, V.target_location.split(','))

        LOGGER.info('Lat and lon loaded')

    except Exception as e:
        msg = 'Failed to get data for plotting: {0}\n{1}'.format(ncfile, e)
        LOGGER.exception(msg)
        raise Exception(msg)

    try:
        fig = plt.figure(facecolor='w', edgecolor='k')
        fig.subplots_adjust(top=.95, bottom=.05, left=.03, right=.95)

        ax = plt.axes(
            projection=ccrs.Robinson(central_longitude=int(np.mean(lons))))

        divider = make_axes_locatable(ax)
        cax = divider.new_horizontal("4%", pad=0.15, axes_class=maxes.Axes)
        fig.add_axes(cax)

        ax.plot(lon, lat, marker='o', mfc='#292421', ms=13, transform=ccrs.PlateCarree())
        ax.plot(lon, lat, marker='o', mfc='#ffffff', ms=7, transform=ccrs.PlateCarree())

        cs = ax.contourf(lons, lats, var, 60,
                         transform=ccrs.PlateCarree(),
                         cmap=cmap, interpolation='nearest')

        ax.coastlines(color='k', linewidth=.8)
        ax.set_title(title)

        cb = plt.colorbar(cs, cax=cax, orientation='vertical')
        cb.set_label(u"–            Dissimilarity             +")  # ha='left', va='center')
        cb.set_ticks([])

    except Exception as ex:
        msg = 'failed to plot graphic {}'.format(ex)
        LOGGER.exception(msg)

    LOGGER.info('Plot created and figure saved')
    return fig
