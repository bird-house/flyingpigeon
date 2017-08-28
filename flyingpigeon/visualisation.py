import os
from os.path import join
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date
import numpy as np
import logging
import matplotlib
matplotlib.use('Agg')   # use this if no xserver is available

from matplotlib import pyplot as plt
from matplotlib.colors import Normalize
from cartopy import config as cartopy_config
from cartopy.util import add_cyclic_point
import cartopy.crs as ccrs
from flyingpigeon import utils

logger = logging.getLogger(__name__)


class MidpointNormalize(Normalize):
    def __init__(self, vmin=None, vmax=None, midpoint=None, clip=False):
        self.midpoint = midpoint
        Normalize.__init__(self, vmin, vmax, clip)

    def __call__(self, value, clip=None):
        x, y = [self.vmin, self.midpoint, self.vmax], [0, 0.5, 1]
        return np.ma.masked_array(np.interp(value, x, y))


def plot_polygons(regions):
    """
    extract the polygon coordinate and plot it on a worldmap

    :param regions: list of ISO abreviations for polygons

    :return png: map_graphic.png
    """

    from cartopy.io.shapereader import Reader
    from cartopy.feature import ShapelyFeature

    from flyingpigeon import config
    DIR_SHP = config.shapefiles_path()

    if type(regions) == str:
        regions = list([regions])

    fname = join(DIR_SHP, "countries.shp")
    geos = Reader(fname).geometries()
    records = Reader(fname).records()

    fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
    projection = ccrs.Orthographic(central_longitude=0.0, central_latitude=0.0, globe=None)  # Robinson()
    ax = plt.axes(projection=projection)

    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            shape_feature = ShapelyFeature(geo, ccrs.PlateCarree(), edgecolor='black')
            ax.add_feature(shape_feature)
        ax.coastlines()
        # ax.set_global()

    o1, map_graphic = mkstemp(dir='.', suffix='.png')

    fig.savefig(map_graphic)
    plt.close()

    return map_graphic


def factsheetbrewer(png_country=None, png_spaghetti=None, png_uncertainty=None):
    """
    Put graphics into the climate fact sheet template to generate the final climate fact sheet

    :param png_country: World map graphic with countries polygons.
    :param png_uncertainty: Graphic showing a timeseries with fieldmean values and corresponding uncertainty

    :return pdf foumular: pdf with fillable text boxes for interpretation text
    """
    from PyPDF2 import PdfFileWriter, PdfFileReader
    from reportlab.pdfgen import canvas
    from flyingpigeon.config import static_dir
    try:
        try:
            _, pdf_country = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_country)
            c.drawImage(png_country, 355, 490, width=270, height=150)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_country = PdfFileReader(open(pdf_country, 'rb'))
        except:
            logger.exception('failed to convert png to pdf')

        try:
            _, pdf_uncertainty = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_uncertainty)
            c.drawImage(png_uncertainty, 20, 320, width=300, height=150)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_uncertainty = PdfFileReader(open(pdf_uncertainty, 'rb'))
        except:
            logger.exception('failed to convert png to pdf')

        try:
            _, pdf_spaghetti = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_spaghetti)
            c.drawImage(png_spaghetti, 280, 320, width=300, height=150)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_spagetthi = PdfFileReader(open(pdf_spaghetti, 'rb'))
        except:
            logger.exception('failed to convert png to pdf')

        output_file = PdfFileWriter()
        pfr_template = PdfFileReader(file(static_dir() + '/pdf/climatefactsheettemplate.pdf', 'rb'))
        logger.debug('template: %s' % pfr_template)

        page_count = pfr_template.getNumPages()
        for page_number in range(page_count):
            logger.debug("Plotting png to {} of {}".format(page_number, page_count))
            input_page = pfr_template.getPage(page_number)
            try:
                input_page.mergePage(pfr_country.getPage(0))
            except:
                logger.warn('failed to merge courtry map')
            try:
                input_page.mergePage(pfr_uncertainty.getPage(0))
            except:
                logger.warn('failed to merge uncertainy plot')
            try:
                input_page.mergePage(pfr_spagetthi.getPage(0))
            except:
                logger.warn('failed to merge spaghetti plot')
            try:
                output_file.addPage(input_page)
            except:
                logger.warn('failed to add page to output pdf')
        try:
            _, climatefactsheet = mkstemp(dir='.', suffix='.pdf')
            with open(climatefactsheet, 'wb') as outputStream:
                output_file.write(outputStream)
            logger.info('sucessfully brewed the demanded factsheet')
        except:
            logger.exception('failed write filled template to pdf. empty template will be set as output')
            climatefactsheet = static_dir() + '/pdf/climatefactsheettemplate.pdf'
    except:
        logger.exception("failed to brew the factsheet, empty template will be set as output")
    return climatefactsheet


def spaghetti(resouces, variable=None, title=None):
    """
    creates a png file containing the appropriate spaghetti plot as a field mean of the values.

    :param resouces: list of files containing the same variable
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title

    :retruns str: path to png file
    """
    from flyingpigeon.calculation import fieldmean

    try:
        fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
        logger.debug('Start visualisation spaghetti plot')

        # === prepare invironment
        if type(resouces) != list:
            resouces = [resouces]
        if variable is None:
            variable = utils.get_variable(resouces[0])
        if title is None:
            title = "Field mean of %s " % variable

        logger.info('plot values preparation done')
    except:
        msg = "plot values preparation failed"
        logger.exception(msg)
        raise Exception(msg)
    try:
        o1, output_png = mkstemp(dir='.', suffix='.png')
        for c, nc in enumerate(resouces):
            # get timestapms
            try:
                d = utils.get_time(nc)  # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
                dt = [datetime.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in d]
                ts = fieldmean(nc)
                plt.plot(dt, ts)
                # fig.line( dt,ts )
            except:
                msg = "spaghetti plot failed for"
                logger.exception(msg)
                raise Exception(msg)

        plt.title(title, fontsize=20)
        plt.grid()
        fig.savefig(output_png)
        plt.close()
        logger.info('timeseries spaghetti plot done for %s with %s lines.' % (variable, c))
    except:
        msg = 'matplotlib spaghetti plot failed'
        logger.exception(msg)
    return output_png

def uncertainty(resouces, variable=None, ylim=None, title=None, dir_out=None):
    """
    creates a png file containing the appropriate uncertainty plot.
    :param resouces: list of files containing the same variable
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title
    :returns str: path/to/file.png
    """
    LOGGER.debug('Start visualisation uncertainty plot')

    import pandas as pd
    import numpy as np
    from os.path import basename
    from flyingpigeon.utils import get_values, get_time
    # === prepare invironment
    if type(resouces) == str:
        resouces = list([resouces])
    if variable is None:
        variable = utils.get_variable(resouces[0])
    if title is None:
        title = "Field mean of %s " % variable
    if dir_out is None:
        dir_out = '.'

    try:
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')  # dpi=600,
        o1, output_png = mkstemp(dir=dir_out, suffix='.png')
        variable = utils.get_variable(resouces[0])
        df = pd.DataFrame()

        LOGGER.info('variable %s found in resources.' % variable)
        for f in resouces:
            try:
                data = get_values(f)
                LOGGER.debug("shape of data = %s " % len(data.shape))
                if len(data.shape) == 3:
                    meanData = np.mean(data, axis=1)
                    ts = np.mean(meanData, axis=1)
                else:
                    ts = data[:]
                jd = get_time(f)
                hs = pd.Series(ts, index=jd, name=basename(f))
                hd = hs.to_frame()
                df[basename(f)] = hs
            except:
                LOGGER.exception('failed to calculate timeseries for %s ' % (f))

        if len(df.index.values) >= 90:
            # TODO: calculate windowsize according to timestapms (day,mon,yr ... with get_frequency)
            df_smooth = df.rolling(window=30, center=True).mean()
            LOGGER.info('rolling mean calculated for all input data')
        else:
            df_smooth = df
            LOGGER.debug('timeseries too short, no rolling mean calculated')
            fig.text(0.95, 0.05, '!!! Plot not valide: timeseries too short !!!',
                     fontsize=20, color='red',
                     ha='right', va='bottom', alpha=0.5)
        try:
            rmean = df_smooth.quantile([0.5], axis=1,)  # df_smooth.median(axis=1)
            # skipna=False  quantile([0.5], axis=1, numeric_only=False )
            q05 = df_smooth.quantile([0.10], axis=1,)  # numeric_only=False)
            q33 = df_smooth.quantile([0.33], axis=1,)  # numeric_only=False)
            q66 = df_smooth.quantile([0.66], axis=1, )  # numeric_only=False)
            q95 = df_smooth.quantile([0.90], axis=1, )  # numeric_only=False)
            LOGGER.info('quantile calculated for all input data')
        except:
            LOGGER.exception('failed to calculate quantiles')

        try:
            plt.fill_between(df_smooth.index.values, np.squeeze(q05.values), np.squeeze(q95.values),
                             alpha=0.5, color='grey')
            plt.fill_between(df_smooth.index.values, np.squeeze(q33.values), np.squeeze(q66.values),
                             alpha=0.5, color='grey')

            plt.plot(df_smooth.index.values, np.squeeze(rmean.values), c='r', lw=3)

            plt.xlim(min(df.index.values), max(df.index.values))
            plt.ylim(ylim)
            plt.title(title, fontsize=20)
            plt.grid()  # .grid_line_alpha=0.3

            fig.savefig(output_png)
            plt.close()
            LOGGER.debug('timeseries uncertainty plot done for %s' % variable)
        except:
            LOGGER.exception('failed to calculate quantiles')
    except:
        LOGGER.exception('uncertainty plot failed for %s' % variable)
        _, output_png = mkstemp(dir='.', suffix='.png')
    return output_png
#
# def uncertainty(resouces, variable=None, ylim=None, title=None):
#     """
#     creates a png file containing the appropriate uncertainty plot.
#
#     :param resouces: list of files containing the same variable
#     :param variable: variable to be visualised. If None (default), variable will be detected
#     :param title: string to be used as title
#
#     :returns str: path/to/file.png
#     """
#     logger.debug('Start visualisation uncertainty plot')
#     from flyingpigeon.calculation import fieldmean
#     from flyingpigeon.utils import get_time, sort_by_filename
#
#     import pandas as pd
#     import numpy as np
#
#     # === prepare invironment
#     if type(resouces) == str:
#         resouces = list([resouces])
#     if variable is None:
#         variable = utils.get_variable(resouces[0])
#     if title is None:
#         title = "Field mean of %s " % variable
#     ncs = sort_by_filename(resouces)
#
#     try:
#         fig = plt.figure(figsize=(20, 10), dpi=600, facecolor='w', edgecolor='k')
#         o1, output_png = mkstemp(dir='.', suffix='.png')
#         variable = utils.get_variable(resouces[0])
#         df = pd.DataFrame()
#
#         logger.info('variable %s found in resources.' % variable)
#
#         for key in ncs.keys():
#             try:
#                 d = utils.get_time(ncs[key])  # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
#                 dt = [datetime.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in d]
#                 ts = fieldmean(ncs[key])
#                 plt.plot(dt, ts)
#
#
#                 hs = pd.Series(ts, index=dt, name=key)
#                 hd = hs.to_frame()
#                 df[key] = hd
#             except:
#                 logger.debug('failed to calculate timeseries for %s :' % (key))
#
#         # try:
#         #     if len(df.index) > 90:
#         #         rollmean = df.rolling(window=30, center=True).mean()
#         #         logger.info('rolling mean calculated for all input data')
#         #     else:
#         #         rollmean = df
#         #         # TODO : plot warning into graphic
#         #     rmean = rollmean.median(axis=1, skipna=False)  # quantile([0.5], axis=1, numeric_only=False )
#         #     q05 = rollmean.quantile([0.05], axis=1,)  # numeric_only=False)
#         #     q33 = rollmean.quantile([0.33], axis=1,)  # numeric_only=False)
#         #     q66 = rollmean.quantile([0.66], axis=1, )  # numeric_only=False)
#         #     q95 = rollmean.quantile([0.95], axis=1, )  # numeric_only=False)
#         #
#         #     logger.info('quantile calculated for all input data')
#         # except:
#         #     logger.debug('failed to calculate quantiles')
#         #
#         try:
#         #     plt.fill_between(rollmean.index.values, np.squeeze(q05.values), np.squeeze(q95.values),
#         #                      alpha=0.5, color='grey')
#         #     plt.fill_between(rollmean.index.values, np.squeeze(q33.values), np.squeeze(q66.values),
#         #                      alpha=0.5, color='grey')
#         #     plt.plot(rollmean.index.values, np.squeeze(rmean.values), c='r', lw=3)
#         #
#         #     plt.xlim(min(df.index.values), max(df.index.values))
#         #     plt.ylim(ylim)
#         #     plt.title(title, fontsize=20)
#         #     plt.grid()  # .grid_line_alpha=0.3
#
#             fig.savefig(output_png)
#             plt.close()
#             logger.debug('timeseries uncertainty plot done for %s' % variable)
#         except:
#             logger.exception('failed to calculate quantiles')
#     except:
#         logger.exception('uncertainty plot failed for %s' % variable)
#         raise
#     return output_png


def map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, variable=None, cmap='seismic', title=None):
    """
    generates a graphic for the output of the ensembleRobustness process for a lat/long file.

    :param signal: netCDF file containing the signal difference over time
    :param highagreement:
    :param lowagreement:
    :param variable:
    :param cmap: default='seismic',
    :param title: default='Model agreement of signal'
    :returns str: path/to/file.png
    """
    from flyingpigeon import utils
    from  numpy import mean

    if variable is None:
        variable = utils.get_variable(signal)

    try:
        # get the path of the file. It can be found in the repo data directory.

        # ds_signal = Dataset(signal, mode='r')
        # ds_lagree = Dataset(low_agreement_mask, mode='r')
        # ds_hagree = Dataset(high_agreement_mask, mode='r')

        var_signal = utils.get_values(signal)  # np.squeeze(ds_signal.variables[variable])
        mask_l = utils.get_values(low_agreement_mask)  #  np.squeeze(ds_lagree.variables[variable])
        mask_h = utils.get_values(high_agreement_mask)  #  np.squeeze(ds_hagree.variables[variable])

        mask_l[mask_l is 0] = np.nan
        mask_h[mask_h is 0] = np.nan

        logger.info('data loaded')

        lats, lons = utils.get_coordinates(signal, unrotate=True)
        # lons = np.squeeze(ds_signal.variables['lon'][:])
        # lats = np.squeeze(ds_signal.variables['lat'][:])

        if len(lats.shape) == 1:
            cyclic_var, cyclic_lons = add_cyclic_point(var_signal, coord=lons)
            mask_l, cyclic_lons = add_cyclic_point(mask_l, coord=lons)
            mask_h, cyclic_lons = add_cyclic_point(mask_h, coord=lons)

            lons = cyclic_lons.data
            var_signal = cyclic_var

        logger.info('lat lon loaded')

        minval = round(np.nanmin(var_signal))
        maxval = round(np.nanmax(var_signal)+.5)

        logger.info('prepared data for plotting')
    except Exception as e:
        msg = 'failed to get data for plotting %s' % e
        logger.exception(msg)
        raise Exception(msg)

    try:
        fig = plt.figure(facecolor='w', edgecolor='k')  # figsize=(20,10), dpi=600,

        ax = plt.axes(projection=ccrs.Robinson(central_longitude=int(mean(lons))))
        norm = MidpointNormalize(midpoint=0)

        cs = plt.contourf(lons, lats, var_signal, 60, norm=norm, transform=ccrs.PlateCarree(),
                          cmap=cmap, interpolation='nearest')
        cl = plt.contourf(lons, lats, mask_l, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['//'])
        ch = plt.contourf(lons, lats, mask_h, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['.'])

        # plt.clim(minval,maxval)
        ax.coastlines()
        # ax.set_global()

        if title is None:
            plt.title('%s with Agreement' % variable)
        else:
            plt.title(title)
        plt.colorbar(cs)

        plt.annotate('// = low model ensemble agreement', (0, 0), (0, -10),
                     xycoords='axes fraction', textcoords='offset points', va='top')
        plt.annotate('..  = high model ensemble agreement', (0, 0), (0, -20),
                     xycoords='axes fraction', textcoords='offset points', va='top')

        o1, graphic = mkstemp(dir='.', suffix='.png')
        fig.savefig(graphic)
        plt.close()

        logger.info('Plot created and figure saved')
    except:
        msg = 'failed to plot graphic'
        logger.exception(msg)

    return graphic


def concat_images(images, orientation='v'):
    """
    concatenation of images.

    :param images: list of images
    :param orientation: vertical ('v' default) or horizontal ('h') concatenation

    :return string: path to image
    """
    from PIL import Image
    import sys

    logger.debug('Images to be concatinated: %s' % images)

    if len(images) > 1:
        try:
            images_existing = [img for img in images if os.path.exists(img)]
            open_images = map(Image.open, images_existing)
            w = max(i.size[0] for i in open_images)
            h = max(i.size[1] for i in open_images)
            nr = len(open_images)
            if orientation == 'v':
                result = Image.new("RGB", (w, h * nr))
                # p = nr # h / len(images)
                for i in range(len(open_images)):
                    oi = open_images[i]
                    cw = oi.size[0]
                    ch = oi.size[1]
                    cp = h * i
                    box = [0, cp, cw, ch+cp]
                    result.paste(oi, box=box)

            if orientation == 'h':
                result = Image.new("RGB", (w * nr, h))
                # p = nr # h / len(images)
                for i in range(len(open_images)):
                    oi = open_images[i]

                    cw = oi.size[0]
                    ch = oi.size[1]
                    cp = w * i
                    box = [cp, 0, cw+cp, ch]
                    result.paste(oi, box=box)

            ip, image = mkstemp(dir='.', suffix='.png')
            result.save(image)
        except:
            logger.exception('failed to concat images')
            _, image = mkstemp(dir='.', suffix='.png')
            result = Image.new("RGB", (50, 50))
            result.save(image)
    elif len(images) == 1:
        image = images[0]
    else:
        logger.exception('No concatable number of images: %s, Dummy will be produced' % len(image))
        _, image = mkstemp(dir='.', suffix='.png')
        result = Image.new("RGB", (50, 50))
        result.save(image)
    return image


def pdfmerge(pdfs):
    """
    merge a list of pdfs

    :param pdfs: list of pdf files

    :retun str: merged pdf
    """
    from PyPDF2 import PdfFileMerger

    # pdfs = ['file1.pdf', 'file2.pdf', 'file3.pdf', 'file4.pdf']
    try:
        merger = PdfFileMerger()
        for pdf in pdfs:
            merger.append(pdf)
        _, mergedpdf = mkstemp(dir='.', suffix='.pdf')
        merger.write(mergedpdf)
    except:
        logger.excetion('failed to merge pdfs')
        _, mergedpdf = mkstemp(dir='.', suffix='.pdf')

    return mergedpdf


def map_gbifoccurrences(latlon, dir='.'):
    """
    creates a plot of coordinate points for tree occourences fetch in GBIF data base

    :param latlon: list of latitude longitude coordinates

    :return png: world map with occurences
    """
    # configure cartopy cache dir
    try:
        cartopy_config['data_dir'] = os.path.join(dir, 'cartopy')
        # plotting ...
        ip, tree_presents = mkstemp(dir=dir, suffix='.png')
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')
        ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
        ax.coastlines()
        ax.set_global()
        ax = plt.gca()
        # DEBUG: failed to plot occurency plot: Cannot label gridlines on a Robinson plot.
        #        Only PlateCarree and Mercator plots are currently supported.
        # ax.gridlines(draw_labels=True)
        # or..
        # ax.set_yticks([-60, -30, 0, 30, 60], crs=ccrs.PlateCarree())
        # ax.set_xticks(np.arange(-180, 240, 60), crs=ccrs.PlateCarree())
        # ax.gridlines()  # plt.gca()
        cs = plt.scatter(latlon[:, 1], latlon[:, 0], transform=ccrs.Geodetic())  # ccrs.PlateCarree()
        fig.savefig(tree_presents)
        plt.close()
    except Exception as e:
        msg = 'failed to plot occurency plot: %s' % e
        logger.debug(msg)
    return tree_presents


def map_PAmask(PAmask):
    """
    plots the presents absence mask used in Species distribution Model processes

    :param PAmask: output of sdm.get_PAmask

    :return png: path to png graphic
    """
    try:
        ip, png_PA_mask = mkstemp(dir='.', suffix='.png')
        fig = plt.figure(figsize=(20, 10), dpi=300, facecolor='w', edgecolor='k')
        cs = plt.contourf(PAmask)
        fig.savefig(png_PA_mask)
        plt.close()
    except Exception as e:
        msg = 'failed to plot the PA mask'
        logger.exception(msg)
        with open(png_PA_mask, 'w') as fp:
            # TODO: needs to be a png file
            fp.write(msg)
    return png_PA_mask


# def plot_tSNE(data, title='custer', sub_title='method: principal components'):
#     """
#     !!!Obsolete!!!
#     plot the output of weather classifiaction as a cluster
#     :param param: values for x y coordinate
#     :param title: string for title
#     """
    # fig = plt.figure(figsize=(10, 10))
    # plt.scatter(data[:, 0], data[:, 1], marker=".")
    # plt.title(title)
    # plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction', textcoords='offset points', va='top')
    #
    # ip, image = mkstemp(dir='.',suffix='.png')
    # plt.savefig(image)
    # plt.close()
    #
    # return image


def plot_kMEAN(kmeans, pca, title='kmean', sub_title='file='):
    """
    !!!Obsolete!!!
    """
    pass
    # from itertools import cycle
    # centroids = kmeans.cluster_centers_
    #
    # c = kmeans.predict(pca)
    # x = pca[:, 0]
    # y = pca[:, 1]
    #
    # fig = plt.figure(figsize=(10, 10))
    #
    # cx = centroids[:, 0]
    # cy= centroids[:, 1]
    # ct = plt.scatter(cx, cy,
    #         marker='.', s=100, linewidths=3,
    #         color='black', zorder=10)
    #
    # #n = ['1', '2','3','4']
    #
    # #for i, txt in enumerate(n):
    # #plt.annotate(txt, (cx[i],cy[i]))
    #
    # colors = cycle(["r", "b", "g", "y"])
    #
    # for i in range(max(c)+1):
    # plt.scatter(x[c==i],y[c==i],marker='.', s=30, lw=None, color=next(colors))
    #
    # plt.axvline(0)
    # plt.axhline(0)
    # plt.title(title)
    #
    # plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction', textcoords='offset points', va='top')
    #
    # ip, image = mkstemp(dir='.',suffix='.png')
    # plt.savefig(image)
    # plt.close()
    #
    # return image


def plot_pressuremap(data, lats=None, lons=None,
                     facecolor=None,  edgecolor=None, vmin=None, vmax=None,
                     title='Pressure Pattern',
                     sub_title='plotted in birdhouse'):
    """
    !!!Obsolete!!!
    plots pressure data
    :param data: 2D or 3D array of pressure data. if data == 3D, a mean will be calculated
    :param lats: 1D or 2D array for latitude coordinates (geographcal map will be plotted if lats/lons are provided)
    :param lons: 1D or 2D array for longitude coordinates (geographcal map will be plotted if lats/lons are provided)
    :param title: string for title
    :param sub_title: string for sub_title
    """
    pass
    # from numpy import squeeze, mean, meshgrid
    # norm = MidpointNormalize(midpoint=0, vmin=vmin, vmax=vmax)
    # d = squeeze(data)
    #
    # if len(d.shape)==3:
    # d = mean(d, axis=0)
    # if len(d.shape)!=2:
    # logger.error('data are not in shape for map display')
    #
    # # fig = plt.figure( )
    # # fig.patch.set_facecolor(facecolor)
    #
    # if not (lats == None or lons == None):
    # if len(lats.shape) == 1:
    # lons, lats = meshgrid( lons, lats)
    # central_longitude = int(mean(lons))
    #
    # #AlbersEqualArea(central_longitude=0.0, central_latitude=0.0, false_easting=0.0, false_northing=0.0,
    #                  standard_parallels=(20.0, 50.0), globe=None)
    #
    # ax = plt.axes(projection=ccrs.AlbersEqualArea(central_longitude=central_longitude), axisbg=facecolor),
    #               Robinson(central_longitude=central_longitude))
    # ax.gridlines()
    # ax.coastlines()
    #
    # cf = plt.contourf(lons, lats, d, 60, transform=ccrs.PlateCarree(), norm=norm, cmap='jet', interpolation=None)
    #                   # 'nearest'
    # co = plt.contour(lons, lats, d, transform=ccrs.PlateCarree(), lw=2, color='black')
    # else:
    # cf = plt.contourf(d, norm=norm, cmap='jet')
    # co = plt.contour(d, lw=2, c='black')
    #
    # plt.colorbar(cf, shrink=0.5,)
    # # clb = plt.colorbar( ticks=clevs)
    # plt.clabel(co, inline=1) # fontsize=10
    # plt.title(title)
    # plt.annotate(sub_title, (0,0), (0, -30), xycoords='axes fraction',
    #        textcoords='offset points', va='top')
    #
    # ip, image = mkstemp(dir='.',suffix='.png')
    # plt.savefig(image)
    # plt.close()
    # return image
