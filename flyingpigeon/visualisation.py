import os
from os.path import join
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date
import numpy as np
import matplotlib
matplotlib.use('Agg')   # use this if no xserver is available

from matplotlib import pyplot as plt
from matplotlib.colors import Normalize
from cartopy import config as cartopy_config
import cartopy.crs as ccrs

from flyingpigeon import utils
from flyingpigeon import config

import logging
LOGGER = logging.getLogger("PYWPS")

os.environ['HOME'] = os.curdir


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
    from os.path import curdir, abspath

    if type(regions) == str:
        regions = list([regions])

    fname = join(config.shapefiles_path(), "countries.shp")
    geos = Reader(fname).geometries()
    records = Reader(fname).records()
    xs = []
    ys = []

    # get central longitudes and latitueds of polygons
    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            xy = geo.centroid.coords.xy
            xs.append(xy[0][0])
            ys.append(xy[1][0])

    fig = plt.figure(figsize=(10, 10), facecolor='w', edgecolor='k')  # dpi=600,
    projection = ccrs.Orthographic(central_longitude=np.mean(xs), central_latitude=np.mean(ys), globe=None)  # Robinson()
    ax = plt.axes(projection=projection)

    geos = Reader(fname).geometries()
    records = Reader(fname).records()

    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            shape_feature = ShapelyFeature(geo,
                                           ccrs.PlateCarree(),
                                           edgecolor='black')
            ax.add_feature(shape_feature)

    ax.coastlines()
        # ax.set_global()

    o1, map_graphic = mkstemp(dir=abspath(curdir), suffix='.png')
    fig.savefig(map_graphic)
    plt.close()

    return map_graphic


def factsheetbrewer(png_country=None, png_spaghetti=None, png_uncertainty=None, png_robustness=None):
    """
    Put graphics into the climate fact sheet template to generate the final climate fact sheet

    :param png_country: World map graphic with countries polygons.
    :param png_uncertainty: Graphic showing a timeseries with fieldmean values and corresponding uncertainty

    :return pdf foumular: pdf with fillable text boxes for interpretation text
    """
    from PyPDF2 import PdfFileWriter, PdfFileReader
    from reportlab.pdfgen import canvas

    try:
        try:
            _, pdf_country = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_country)
            c.drawImage(png_country, 355, 500, width=120, height=120)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_country = PdfFileReader(open(pdf_country, 'rb'))
            LOGGER.info('converted png to pdf')
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_uncertainty = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_uncertainty)
            c.drawImage(png_uncertainty, 20, 370, width=300, height=120)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_uncertainty = PdfFileReader(open(pdf_uncertainty, 'rb'))
            LOGGER.info('converted png to pdf')
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_spaghetti = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_spaghetti)
            c.drawImage(png_spaghetti, 280, 370, width=300, height=120)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_spagetthi = PdfFileReader(open(pdf_spaghetti, 'rb'))
            LOGGER.info('converted png to pdf')
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_robustness = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_robustness)
            c.drawImage(png_robustness, 55, 90, width=250, height=180)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_robustness = PdfFileReader(open(pdf_robustness, 'rb'))
            LOGGER.info('converted png to pdf')
        except:
            LOGGER.exception('failed to convert png to pdf')

        output_file = PdfFileWriter()
        pfr_template = PdfFileReader(
            file(os.path.join(config.data_path(), 'pdf', 'climatefactsheettemplate.pdf'), 'rb'))
        LOGGER.debug('template: %s' % pfr_template)

        page_count = pfr_template.getNumPages()
        for page_number in range(page_count):
            LOGGER.debug("Plotting png to {} of {}".format(page_number, page_count))
            input_page = pfr_template.getPage(page_number)
            try:
                input_page.mergePage(pfr_country.getPage(0))
            except:
                LOGGER.exception('failed to merge courtry map')
            try:
                input_page.mergePage(pfr_uncertainty.getPage(0))
            except:
                LOGGER.exception('failed to merge uncertainy plot')
            try:
                input_page.mergePage(pfr_spagetthi.getPage(0))
            except:
                LOGGER.exception('failed to merge spaghetti plot')
            try:
                input_page.mergePage(pfr_robustness.getPage(0))
            except:
                LOGGER.exception('failed to merge robustness plot')
            try:
                output_file.addPage(input_page)
            except:
                LOGGER.exception('failed to add page to output pdf')
        try:
            _, climatefactsheet = mkstemp(dir='.', suffix='.pdf')
            with open(climatefactsheet, 'wb') as outputStream:
                output_file.write(outputStream)
            LOGGER.info('sucessfully brewed the demanded factsheet')
        except:
            LOGGER.exception('failed write filled template to pdf. empty template will be set as output')
            climatefactsheet = os.path.join(config.data_path(), 'pdf', 'climatefactsheettemplate.pdf')
    except:
        LOGGER.exception("failed to brew the factsheet, empty template will be set as output")
    return climatefactsheet


def spaghetti(resouces, variable=None, title=None, dir_out=None):
    """
    creates a png file containing the appropriate spaghetti plot as a field mean of the values.

    :param resouces: list of files containing the same variable
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title
    :param dir_out: directory for output files

    :retruns str: path to png file
    """

    try:
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')  # dpi=600,
        LOGGER.debug('Start visualisation spaghetti plot')

        # === prepare invironment
        if type(resouces) != list:
            resouces = [resouces]
        if variable is None:
            variable = utils.get_variable(resouces[0])
        if title is None:
            title = "Field mean of %s " % variable
        if dir_out is None:
            dir_out = os.curdir
        LOGGER.info('plot values preparation done')
    except:
        msg = "plot values preparation failed"
        LOGGER.exception(msg)
        raise Exception(msg)
    try:
        o1, output_png = mkstemp(dir=dir_out, suffix='.png')
        for c, nc in enumerate(resouces):
            # get timestapms
            try:
                d = utils.get_time(nc)  # [datetime.strptime(elem, '%Y-%m-%d') for elem in strDate[0]]
                dt = [datetime.strptime(str(i), '%Y-%m-%d %H:%M:%S') for i in d]
                ds = Dataset(nc)
                data = np.squeeze(ds.variables[variable][:])
                if len(data.shape) == 3:
                    meanData = np.mean(data, axis=1)
                    ts = np.mean(meanData, axis=1)
                else:
                    ts = data[:]
                plt.plot(dt, ts)
                # fig.line( dt,ts )
            except:
                msg = "spaghetti plot failed for"
                LOGGER.exception(msg)
                raise Exception(msg)

        plt.title(title, fontsize=20)
        plt.grid()
        fig.savefig(output_png)
        plt.close()
        LOGGER.info('timeseries spaghetti plot done for %s with %s lines.' % (variable, c))
    except:
        msg = 'matplotlib spaghetti plot failed'
        LOGGER.exception(msg)
        _, output_png = mkstemp(dir='.', suffix='.png')
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


def map_robustness(signal, high_agreement_mask, low_agreement_mask, cmap='seismic', title=None):
    """
    generates a graphic for the output of the ensembleRobustness process for a lat/long file.

    :param signal: netCDF file containing the signal difference over time
    :param highagreement:
    :param lowagreement:
    :param variable:
    :param ic: default='seismic',
    :param title: default='Model agreement of signal'
    :returns str: path/to/file.png
    """

    try:
        # from flyingpigeon.utils import get_values
        from cartopy.util import add_cyclic_point
        # from flyingpigeon.utils import get_coordinates

        var_signal = utils.get_values(signal)
        mask_l = utils.get_values(low_agreement_mask)
        mask_h = utils.get_values(high_agreement_mask)

        # mask_l.mask = var_signal.mask
        # mask_h.mask = var_signal.mask

        # mask_l[mask_l.data is 0] = False
        # mask_h[mask_h.data is 0] = False

        LOGGER.debug('values loaded')

        lats, lons = utils.unrotate_pole(signal, write_to_file=False)

        #
        # cyclic_var, cyclic_lons = add_cyclic_point(var_signal, coord=lons)
        # mask_l, cyclic_lons = add_cyclic_point(mask_l, coord=lons)
        # mask_h, cyclic_lons = add_cyclic_point(mask_h, coord=lons)
        #
        # lons = cyclic_lons.data
        # var_signal = cyclic_var
        #
        LOGGER.debug('coordinates loaded')
        #
        minval = round(np.nanmin(var_signal))
        maxval = round(np.nanmax(var_signal) + .5)
        central_longitude = np.median(lons)

        LOGGER.info('prepared data for plotting')
    except:
        msg = 'failed to get data for plotting'
        LOGGER.exception(msg)

    try:
        fig = plt.figure(facecolor='w', edgecolor='k')  # figsize=(20,10), dpi=600,
        central_longitude = int(np.mean(lons))
        ax = plt.axes(projection=ccrs.Robinson(central_longitude=central_longitude))
        norm = MidpointNormalize(midpoint=0)

        cs = ax.contourf(lons, lats, var_signal, 60, transform=ccrs.PlateCarree(), cmap=cmap,
                         norm=norm, interpolation='nearest')  # var_signal,  )
        cl = ax.contourf(lons, lats, mask_l, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['//'])
        ch = ax.contourf(lons, lats, mask_h, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['.'])

        # plt.clim(minval, maxval)

        ax.coastlines()
        ax.gridlines(draw_labels=True)
        plt.colorbar(cs)

        # ax.set_global()

        if title is None:
            plt.title('Robustness')
        else:
            plt.title(title)

        plt.annotate('// = low model ensemble agreement', (0, 0), (0, -10),
                     xycoords='axes fraction', textcoords='offset points', va='top')
        plt.annotate('..  = high model ensemble agreement', (0, 0), (0, -20),
                     xycoords='axes fraction', textcoords='offset points', va='top')

        _, graphic = mkstemp(dir='.', suffix='.png')
        fig.savefig(graphic)
        plt.close()

        LOGGER.info('Plot created and figure saved')
    except:
        msg = 'failed to plot graphic'
        LOGGER.exception(msg)
        _, graphic = mkstemp(dir='.', suffix='.png')
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

    LOGGER.debug('Images to be concatinated: %s' % images)

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
            LOGGER.exception('failed to concat images')
            _, image = mkstemp(dir='.', suffix='.png')
            result = Image.new("RGB", (50, 50))
            result.save(image)
    elif len(images) == 1:
        image = images[0]
    else:
        LOGGER.exception('No concatable number of images: %s, Dummy will be produced' % len(image))
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
        LOGGER.excetion('failed to merge pdfs')
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
        LOGGER.debug(msg)
    return tree_presents


def map_PAmask(PAmask):
    """
    plots the presents absence mask used in Species distribution Model processes

    :param PAmask: output of sdm.get_PAmask

    :return png: path to png graphic
    """
    try:
        ip, png_PA_mask = mkstemp(dir='.', suffix='.png')
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')  # dpi=300,
        cs = plt.contourf(PAmask)
        fig.savefig(png_PA_mask)
        plt.close()
    except Exception as e:
        msg = 'failed to plot the PA mask'
        LOGGER.exception(msg)
        with open(png_PA_mask, 'w') as fp:
            # TODO: needs to be a png file
            fp.write(msg)
    return png_PA_mask


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
    # LOGGER.error('data are not in shape for map display')
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
