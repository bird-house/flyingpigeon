# encoding: utf8
import os
from os.path import join
from tempfile import mkstemp
from netCDF4 import Dataset
from datetime import datetime, date
import numpy as np
import logging
from matplotlib import use
use('Agg')   # use this if no xserver is available


from matplotlib import pyplot as plt
from matplotlib.colors import Normalize
from cartopy import config as cartopy_config
import cartopy.crs as ccrs
from flyingpigeon import utils

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


def fig2plot(fig, file_extension='png', bbox_inches='tight', dpi=300, facecolor='w', edgecolor='k'):
    '''saving a matplotlib figure to a graphic

    :param fig: matplotlib figure object
    :param file_extension: file file_extension (default='png')

    :return str: path to graphic
    '''

    _, graphic = mkstemp(dir='.', suffix='.%s' % file_extension)
    fig.savefig(graphic, bbox_inches=bbox_inches)

    return graphic


def plot_extend(resource, file_extension='png'):
    """
    plots the extend (domain) of the values stored in a netCDF file:

    :parm resource: path to netCDF file
    :param file_extension: file format of the graphic. if file_extension=None a matplotlib figure will be returned

    :return graphic: graphic in specified format
    """
    import matplotlib.patches as mpatches
    lats, lons = utils.get_coordinates(resource, unrotate=True)

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
    ax.add_patch(mpatches.Polygon(xy, closed=True,  transform=ccrs.PlateCarree(), color='coral', alpha=0.6))
    # ccrs.Geodetic()
    ax.gridlines()
    plt.show()

    if file_extension is None:
        map_graphic = fig
    else:
        map_graphic = fig2plot(fig=fig, file_extension=file_extension)
    plt.close()

    return map_graphic


def plot_polygons(regions, file_extension='png'):
    """
    extract the polygon coordinate and plot it on a worldmap

    :param regions: list of ISO abreviations for polygons

    :return png: map_graphic.png
    """

    from cartopy.io.shapereader import Reader
    from cartopy.feature import ShapelyFeature
<<<<<<< HEAD
    from os.path import curdir, abspath
=======
    from numpy import mean, append
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34

    from flyingpigeon import config
    DIR_SHP = config.shapefiles_dir()

    if type(regions) == str:
        regions = list([regions])

    fname = join(DIR_SHP, "countries.shp")
    geos = Reader(fname).geometries()
    records = Reader(fname).records()
    central_latitude = []
    central_longitude = []

<<<<<<< HEAD
    LOGGER.debug('')

    fig = plt.figure(figsize=(10, 10), facecolor='w', edgecolor='k')  # dpi=600,
    projection = ccrs.Orthographic(central_longitude=0.0, central_latitude=0.0, globe=None)  # Robinson()
=======
    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            x, y = geo.centroid.coords.xy
            central_longitude.append(x[0])
            central_latitude.append(y[0])

    fig = plt.figure(figsize=(20, 10))
    projection = ccrs.Orthographic(central_longitude=mean(central_longitude),
                                   central_latitude=mean(central_latitude),
                                   globe=None)  # Robinson()
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
    ax = plt.axes(projection=projection)

    geos = Reader(fname).geometries()
    records = Reader(fname).records()

    for r in records:
        geo = geos.next()
        if r.attributes['ISO_A3'] in regions:
            shape_feature = ShapelyFeature(geo, ccrs.PlateCarree(), edgecolor='black', color='coral')
            ax.add_feature(shape_feature)
<<<<<<< HEAD
        ax.coastlines()
        # ax.set_global()

    o1, map_graphic = mkstemp(dir=abspath(curdir), suffix='.png')
    fig.savefig(map_graphic)
=======
    ax.coastlines()
    ax.gridlines()
    ax.stock_img()
    # ax.set_global()
    map_graphic = fig2plot(fig=fig, file_extension=file_extension)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
    plt.close()

    return map_graphic


<<<<<<< HEAD
def factsheetbrewer(png_country=None, png_spaghetti=None, png_uncertainty=None, png_robustness=None):
=======
def factsheetbrewer(png_region=None, png_spaghetti=None, png_uncertainty=None, png_robustness=None):
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
    """
    Put graphics into the climate fact sheet template to generate the final climate fact sheet

    :param png_region: World map graphic with countries polygons.
    :param png_uncertainty: Graphic showing a timeseries with fieldmean values and corresponding uncertainty
    :param png_spaghetti: Graphic showing each datatset as a single timeseries
    :param png_robustness: Map of the signal change including hashes and dots for robutsness values

    :return pdf foumular: pdf with fillable text boxes for interpretation text
    """
    from PyPDF2 import PdfFileWriter, PdfFileReader
    from reportlab.pdfgen import canvas
    from flyingpigeon.config import data_path
    try:
        try:
<<<<<<< HEAD
            _, pdf_country = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_country)
            c.drawImage(png_country, 355, 500, width=120, height=120)  # , mask=None, preserveAspectRatio=False)
=======
            _, pdf_region = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_region)
            c.drawImage(png_region, 340, 490, width=130, height=130)  # , mask=None, preserveAspectRatio=False)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
            c.save()
            pfr_region = PdfFileReader(open(pdf_region, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_uncertainty = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_uncertainty)
<<<<<<< HEAD
            c.drawImage(png_uncertainty, 20, 370, width=300, height=120)  # , mask=None, preserveAspectRatio=False)
=======
            c.drawImage(png_uncertainty, 20, 350, width=250, height=130)  # , mask=None, preserveAspectRatio=False)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
            c.save()
            pfr_uncertainty = PdfFileReader(open(pdf_uncertainty, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_spaghetti = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_spaghetti)
<<<<<<< HEAD
            c.drawImage(png_spaghetti, 280, 370, width=300, height=120)  # , mask=None, preserveAspectRatio=False)
=======
            c.drawImage(png_spaghetti, 280, 350, width=250, height=130)  # , mask=None, preserveAspectRatio=False)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
            c.save()
            pfr_spagetthi = PdfFileReader(open(pdf_spaghetti, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_robustness = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_robustness)
<<<<<<< HEAD
            c.drawImage(png_robustness, 55, 90, width=250, height=180)  # , mask=None, preserveAspectRatio=False)
=======
            c.drawImage(png_robustness, 30, 100, width=200, height=170)  # , mask=None, preserveAspectRatio=False)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
            c.save()
            pfr_robustness = PdfFileReader(open(pdf_robustness, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        output_file = PdfFileWriter()
        pfr_template = PdfFileReader(file(data_path() + '/pdf/climatefactsheettemplate.pdf', 'rb'))
        LOGGER.debug('template: %s' % pfr_template)

        page_count = pfr_template.getNumPages()
        for page_number in range(page_count):
            LOGGER.debug("Plotting png to {} of {}".format(page_number, page_count))
            input_page = pfr_template.getPage(page_number)
            try:
                input_page.mergePage(pfr_region.getPage(0))
            except:
                LOGGER.warn('failed to merge courtry map')
            try:
                input_page.mergePage(pfr_uncertainty.getPage(0))
            except:
                LOGGER.warn('failed to merge uncertainty plot')
            try:
                input_page.mergePage(pfr_spagetthi.getPage(0))
            except:
                LOGGER.warn('failed to merge spaghetti plot')
            try:
                input_page.mergePage(pfr_robustness.getPage(0))
            except:
                LOGGER.warn('failed to merge robustness plot')
            try:
                output_file.addPage(input_page)
            except:
                LOGGER.warn('failed to add page to output pdf')
        try:
            _, climatefactsheet = mkstemp(dir='.', suffix='.pdf')
            with open(climatefactsheet, 'wb') as outputStream:
                output_file.write(outputStream)
            LOGGER.info('sucessfully brewed the demanded factsheet')
        except:
            LOGGER.exception('failed write filled template to pdf. empty template will be set as output')
            climatefactsheet = data_path() + '/pdf/climatefactsheettemplate.pdf'
    except:
        LOGGER.exception("failed to brew the factsheet, empty template will be set as output")
    return climatefactsheet


<<<<<<< HEAD
def spaghetti(resouces, variable=None, title=None, dir_out=None):
=======
def spaghetti(resouces, variable=None, title=None, file_extension='png'):
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
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
<<<<<<< HEAD
        o1, output_png = mkstemp(dir=dir_out, suffix='.png')
=======
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
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

        output_png = fig2plot(fig=fig, file_extension=file_extension)

        plt.close()
        LOGGER.info('timeseries spaghetti plot done for %s with %s lines.' % (variable, c))
    except:
        msg = 'matplotlib spaghetti plot failed'
        LOGGER.exception(msg)
        _, output_png = mkstemp(dir='.', suffix='.png')
    return output_png


<<<<<<< HEAD
def uncertainty(resouces, variable=None, ylim=None, title=None, dir_out=None):
=======
def uncertainty(resouces, variable=None, ylim=None, title=None, file_extension='png', window=None):
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
    """
    creates a png file containing the appropriate uncertainty plot.

    :param resouces: list of files containing the same variable
    :param variable: variable to be visualised. If None (default), variable will be detected
    :param title: string to be used as title
<<<<<<< HEAD
=======
    :param window: windowsize of the rolling mean
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34

    :returns str: path/to/file.png
    """
    LOGGER.debug('Start visualisation uncertainty plot')

    import pandas as pd
    import numpy as np
    import netCDF4
    from os.path import basename
<<<<<<< HEAD
=======
    from flyingpigeon.utils import get_time, sort_by_filename
    from flyingpigeon.calculation import fieldmean
    from flyingpigeon.metadata import get_frequency
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34

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
<<<<<<< HEAD
        o1, output_png = mkstemp(dir=dir_out, suffix='.png')
        variable = utils.get_variable(resouces[0])
        df = pd.DataFrame()

        LOGGER.info('variable %s found in resources.' % variable)
        for f in resouces:
            try:
                ds = Dataset(f)
                data = np.squeeze(ds.variables[variable][:])
                if len(data.shape) == 3:
                    meanData = np.mean(data, axis=1)
                    ts = np.mean(meanData, axis=1)
                else:
                    ts = data[:]

                times = ds.variables['time']
                jd = netCDF4.num2date(times[:], times.units)

                hs = pd.Series(ts, index=jd, name=basename(f))
                hd = hs.to_frame()
                df[basename(f)] = hs
            except:
                LOGGER.exception('failed to calculate timeseries for %s ' % (f))

=======
        #  variable = utils.get_variable(resouces[0])
        df = pd.DataFrame()

        LOGGER.info('variable %s found in resources.' % variable)
        datasets = sort_by_filename(resouces, historical_concatination=True)

        for key in datasets.keys():
            try:
                data = fieldmean(datasets[key])  # get_values(f)
                ts = get_time(datasets[key])
                ds = pd.Series(data=data, index=ts, name=key)
                # ds_yr = ds.resample('12M', ).mean()   # yearly mean loffset='6M'
                df[key] = ds

            except Exception:
                LOGGER.exception('failed to calculate timeseries for %s ' % (key))

        frq = get_frequency(resouces[0])
        print frq

        if window is None:
            if frq == 'day':
                window = 10951
            elif frq == 'man':
                window = 359
            elif frq == 'sem':
                window = 119
            elif frq == 'yr':
                window = 30
            else:
                LOGGER.debug('frequency %s is not included' % frq)
                window = 30

        if len(df.index.values) >= window * 2:
            # TODO: calculate windowsize according to timestapms (day,mon,yr ... with get_frequency)
            df_smooth = df.rolling(window=window, center=True).mean()
            LOGGER.info('rolling mean calculated for all input data')
        else:
            df_smooth = df
            LOGGER.debug('timeseries too short for moving mean')
            fig.text(0.95, 0.05, '!!! timeseries too short for moving mean over 30years !!!',
                     fontsize=20, color='red',
                     ha='right', va='bottom', alpha=0.5)

>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
        try:
            rollmean = df.rolling(window=30, center=True).mean()
            LOGGER.info('rolling mean calculated for all input data')
            rmean = rollmean.median(axis=1, skipna=False)  # quantile([0.5], axis=1, numeric_only=False )
            q05 = rollmean.quantile([0.05], axis=1,)  # numeric_only=False)
            q33 = rollmean.quantile([0.33], axis=1,)  # numeric_only=False)
            q66 = rollmean.quantile([0.66], axis=1, )  # numeric_only=False)
            q95 = rollmean.quantile([0.95], axis=1, )  # numeric_only=False)

            LOGGER.info('quantile calculated for all input data')
        except Exception:
            LOGGER.exception('failed to calculate quantiles')

        try:
            plt.fill_between(rollmean.index.values, np.squeeze(q05.values), np.squeeze(q95.values),
                             alpha=0.5, color='grey')
            plt.fill_between(rollmean.index.values, np.squeeze(q33.values), np.squeeze(q66.values),
                             alpha=0.5, color='grey')
            plt.plot(rollmean.index.values, np.squeeze(rmean.values), c='r', lw=3)

            plt.xlim(min(df.index.values), max(df.index.values))
            plt.ylim(ylim)
            plt.title(title, fontsize=20)
            plt.grid()  # .grid_line_alpha=0.3

            output_png = fig2plot(fig=fig, file_extension=file_extension)
            plt.close()
            LOGGER.debug('timeseries uncertainty plot done for %s' % variable)
        except Exception as err:
            raise Exception('failed to calculate quantiles. %s' % err.message)
    except Exception:
        LOGGER.exception('uncertainty plot failed for %s.' % variable)
        _, output_png = mkstemp(dir='.', suffix='.png')
    return output_png


<<<<<<< HEAD
def map_robustness(signal, high_agreement_mask, low_agreement_mask, cmap='seismic', title=None):
=======
def map_robustness(signal, high_agreement_mask, low_agreement_mask,
                   variable=None, cmap='seismic', title=None,
                   file_extension='png'):
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
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
<<<<<<< HEAD
=======
    from flyingpigeon import utils
    from numpy import mean, ma

    if variable is None:
        variable = utils.get_variable(signal)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34

    try:
        # from flyingpigeon.utils import get_values
        from cartopy.util import add_cyclic_point
        # from flyingpigeon.utils import get_coordinates

        var_signal = utils.get_values(signal)
        mask_l = utils.get_values(low_agreement_mask)
        mask_h = utils.get_values(high_agreement_mask)

<<<<<<< HEAD
        mask_l.mask = var_signal.mask
        mask_h.mask = var_signal.mask
=======
        # mask_l = ma.masked_where(low < 0.5, low)
        # mask_h = ma.masked_where(high < 0.5, high)
        # mask_l[mask_l == 0] = np.nan
        # mask_h[mask_h == 0] = np.nan

        LOGGER.info('data loaded')

        lats, lons = utils.get_coordinates(signal, unrotate=True)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34

        mask_l.mask[mask_l.data is 0] = False
        mask_h.mask[mask_h.data is 0] = False

        LOGGER.debug('values loaded')

        lats, lons = utils.get_coordinates(signal)

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

        LOGGER.info('prepared data for plotting')
    except:
        msg = 'failed to get data for plotting'
        LOGGER.exception(msg)

    try:
        fig = plt.figure(facecolor='w', edgecolor='k')  # figsize=(20,10), dpi=600,
        ax = plt.axes(projection=ccrs.Robinson(central_longitude=0))
        norm = MidpointNormalize(midpoint=0)

<<<<<<< HEAD
        cs = plt.contourf(lons, lats, var_signal,
                          60, transform=ccrs.PlateCarree(),
                          norm=norm, cmap=cmap, interpolation='nearest')
        cl = plt.contourf(lons, lats, mask_l, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['//'])
        ch = plt.contourf(lons, lats, mask_h, 60, transform=ccrs.PlateCarree(), colors='none', hatches=['.'])

        # plt.clim(minval, maxval)
=======
        cs = plt.contourf(lons, lats, var_signal, 60, norm=norm, transform=ccrs.PlateCarree(),
                          cmap=cmap, interpolation='nearest')

        cl = plt.contourf(lons, lats, mask_l, 1, transform=ccrs.PlateCarree(), colors='none', hatches=[None, '/'])
        ch = plt.contourf(lons, lats, mask_h, 1, transform=ccrs.PlateCarree(), colors='none', hatches=[None, '.'])
        # artists, labels = ch.legend_elements()
        # plt.legend(artists, labels, handleheight=2)
        # plt.clim(minval,maxval)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
        ax.coastlines()
        ax.gridlines()
        # ax.set_global()

        if title is None:
            plt.title('Robustness')
        else:
            plt.title(title)

        plt.colorbar(cs)

        plt.annotate('// = low model ensemble agreement', (0, 0), (0, -10),
                     xycoords='axes fraction', textcoords='offset points', va='top')
        plt.annotate('..  = high model ensemble agreement', (0, 0), (0, -20),
                     xycoords='axes fraction', textcoords='offset points', va='top')

<<<<<<< HEAD
        _, graphic = mkstemp(dir='.', suffix='.png')
        fig.savefig(graphic)
=======
        graphic = fig2plot(fig=fig, file_extension=file_extension)
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
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


def map_gbifoccurrences(latlon, dir='.', file_extension='png'):
    """
    creates a plot of coordinate points for tree occourences fetch in GBIF data base

    :param latlon: list of latitude longitude coordinates

    :return png: world map with occurences
    """
    # configure cartopy cache dir
    try:
        cartopy_config['data_dir'] = os.path.join(dir, 'cartopy')
        # plotting ...
        # ip, tree_presents = mkstemp(dir=dir, suffix='.png')
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

        tree_presents = fig2plot(fig=fig, file_extension=file_extension)

        plt.close()
    except Exception as e:
        msg = 'failed to plot occurency plot: %s' % e
        LOGGER.debug(msg)
    return tree_presents


def map_PAmask(PAmask, file_extension='png'):
    """
    plots the presents absence mask used in Species distribution Model processes

    :param PAmask: output of sdm.get_PAmask

    :return png: path to png graphic
    """
    try:
        ip, png_PA_mask = mkstemp(dir='.', suffix='.png')
        fig = plt.figure(figsize=(20, 10), facecolor='w', edgecolor='k')  # dpi=300,
        cs = plt.contourf(PAmask)
        png_PA_mask = fig2plot(fig=fig, file_extension=file_extension)
        plt.close()
    except Exception as e:
        msg = 'failed to plot the PA mask'
        LOGGER.exception(msg)
        with open(png_PA_mask, 'w') as fp:
            # TODO: needs to be a png file
            fp.write(msg)
    return png_PA_mask


<<<<<<< HEAD
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
=======
def map_spatial_analog(ncfile, variable='dissimilarity', cmap='viridis', title='Spatial analog'):
    """Return a matplotlib Figure instance showing a map of the dissimilarity measure.
    """
    import netCDF4 as nc
    from flyingpigeon import utils
    from mpl_toolkits.axes_grid import make_axes_locatable
    import matplotlib.axes as maxes

    try:
        var = utils.get_values(ncfile, variable)
        LOGGER.info('Data loaded')

        lats, lons = utils.get_coordinates(ncfile, variable=variable, unrotate=False)

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

    except:
        msg = 'failed to plot graphic'
        LOGGER.exception(msg)

    LOGGER.info('Plot created and figure saved')
    return fig
>>>>>>> 0565e9460e15a790333f8b61db92180dacdb6f34
