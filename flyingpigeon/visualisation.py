# encoding: utf8
import os
from tempfile import mkstemp
import numpy as np
import logging
from matplotlib import use
use('Agg')   # use this if no xserver is available

from matplotlib import pyplot as plt
from cartopy import config as cartopy_config
from cartopy.util import add_cyclic_point
import cartopy.crs as ccrs

LOGGER = logging.getLogger("PYWPS")

from eggshell.visualisation import MidpointNormalize, fig2plot, plot_extend, plot_polygons, spaghetti, uncertainty
from eggshell.visualisation import map_robustness, concat_images

def factsheetbrewer(png_region=None, png_spaghetti=None, png_uncertainty=None, png_robustness=None):
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
            _, pdf_region = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_region)
            c.drawImage(png_region, 340, 490, width=130, height=130)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_region = PdfFileReader(open(pdf_region, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_uncertainty = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_uncertainty)
            c.drawImage(png_uncertainty, 20, 350, width=250, height=130)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_uncertainty = PdfFileReader(open(pdf_uncertainty, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_spaghetti = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_spaghetti)
            c.drawImage(png_spaghetti, 280, 350, width=250, height=130)  # , mask=None, preserveAspectRatio=False)
            c.save()
            pfr_spagetthi = PdfFileReader(open(pdf_spaghetti, 'rb'))
        except:
            LOGGER.exception('failed to convert png to pdf')

        try:
            _, pdf_robustness = mkstemp(dir='.', suffix='.pdf')
            c = canvas.Canvas(pdf_robustness)
            c.drawImage(png_robustness, 30, 100, width=200, height=170)  # , mask=None, preserveAspectRatio=False)
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
        fig = plt.figure(figsize=(20, 10), dpi=300, facecolor='w', edgecolor='k')
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
