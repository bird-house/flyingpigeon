import os
from pywps import configuration

_PATH = os.path.abspath(os.path.dirname(__file__))

from chameleon import PageTemplateLoader
TEMPLATES = PageTemplateLoader(os.path.join(_PATH, "templates"))

import logging
LOGGER = logging.getLogger("PYWPS")


def esgfsearch_url():
    url = configuration.get_config_value("extra", "esgfsearch_url")
    if not url:
        LOGGER.warn("No ESGF Search URL configured. Using default value.")
        url = 'https://esgf-data.dkrz.de/esg-search'
    return url


def esgfsearch_distrib():
    distrib = configuration.get_config_value("extra", "esgfsearch_distrib")
    if distrib is None:
        LOGGER.warn("No ESGF Search distrib option configured. Using default value.")
        distrib = True
    return distrib


def shapefiles_dir():
    return os.path.join(_PATH, 'shapefiles')


def Rsrc_dir():
    return os.path.join(_PATH, 'Rsrc')


def masks_dir():
    return os.path.join(_PATH, 'masks')


def static_dir():
    return os.path.join(_PATH, 'static')


def cache_path():
    cache_path = configuration.get_config_value("cache", "cache_path")
    if not cache_path:
        LOGGER.warn("No cache path configured. Using default value.")
        cache_path = os.path.join(os.sep, "tmp", "cache")
    return cache_path


def output_path():
    output_path = configuration.get_config_value("server", "outputpath")
    if not output_path:
        output_path = None
        LOGGER.warn('no output path configured')
    return output_path


def output_url():
    url = configuration.get_config_value("server", "outputurl")
    if not url:
        url = None
        LOGGER.warn('no outputurl configured')
    return url


def www_url():
    url = configuration.get_config_value("extra", "www_url")
    if not url:
        url = None
        LOGGER.warn('no www-url configured')
    return url
