import os
from pywps import configuration

_PATH = os.path.abspath(os.path.dirname(__file__))


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


def static_path():
    return os.path.join(_PATH, 'static')


def data_path():
    return os.path.join(_PATH, 'data')


def shapefiles_path():
    return os.path.join(data_path(), 'shapefiles')


def masks_path():
    # TODO: currently this folder is not used
    return os.path.join(data_path(), 'masks')


def Rsrc_dir():
    return os.path.join(_PATH, 'Rsrc')


def cache_path():
    cache_path = configuration.get_config_value("cache", "cache_path")
    if not cache_path:
        LOGGER.warn("No cache path configured. Using default value.")
        cache_path = os.path.join(configuration.get_config_value("server", "outputpath"), "cache")
    return cache_path


def output_path():
    return configuration.get_config_value("server", "outputpath")


def output_url():
    url = configuration.get_config_value("server", "outputurl")
    if url:
        url = url.rstrip('/')
    return url
