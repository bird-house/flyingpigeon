import os
from pywps import config as wpsconfig

import logging
logger = logging.getLogger(__name__)


def shapefiles_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'shapefiles')


def Rsrc_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'Rsrc')


def JSsrc_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'JSsrc')


def masks_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'masks')


def static_dir():
    return os.path.join(os.path.abspath(os.path.dirname(__file__)), 'static')


def cache_path():
    cache_path = None
    try:
        cache_path = wpsconfig.getConfigValue("cache", "cache_path")
    except:
        logger.warn("No cache path configured. Using default value.")
        cache_path = os.path.join(os.sep, "tmp", "cache")
    return cache_path


def output_path():
    try:
        output_path = wpsconfig.getConfigValue("server", "outputPath")
    except:
        output_path = None
        logger.warn('no output path configured')
    return output_path


def outputUrl_path():
    try:
        outputUrl = wpsconfig.getConfigValue("server", "outputUrl")
    except:
        outputUrl = None
        logger.warn('no outputUrl configured')
    return outputUrl


def www_url():
    try:
        url = wpsconfig.getConfigValue("flyingpigeon", "www_url")
    except:
        url = None
        logger.warn('no www-url configured')
    return url
