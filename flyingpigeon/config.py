"""
Configuration
-------------

WPS servers often need to specify a number of paths for processes to find data, shapefiles, caches and determine where
outputs are stored. To make sure all birds use the same architecture, eggshell provides a :class:`Paths` class to help
with this.
"""

from os.path import join, pardir, abspath
from pywps import configuration
import logging
LOGGER = logging.getLogger("PYWPS")


class Paths(object):
    """This class facilitates the configuration of WPS birds."""
    def __init__(self, module):
        """Instantiate class relative to the given module.

        :param module: Imported module relative to which paths will be defined.
        """
        self._base = module.__path__[0]  # _base is not the top level

    @property
    def top_level(self):
        """ return the top level directory of a WPS bird """
        return abspath(join(self._base, pardir))

    @property
    def data(self):
        """Return the path to the data directory."""
        return join(self._base, 'data')

    @property
    def shapefiles(self):
        """Return the path to the geographic data directory."""
        return join(self.data, 'shapefiles')

    @property
    def testdata(self):
        """Return the path to the test data directory."""
        return join(self.top_level, 'tests/testdata')

    @property
    def cache(self):
        """Return the path to the server cache directory."""
        out = configuration.get_config_value("cache", "cache_path")
        if not out:
            LOGGER.warn("No cache path configured. Using default value.")
            out = join(configuration.get_config_value("server", "outputpath"), "cache")
        return out

    @property
    def outputpath(self):
        """Return the server directory for process outputs."""
        return configuration.get_config_value("server", "outputpath")

    @property
    def outputurl(self):
        """Return the server URL for process outputs."""
        return configuration.get_config_value("server", "outputurl").rstrip('/')

    # @property
    # def masks(self):
    #     """Return the path to the masks directory."""
    #     # TODO: currently this folder is not used
    #     return join(self.data, 'masks')

    # @property
    # def Rsrc_dir(self):
    #     """Return the path to the R source directory."""
    #     return os.path.join(self._base, 'Rsrc')
    #
    # @property
    # def static(self):
    #     """Return the path to the static content directory."""
    #     return os.path.join(self._base, 'static')


# Should these go into the class or they're too specialized ?
def esgfsearch_distrib():
    """TODO"""
    distrib = configuration.get_config_value("extra", "esgfsearch_distrib")
    if distrib is None:
        LOGGER.warn("No ESGF Search distrib option configured. Using default value.")
        distrib = True
    return distrib


def esgfsearch_url():
    """Return the server configuration value for the ESGF search node URL."""
    url = configuration.get_config_value("extra", "esgfsearch_url")
    if not url:
        LOGGER.warn("No ESGF Search URL configured. Using default value.")
        url = 'https://esgf-data.dkrz.de/esg-search'
    return url
