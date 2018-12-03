import os
import tempfile
from pywps import configuration

_PATH = os.path.abspath(os.path.dirname(__file__))


import logging
LOGGER = logging.getLogger("PYWPS")

def shapefiles_path():
    return os.path.join(data_path(), 'shapefiles')


# def data_path():
#     return os.path.join(_PATH, 'data')
#
#
# def masks_path():
#     # TODO: currently this folder is not used
#     return os.path.join(data_path(), 'masks')
#
#
# def static_path():
#     return os.path.join(_PATH, 'static')

# Path to store test files for later inspection. Defined only once to avoid creating multiple test directories.
# test_output_path = tempfile.mkdtemp(prefix='fptest_', dir=output_path())
