# -*- coding: utf-8 -*-

"""
This module is used to manage optional dependencies.

Example usage::

    from .dependencies import netCDF4 as nc
"""

import warnings


try:
    import netCDF4
except ImportError:
    netCDF4 = None
    warnings.warn('netCDF4 is not available.')
