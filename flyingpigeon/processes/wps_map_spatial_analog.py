from flyingpigeon.log import init_process_logger
from flyingpigeon.utils import archiveextract
from flyingpigeon.utils import rename_complexinputs
from flyingpigeon.utils import get_values
from flyingpigeon.ocgis_module import call
from shapely.geometry import Point
import netCDF4 as nc

from pywps import Process
from pywps import LiteralInput
from pywps import ComplexInput, ComplexOutput
from pywps import Format
from pywps.app.Common import Metadata

from datetime import datetime as dt
import os

from ocgis import FunctionRegistry, RequestDataset, OcgOperations
from flyingpigeon.ocgisDissimilarity import Dissimilarity, metrics

FunctionRegistry.append(Dissimilarity)

import logging
LOGGER = logging.getLogger("PYWPS")


class MapSpatialAnalogProcess(Process):
    def __init__(self):
        inputs = [
            ComplexInput('resource', 'netCDF dataset',
                         abstract="Dissimilarity between target at selected "
                                   "location and candidate distributions over the entire grid.",,
                         metadata=[Metadata('Info')],
                         min_occurs=1,
                         max_occurs=1,
                         supported_formats=[
                             Format('application/x-netcdf'),
                             Format('application/x-tar'),
                             Format('application/zip'),
                         ]),
            LiteralInput('fmt', 'format',
                         abstract="Figure format",
                         min_occurs=0,
                         max_occurs=5,
                         allowed_values=['png', 'pdf'])
        ]
