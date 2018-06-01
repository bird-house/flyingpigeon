# Temporary way to link to eggshell content without changing all processes

from flyingpigeon import config
from eggshell.general_utils import archive, archiveextract, check_creationtime, download, FreeMemory, download_file, searchfile, local_path, make_dirs, rename_complexinputs, prepare_static_folder
from eggshell.ocgis_utils import calc_grouping, has_variable, temp_groups
from eggshell.netcdf_utils import get_coordinates, get_values, get_time, get_variable, get_calendar, get_timerange, get_index_lat, get_frequency, get_domain, sort_by_filename, sort_by_time, unrotate_pole, rename_variable
from eggshell.esgf_utils import aggregations, drs_filename, ATTRIBUTE_TO_FACETS_MAP, search_landsea_mask_by_esgf

import os
GROUPING = temp_groups.keys()





