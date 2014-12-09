import ocgis
from netCDF4 import Dataset

#from malleefowl import wpslogging as logging
import logging
logger = logging.getLogger(__name__)

def calc_indice(nc_file, indice="SU", variable="tasmax", out_dir=None):
    prefix = variable + '_' + indice
        
    calc_icclim = [{'func' : 'icclim_SU', 'name' : indice}]
    rd = ocgis.RequestDataset(nc_file, variable) # TODO: time_range=[dt1, dt2]
    result = ocgis.OcgOperations(
        dataset=rd,
        calc=calc_icclim,
        calc_grouping=['year'],
        prefix=prefix,
        output_format='nc',
        dir_output=out_dir,
        add_auxiliary_files=False).execute()

    return result

