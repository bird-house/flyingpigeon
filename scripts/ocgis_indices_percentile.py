
from os import listdir
from os import path

from ocgis import RequestDataset, OcgOperations
from ocgis.contrib import library_icclim as lic

from datetime import datetime as dt

from flyingpigeon.ocgis_module import call
from flyingpigeon.utils import get_values, get_time
from numpy import ma
import uuid

p = '/home/nils/birdhouse/var/lib/pywps/cache/malleefowl/esgf1.dkrz.de/thredds/fileServer/cordex/cordex/output/AFR-44/MPI-CSC/MPI-M-MPI-ESM-LR/historical/r1i1p1/MPI-CSC-REMO2009/v1/day/tas/v20160412/'

resource = [path.join(p, nc) for nc in listdir(p)]
resource.sort()
# rd = RequestDataset(ncs[0])
indice = 'TG'
percentile = 90
var = 'tas'
window_width = 5

dt1 = dt(1980, 01, 01)
dt2 = dt(1989, 12, 31)
refperiod = [dt1, dt2]  # we will calculate the indice for 10 years

# rd = RequestDataset(ncs, 'tas', time_range=time_range_indice)
# basis_indice = rd.get()  # OCGIS data object
#
#
# dt1_ref = dt(1971, 01, 01)
# dt2_ref = dt(2000, 12, 31)
# time_range_ref = [dt1_ref, dt2_ref]
# rd_ref = RequestDataset(ncs, 'tas', time_range=time_range_ref)
# basis_ref = rd_ref.get()  # OCGIS data object


nc_reference = call(resource=resource,
                    prefix=str(uuid.uuid4()),
                    time_range=refperiod,
                    output_format='nc')

arr = get_values(resource=nc_reference)
dt_arr = get_time(resource=nc_reference)
arr = ma.masked_array(arr)
dt_arr = ma.masked_array(dt_arr)


################################
# load the appropriate operation
################################

operation = None
if 'T' in indice:
    if percentile >= 50:
        operation = 'Icclim%s90p' % var
        func = 'icclim_%s90p' % var  # icclim_TG90p
    else:
        operation = 'Icclim%s10p' % var
        func = 'icclim_%s10p' % var


ops = [op for op in dir(lic) if operation in op]
if len(ops) == 0:
    raise Exception("operator does not exist %s", operation)

calendar = 'standard'
units = 'days since 0001-01-01 00:00'

exec "percentile_dict = lic.%s.get_percentile_dict(arr, dt_arr, percentile, window_width, calendar, \
      units, only_leap_years=self.only_leap_years)" % ops[0]
calc = [{'func': func, 'name': name, 'kwds': {'percentile_dict': percentile_dict}}]


(value, self.field.temporal.value_datetime, self.percentile,
                                       self.window_width, self.field.temporal.calendar,
                                       self.field.temporal.units, only_leap_years=self.only_leap_years)

values_ref = basis_ref.variables['tas'].value
temporal = basis_ref.temporal.value_datetime
percentile = 10
width = 5 # 5-day window
from ocgis.calc.library.index.dynamic_kernel_percentile import DynamicDailyKernelPercentileThreshold
daily_percentile = DynamicDailyKernelPercentileThreshold.get_daily_percentile(values_ref,temporal,percentile,width)

# geom = OcgOperations(rd,
#                      calc=[{'func': 'icclim_' + indice, 'name': indice}],
#                      calc_grouping=['year', 'month'],
#                      prefix='single_file',
#                      output_format='nc').execute()
# print geom
#
# rd = RequestDataset(ncs)
# indice = 'TG'
# geom = OcgOperations(rd,
#                      calc=[{'func': 'icclim_' + indice, 'name': indice}],
#                      calc_grouping=['year', 'month'],
#                      prefix='multi_file',
#                      output_format='nc').execute()
# print geom

from flyingpigeon.indices import calc_indice_percetile

fp_indice = calc_indice_simple(resource=ncs, variable=None, prefix=None, indice='TG',
                               polygons=['CMR'], mosaic=True, grouping='yr', dir_output=None,
                               dimension_map=None, memory_limit=None)

print fp_indice
