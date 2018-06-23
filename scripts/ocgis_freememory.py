from flyingpigeon import datafetch
ncs = datafetch.reanalyses(start=2000, end=2017)

from ocgis import RequestDataset, OcgOperations, env
from ocgis.util.large_array import compute

from datetime import datetime as dt
import uuid

# years = range(1948,2018)

# ncs = []
# for year in years:
#     url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/pressure/slp.%s.nc' % (year)
#     ncs.extend([utils.download_file(url)])

# print ncs
level_range = [700, 700]
time_range = [dt.strptime('20100315', '%Y%m%d'), dt.strptime('20111210', '%Y%m%d')]
bbox = [-80, 20, 20, 70]

# TODO: BUG: ocg compute is not running if calc == None
calc = '%s=%s*1' % (variable, variable)
#


rd = RequestDataset(ncs)

ops = OcgOperations(rd,
                    time_range=time_range,
                    calc = '%s=%s*1' % ('slp', 'slp'),
                    # level_range=level_range,
                    geom=bbox,
                    output_format='nc',
                    prefix='ocgis_module_optimisation',
                    add_auxiliary_files=False)

# ###################################
# check free memory available somehow
from eggshell import util_functions as ufs
free_memory = ufs.FreeMemory(unit='MB')

# ###########################
# check required memory space

data_kb = ops.get_base_request_size()['total']
data_mb = data_kb / 1024.

# ###########################
# check if half of the available memory can take the required data load

if data_mb < fm.user_free/2:
    print "enough memory. data can be processed directly"
    shnip = dt.now()

    geom = ops.execute()

    shnap = dt.now()

    print 'operation performed with execute in %s sec' % (shnap - shnip).total_seconds()
    print geom

########################
# simulation if memory is not enough for the dataload. Than calculate in chunks

fm_sim = data_mb/2

if data_mb >= fm_sim:
    print "NOT enough memory. data will be processed in chunks"
    # calculate tile dimension:
    tile_dimension= 10 # TODO: needs to be calculated based on dataload and available memory

    shnip = dt.now()
    geom = compute(ops, tile_dimension=tile_dimension, verbose=True)
    shnap = dt.now()

    print 'operation performed with compute in  %s sec' % (shnap - shnip ).total_seconds()
    print geom
