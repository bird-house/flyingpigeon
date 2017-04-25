from os import listdir
from os import path

from ocgis import RequestDataset, OcgOperations, env

env.OVERWRITE = True

p = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/'
nc = [path.join(p, f) for f in listdir(p)]
nc.sort()


output_format_options = {'data_model': 'NETCDF4_CLASSIC'}

rs = RequestDataset(nc[0])
ocg = OcgOperations(rs, geom=[-80, 20, 40, 50], output_format='nc', output_format_options=output_format_options, prefix='single_file').execute()
print 'single file: %s ' % ocg
print '***********************'

rs = RequestDataset(nc)
ocg = OcgOperations(rs, geom=[-80, 20, 40, 50], output_format='nc', prefix='multiple_files',
                    output_format_options=output_format_options).execute()
print 'multiple files : %s ' % ocg
print '***********************'
