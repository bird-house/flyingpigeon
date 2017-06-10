from os import path
from flyingpigeon.ocgis_module import call
from shutil import move


url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/%s.%s.nc' % (variable, year)


move(df, f)
conv = call(resource=f, output_format_options={'data_model': 'NETCDF4_CLASSIC'}, dir_output=p, prefix='converted_netcdf')
