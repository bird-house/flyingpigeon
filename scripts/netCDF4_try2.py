from flyingpigeon.utils import download
from ocgis import RequestDataset, OcgOperations

variable = 'slp'
year = 2000

url = 'https://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/%s.%s.nc' % (variable, year)

f = download(url)
rd = RequestDataset(f)
ds = OcgOperations(rd,output_format_options={'data_model': 'NETCDF4_CLASSIC'},prefix='converted_to_NETCDF_CLASSIC' ).execute()
ds = OcgOperations(rd,output_format_options={'data_model': 'NETCDF4_CLASSIC'}, output_format = 'nc', prefix='converted_to_NETCDF_CLASSIC' ).execute()
print ds
