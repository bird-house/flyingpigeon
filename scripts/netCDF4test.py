import wget
resource = []
for year in range(2008, 2010):
  url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.%s.nc' % year
  resource.append(wget.download(url))

from ocgis import RequestDataset, OcgOperations
rd = RequestDataset(resource[0])
print rd.inspect() # works fine

# print '**********************'
# rd = RequestDataset(resource)
# print rd.inspect() 