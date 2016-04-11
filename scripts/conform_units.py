from ocgis import OcgOperations, RequestDataset
import wget

url = 'http://www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/slp.2000.nc'

wget.download(url)

rd =  RequestDataset('slp.2000.nc')

rd.inspect()

#slp:actual_range = "[  93680.  110762.]" ;
#slp:valid_range = "[  87000.  115000.]" 

# ops = OcgOperations(rd, geom=[20,20,30,30], conform_units_to='hPa', time_region={'month':[12,1]}, prefix='hPa', output_format='nc').execute()
ops = OcgOperations(rd, geom=[20,20,30,30], conform_units_to='hectopascals', time_region={'month':[12,1]}, prefix='hPa', output_format='nc').execute()


rd_subset = RequestDataset(ops)
rd_subset.inspect()

# slp:actual_range = "[  93680.  110762.]" ;
# slp:valid_range = "[  87000.  115000.]" ;
 
