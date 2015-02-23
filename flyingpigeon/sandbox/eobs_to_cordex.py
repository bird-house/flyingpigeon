import ocgis
from cdo import * 
cdo = Cdo()

nc = '/home/nils//anaconda/var/cache/pywps/tx_0.22deg_rot_2014.nc'
nc2 = '/home/nils//anaconda/var/cache/pywps/tx_0.22deg_rot_v10.0.nc'

rd = ocgis.RequestDataset([nc,nc2], 'tx', conform_units_to='K')

ocgis.env.OVERWRITE=True

geom_file = ocgis.OcgOperations(dataset= rd, output_format='nc', dir_output= '/home/nils/data/EOBS/', add_auxiliary_files=False).execute()
# print(geom_file)

cdo.setreftime('1949-12-01,00:00:00,days', input=geom_file, output='/home/nils/data/EOBS/tx_0.22deg_rot_2014_Cordex.nc')
cdo.setname('tasmax', input='/home/nils/data/EOBS/tx_0.22deg_rot_2014_Cordex.nc' , output='/home/nils/data/EOBS/tasmax_EOBS-22_2014.nc')