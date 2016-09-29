import tempfile
import ocgis
import os


# global model grid with ~3 degree resolution
URI1 = os.path.expanduser('/homel/nhempel/data/tests/slp.2000.nc')
# downscaled model grid covering the conterminous United States with ~1/8 degree resolution
URI2 = os.path.expanduser('/homel/nhempel/data/tests/output_netcdf-e49f2d78-8563-11e6-bf14-fbeae168c26e.nc')
ocgis.env.DIR_OUTPUT = tempfile.gettempdir()


########################################################################################################################
# simple regridding example with a bounding box subset writing to netCDF using conservative regridding.
########################################################################################################################

#bbox = [-104, 36, -95, 44]
# regrid the global dataset to the downscaled grid
rd_global = ocgis.RequestDataset(uri=URI1)
rd_downscaled = ocgis.RequestDataset(uri=URI2)
ops = ocgis.OcgOperations(dataset=rd_global, regrid_destination=rd_downscaled,  output_format='nc', #geom=bbox,
                          prefix='with_corners')
ret = ops.execute()
print ret

########################################################################################################################
# regrid using bilinear interpolation (i.e. without corners)
########################################################################################################################

rd_global = ocgis.RequestDataset(uri=URI1)
rd_downscaled = ocgis.RequestDataset(uri=URI2)
regrid_options = {'with_corners': False}
ops = ocgis.OcgOperations(dataset=rd_global, regrid_destination=rd_downscaled,  output_format='nc', #geom=bbox,
                          regrid_options=regrid_options, prefix='without_corners')
ret = ops.execute()
print ret