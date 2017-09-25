import ocgis
from shapely.ops import cascaded_union

from flyingpigeon.config import shapefiles_path
from os.path import join
from os import listdir

p = '/home/nils/birdhouse/var/lib/pywps/cache/flyingpigeon/www.esrl.noaa.gov/psd/thredds/fileServer/Datasets/ncep.reanalysis.dailyavgs/surface/'

PATH_NC = [join(p, nc) for nc in listdir(p)]

PATH_NC.sort()

SH_PATH = shapefiles_path()
GEOM_PATH = join(SH_PATH, 'countries.shp')
# GEOM_PATH = '/home/benkoziol/l/project/ocg/bin/shp/world_countries/world_countries.shp'
ocgis.env.DIR_OUTPUT = '/home/nils/data/'
ocgis.env.OVERWRITE = True

rd = ocgis.RequestDataset(PATH_NC)

# We do not want to update the coordinate system from WGS84 to Spherical. This causes issues with wrapping for unknown
# reasons. We also need to union the geometries to remove other inconsistencies requiring the unwrapping occur outside
# the operations call.
gc = ocgis.GeomCabinetIterator(path=GEOM_PATH, select_sql_where="ADMIN = 'Russia'", as_field=True)

subset_field = list(gc)[0]

shapely_geom = subset_field.geom.get_value()[0]
wrapper = ocgis.spatial.wrap.GeometryWrapper()
unwrapped = wrapper.unwrap(shapely_geom)
subset_field.geom.get_value()[0] = cascaded_union([g for g in unwrapped])
# subset_field.set_crs(rd.crs)
subset_field.set_crs(ocgis.crs.Spherical())

# Subset the netCDF file and write to disk.
ops = ocgis.OcgOperations(dataset=rd, geom=subset_field, snippet=True, output_format='nc', prefix='ocgis-russia-subset')
ret = ops.execute()

# Convert the subset netCDF file to ESRI shapefile.
ops2 = ocgis.OcgOperations(dataset={'uri': ret}, output_format='shp', vector_wrap=False, prefix='ocgis-as-shp')
ops2.execute()

# Write the original model grid to netCDF file.
ops3 = ocgis.OcgOperations(dataset=rd, output_format='shp', vector_wrap=False, snippet=True, prefix='original-grid')
ops3.execute()
