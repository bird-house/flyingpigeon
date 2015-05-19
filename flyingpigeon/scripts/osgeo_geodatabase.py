import os
from osgeo import ogr
from os import environ
import sys

gdb = '/home/nils/data/shapefiles/UTM_1km_Polyline_Shapefile_Geodatabase/UTM_1km_Polyline_Shapefile_Geodatabase.gdb/'


# import OGR
# use OGR specific exceptions
ogr.UseExceptions()

# get the driver
driver = ogr.GetDriverByName("OpenFileGDB")

# opening the FileGDB
try:
    gdb = driver.Open(gdb, 0)
except Exception, e:
    print e
    sys.exit()

# list to store layers'names
featsClassList = []

# parsing layers by index
for featsClass_idx in range(gdb.GetLayerCount()):
    featsClass = gdb.GetLayerByIndex(featsClass_idx)
    featsClassList.append(featsClass.GetName())

# sorting
featsClassList.sort()

# printing
for featsClass in featsClassList:
    print featsClass
    
# clean close
del gdb
