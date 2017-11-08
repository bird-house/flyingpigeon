from owslib.wms import WebMapService
import matplotlib.pyplot as plt
from pyproj import Proj, transform
import json

instance_id = ...

# in meters
region_size = (2000, 2000)

outProj = Proj(init='epsg:3857') # https://epsg.io/3857

inProj = Proj(init='epsg:4326') # https://epsg.io/4326

arizona_crater=(-111.02278,35.02722)

loc = arizona_crater

x, y = transform(inProj,outProj,loc[0],loc[1])

xupper = int(round(x - region_size[0] / 2))
xlower = int(round(x + region_size[0] / 2))
yupper = int(round(y - region_size[1] / 2))
ylower = int(round(y + region_size[1] / 2))

bbox = (xupper, yupper, xlower, ylower)
#bbox=(-11375363,3757392,-11372226,3759102)
size=region_size
#size=(1333, 716)

#wms_version = '1.1.1'
wms_version = '1.3.0'

wms = WebMapService('http://services.sentinel-hub.com/v1/wms/' + instance_id, version=wms_version)

print 'Type: ', wms.identification.type
print 'Title: ', wms.identification.title
print 'Contents: ', list(wms.contents)
print 'Operations: ',[op.name for op in wms.operations]
#print 'Capabilities: ', wms.getcapabilities()

info = wms.getfeatureinfo(
    layers=['1_NATURAL_COL0R'],
    srs='EPSG:3857',
    bbox = bbox,
    size = size,
    info_format='application/json',
    time='2015-01-01/2017-06-01/P1D',
    xy = (0, 0)
)
info = json.loads(info.read())
