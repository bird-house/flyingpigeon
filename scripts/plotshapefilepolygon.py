import shapefile as shp
import matplotlib.pyplot as plt
from os.path import join

from flyingpigeon import config
from flyingpigeon.subset import get_ugid
DIR_SHP = config.shapefiles_dir()

sf = shp.Reader(join(DIR_SHP, "countries.shp"))

ugid = get_ugid(polygons='DEU', geom='countries')
print ugid

plt.figure()
for c, shape in enumerate(sf.shapeRecords()):
    if c in ugid:
        x = [i[0] for i in shape.shape.points[:]]
        y = [i[1] for i in shape.shape.points[:]]
        plt.plot(x, y)
plt.show()
