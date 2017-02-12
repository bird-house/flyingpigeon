from matplotlib import pyplot as plt
from flyingpigeon import sdm

csvfile = '/home/nils/data/sdm/output_csv-c68f5b56-ca90-11e6-96bb-868dacf6ed58.csv'
nc = '/home/nils/data/sdm/SU_EUR-44_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_yr_20110101-20151231.nc'

coords = sdm.latlon_gbifcsv(csvfile)

mask = sdm.get_PAmask(coordinates=coords, nc=nc)

plt.contourf(mask)

plt.show()
