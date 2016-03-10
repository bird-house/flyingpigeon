import numpy as np
#import numpy.random as R
import matplotlib.pyplot as plt

#from statsmodels.sandbox import gam  #?
#from statsmodels.genmod.families import family

nobs = [1000]
x1 = np.random.normal(20, 10, 1000) 
x2 = x1 / 2 + np.random.normal(2, 1, 1000)

PA = x1
PA[x1<15] = 0
PA[x1>25] = 0
PA[PA>0] = 1

x1 = x1 + np.random.normal(3, 1, 1000)

from rpy2.robjects.packages import importr
#from rpy2.robjects import DataFrame
import rpy2.robjects as ro

import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

grdevices = importr('grDevices')
grdevices.png(file="path/to/file.png", width=512, height=512)
# plotting code here
grdevices.dev_off()

mgcv = importr("mgcv")
stats = importr("stats")
base = importr("base")
ds = importr('datasets')
utils = importr('utils')

d = {'PA': ro.IntVector(PA), 'x1': ro.FloatVector(x1),'x2': ro.FloatVector(x2) }
dataf = ro.DataFrame(d)

eq = ro.Formula("PA ~ s(x1, k=3) + s(x2, k=3)")

# from rpy2.rinterface import ListSexpVector
#
trans = ro.Formula('x ~ exp(x)/(1+exp(x))')

trained_model = mgcv.gam(base.eval(eq), data=dataf, family=stats.binomial(), scale=-1)

for i in [1,2]:    
#ylim = ro.IntVector([-6,6])
  mgcv.plot_gam(trained_model, shade='T', col='black',select=i,ylab='Predicted Probability',rug=False , cex_lab = 1.4, cex_axis = 1.4, ) #ylim=ylim,  trans=base.eval(trans),
  raw_input("Press Enter to continue...")
  grdevices.dev_off()
  

from netCDF4 import Dataset

nc = '/home/nils/.conda/envs/birdhouse/etc/pywps/TN_1_EUR-11_ICHEC-EC-EARTH_historical_r1i1p1_KNMI-RACMO22E_v1_day_19960101-20000101.nc'

ds = Dataset(nc, mode='r')
TN_1 = ds.variables['TN_1']  

dims = np.squeeze(TN_1).shape
x1 = np.ravel(TN_1) - 255 

x2 = x1 / 2 + np.random.normal(2, 1, len(x1)) 

d2 = { 'x1': x1,'x2': x2}

df = ro.DataFrame(d2)
# rstack = utils.stack(df)

p = mgcv.predict_gam(trained_model, newdata=df, type="response", progress="text", na_rm=True)
  
p2d = np.array(p)
p2d.resize(dims)