import numpy as np
#import numpy.random as R
import matplotlib.pyplot as plt

from statsmodels.sandbox import gam  #?
from statsmodels.genmod.families import family

nobs = [1000]
#x = np.zeros(3, dtype={'names':['col1', 'col2'], 'formats':['i4','f4']})
x1 = np.random.standard_normal(nobs)
x1.astype({'names':['x1'], 'formats':[np.float]})
x2 = np.random.standard_normal(nobs)
x2.astype({'names':['x2'], 'formats':[np.float]})
#y = np.random.standard_normal(nobs)

PA = np.zeros(nobs)
PA[x1<0.5] = 1

PA.astype({'names':['PA'], 'formats':[np.int]})

d = np.array([PA,x1,x2]).T

import scipy.stats, time
  
print("binomial")
f = family.Binomial()

# b = np.asarray([scipy.stats.bernoulli.rvs(p) for p in f.link.inverse(y)])
# b.shape = y.shape
b = np.zeros(nobs)
b[x2>0.5] = 1

m = gam.Model(b, d, family=f)

from rpy2.robjects.packages import importr
from rpy2.robjects import DataFrame


from rpy2.robjects.packages import importr
import rpy2.robjects as ro
import numpy as np

import rpy2.robjects.numpy2ri
rpy2.robjects.numpy2ri.activate()

mgcv = importr("mgcv")
stats = importr("stats")
base = importr("base")
ds = importr('datasets')

#mtcars = r_ds.__rdata__.fetch('mtcars')['mtcars']
#mgcv.gam(ro.Formula('mpg ~ s(drat) + wt'), data=mtcars)


eq = ro.Formula("PA ~ s(x1) + s(x2)")
trained_model = mgcv.gam(base.eval(eq),data=d, family=stats.binomial(), scale=-1)