import numpy as np
import numpy.random as R
import matplotlib.pyplot as plt
  
#from statsmodels.sandbox.gam import AdditiveModel
from statsmodels.sandbox.gam import Model as GAM #?
from statsmodels.genmod.families import family
#from statsmodels.genmod.generalized_linear_model import GLM

standardize = lambda x: (x - x.mean()) / x.std()
demean = lambda x: (x - x.mean())  
normalize = lambda x: (x - x.mean()) / x.std()
nobs = 150

x1 = R.standard_normal(nobs)
x1.sort()
x2 = R.standard_normal(nobs)
x2.sort()
y = R.standard_normal((nobs,))
  
d = np.array([x1,x2]).T
  
import scipy.stats, time
  
print("binomial")
f = family.Binomial()

# b = np.asarray([scipy.stats.bernoulli.rvs(p) for p in f.link.inverse(y)])
# b.shape = y.shape
b = np.zeros(len(x1))
b[x1>0.5] = 1

m = GAM(b, d, family=f)
toc = time.time()
m.fit(b)
tic = time.time()
print(tic-toc)

  
plt.figure()
plt.plot(x1, standardize(m.smoothers[0](x1)), 'r')
#plt.plot(x1, standardize(f1(x1)), linewidth=2)
#plt.figure()
plt.plot(x2, standardize(m.smoothers[0](x2)), 'r')
#plt.plot(x2, standardize(f2(x2)), linewidth=2)
  
plt.show()
  