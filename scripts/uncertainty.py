from matplotlib import pyplot as plt
import pandas as pd
import numpy as np
import netCDF4
from os.path import basename

url=['/home/nils/data/segetalflora/DEU/sfextensive3/sfextensive3_EUR-11_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_SMHI-RCA4_v1_yr_20060701-21000701_DEU.nc','/home/nils/data/segetalflora/DEU/sfextensive3/sfextensive3_EUR-11_CNRM-CERFACS-CNRM-CM5_rcp85_r1i1p1_SMHI-RCA4_v1_yr_19700701-21000701_DEU.nc']

vname = 'sfextensive3'


df = pd.DataFrame()

for f in url: 
  ds = netCDF4.Dataset(f)
  data = np.squeeze(ds.variables[vname][:])
  if len(data.shape) == 3: 
    meanData = np.mean(data,axis=1)
    ts = np.mean(meanData,axis=1)
  else: 
    ts = data[:]

  times = ds.variables['time']
  jd = netCDF4.num2date(times[:],times.units)
  hs = pd.Series(ts,index=jd, name=basename(f))
  df[basename(f)] = hs

rollmean = df.rolling(window=30,center=True).mean()
rollmean.plot(title='test', legend=False)
plt.show()

q50 = df.quantile([0.5], axis=1)
q05 = df.quantile([0.05], axis=1)
q95 = df.quantile([0.95], axis=1)

plt.plot(rollmean.index.values, np.squeeze( q05.values))
plt.plot(rollmean.index.values, np.squeeze( q95.values))
plt.plot(rollmean.index.values, np.squeeze( q50.values))

plt.show()