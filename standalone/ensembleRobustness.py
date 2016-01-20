from flyingpigeon import ensembleRobustness as erob

files = []

for i in range(1,3): # 16
  files.append('/home/estimr1/EUCLEIA/indices/RX5day/DJF/RX5day_DJF_HadGEM3-A-N216_historical_r1i1p%s_19600101-20131230.nc' % (i))


signal , mask = erob.worker(resource=files, start=None, end=None, timeslice=10)


print signal 
print mask