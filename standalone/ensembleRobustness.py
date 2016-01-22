from flyingpigeon import ensembleRobustness as erob

files = []

for i in range(1,3): # 16
  files.append('/home/estimr1/EUCLEIA/indices/RX5day/DJF/RX5day_DJF_HadGEM3-A-N216_historical_r1i1p%s_19600101-20131230.nc' % (i))

signal, high_agreement_mask, low_agreement_mask = erob.worker(resource=files, start=None, end=None, timeslice=10)

print signal 
print high_agreement_mask
print low_agreement_mask


from flyingpigeon.visualisation import map_ensembleRobustness
#from flyingpigeon.utils import get_variable
    
#variable = get_variable(signal)
graphic = map_ensembleRobustness(signal, high_agreement_mask, low_agreement_mask, 
              variable='RX5day_DJF', 
              cmap='seismic', 
              title='Change of Sigal')