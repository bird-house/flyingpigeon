from flyingpigeon import visualisation as vs

signal = '/home/nils/data/robustness/output_signal-73f2f562-fdcb-11e6-9c13-9cb6d0d3acd7.nc'
high = '/home/nils/data/robustness/output_high-73f2f562-fdcb-11e6-9c13-9cb6d0d3acd7.nc'
low = '/home/nils/data/robustness/output_low-73f2f562-fdcb-11e6-9c13-9cb6d0d3acd7.nc'

graphic = vs.map_robustness(signal, high, low)

print(graphic)
