from flyingpigeon import visualisation as vs

signal = '/home/nils/birdhouse/var/lib/pywps/outputs/flyingpigeon/output_signal-12534cad-fdd6-11e6-9c13-9cb6d0d3acd7.nc'
high = '/home/nils/birdhouse/var/lib/pywps/outputs/flyingpigeon/output_high-12534cad-fdd6-11e6-9c13-9cb6d0d3acd7.nc'
low = '/home/nils/birdhouse/var/lib/pywps/outputs/flyingpigeon/output_low-12534cad-fdd6-11e6-9c13-9cb6d0d3acd7.nc'

graphic = vs.map_robustness(signal, high, low)

print(graphic)
