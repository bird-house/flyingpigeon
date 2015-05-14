"""Python WPS execute"""

from owslib.wps import WebProcessingService, monitorExecution
from os import system

wps = WebProcessingService(url="http://localhost:8093/wps", verbose=False)

execute = wps.execute(
    identifier="segetalflora",
    inputs=[
    # ("netcdf_file","http://localhost:8090/wpscache/tas_EUR-11_CNRM-CERFACS-CNRM-CM5_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_sem_200101-200510.nc"),
    ("netcdf_file","http://localhost:8090/wpscache/tas_EUR-11_ICHEC-EC-EARTH_rcp85_r1i1p1_KNMI-RACMO22E_v1_day_20210101-20251231.nc"),
    ],
#    climate_type = 2, 
#    culture_type = ['fallow']
    )
#output= [('out_fieldmeans', True),('out_polygons', True),('out_tas', True)]   
# check process if completed ...
monitorExecution(execute, sleepSecs=5)

execute.getStatus()

for o in execute.processOutputs:
    print o.reference

print '\n'
    
call =  'curl %s' % (execute.processOutputs[2].reference)
system(call)