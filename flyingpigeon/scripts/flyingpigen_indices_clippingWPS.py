"""Python WPS execute"""

from owslib.wps import WebProcessingService, monitorExecution
from os import system

wps = WebProcessingService(url="http://localhost:8093/wps", verbose=False)

print wps.identification.title

for process in wps.processes:
    print '%s : \t %s' % (process.identifier, process.abstract)

execute = wps.execute(
    identifier="indices_clipping", #indices_clipping",
    inputs=[
       ("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc"),
       ("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19910101-19951231.nc"),
       ("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19960101-20001231.nc"),
       #("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc"),
       #("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_sem_200101-200510.nc"),
       #("resource","http://localhost:8090/wpscache/tas_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19910101-19951231.nc"),
       #("resource","http://localhost:8090/wpscache/tas_EUR-11_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19860101-19901231.nc"),
       #("resource","http://localhost:8090/wpscache/tas_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc"),
       ("indice" , "TG"),
       ("region","DEU")
       ])
    
#output= [('out_fieldmeans', True),('out_polygons', True),('out_tas', True)]   
# check process if completed ...
monitorExecution(execute, sleepSecs=5)

print execute.getStatus()

for o in execute.processOutputs:
    print o.reference
