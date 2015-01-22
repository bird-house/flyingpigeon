import nose.tools
from nose import SkipTest
from nose.plugins.attrib import attr

from malleefowl import wpsclient

from __init__ import SERVICE, TESTDATA

def setup():
    pass

@attr('online')   
def test_indices():
    result = wpsclient.execute(
        service = SERVICE,
        identifier = "indice",
        inputs = [('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_rcp85_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20510101-20551231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_20010101-20051231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19960101-20001231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19910101-19951231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19860101-19901231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19810101-19851231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19760101-19801231.nc']),
                  ('netcdf_file', TESTDATA['tasmax_EUR-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_CLMcom-CCLM4-8-17_v1_day_19710101-19751231.nc']),
                  
                  ('domain','EUR'),
                  ('anomalies','True'),
                  ('group','mon'),
                  ('ID','True'),
                  # ('CSU','True'),
                  #('TG','True'),('TX','True'),('TXx','True'), ('TXn','True'), ('TN','True'), ('TNx','True'), ('TNn','True'),  ('FD','True'),('CFD','True'),('TR','True'), , ('HD17','True'), ('GD4','True'), ('RR','True'), ('RR1','True'),('CWD','True'), ('SDII','True'), ('R10mm','True'), ('R20mm','True'),('RX1day','True'), ('RX5day','True'), ('SD','True'), ('SD1','True'),('SD5cm','True'), ('SD50cm','True'), ('CDD','True')
                  
                  ],
        outputs = [('logout', True),('tarout', True)], # ('cvout', True),
        verbose = False,
        sleep_secs=2
                  )

    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)
    nose.tools.ok_(len(result['processOutputs']) == 2, result)
    #nose.tools.ok_(False, result)

    #nose.tools.ok_('txt' in result[0]['reference'], result)
    #content = urllib.urlopen(result[0]['reference']).read()
    #nose.tools.ok_(not ('failed' in content), content)
    
