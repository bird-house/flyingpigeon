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
        inputs = [('netcdf_file', TESTDATA['tasmax_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'
          #,'tas_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc', 'tasmin_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc', 'pr_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc', 'prsn_WAS-44_MPI-M-MPI-ESM-LR_historical_r1i1p1_MPI-CSC-REMO2009_v1_day_20010101-20051231.nc'
          ]),
        ('SU','True'),
        ('group','year'),
            ('TG','True'),
            ('TX','True'),
            ('TXx','True'),
            ('TXn','True'),
            ('TN','True'),
            ('TNx','True'),
            ('TNn','True'),
            ('SU','True'),
            ('CSU','True'),
            ('FD','True'),
            ('CFD','True'),
            ('TR','True'),
            ('ID','True'),
            ('HD17','True'),
            ('GD4','True'),
            ('RR','True'),
            ('RR1','True'),
            ('CWD','True'),
            ('SDII','True'),
            ('R10mm','True'),
            ('R20mm','True'),
            ('RX1day','True'),
            ('RX5day','True'),
            ('SD','True'),
            ('SD1','True'),
            ('SD5cm','True'),
            ('SD50cm','True'),
            ('CDD','True')
        ],
        outputs = [('logout', True) , ('ncout', True)],
        verbose = False,
        sleep_secs=2
        )

    nose.tools.ok_(result['status'] == 'ProcessSucceeded', result)
    nose.tools.ok_(len(result['processOutputs']) == 2, result)
    #nose.tools.ok_(False, result)

    #nose.tools.ok_('txt' in result[0]['reference'], result)
    #content = urllib.urlopen(result[0]['reference']).read()
    #nose.tools.ok_(not ('failed' in content), content)
    
