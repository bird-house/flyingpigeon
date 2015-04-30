""" processing of timeseries """

from cdo import *   # python version
cdo = Cdo()


def add_statistic(nc_url, var):
    """ add statistic variables to a given netCDF file for a given variable. 
    The statisitcs is calculated as a running 30 Year moving time window"""
 
    import pandas as pd
    import netCDF4
    from numpy import squeeze
    import numpy as np 

    nc = netCDF4.Dataset(nc_url, mode="a")
    h =  squeeze(nc.variables[var])
    times = nc.variables['time']
    jd = netCDF4.num2date(times[:],times.units)
    hs = pd.Series(h,index=jd)
    
    showyear = cdo.showyear(input = file )
    years = [int(i) for i in showyear[0].split(' ')]

    nc.createDimension('ref_period', len(period))
    
    ref_period = nc.createVariable(varname= 'ref_period', datatype = 'i', dimensions='ref_period')
    ref_median = nc.createVariable(varname= 'ref_median', datatype = 'float', dimensions='ref_period')
    ref_mean = nc.createVariable(varname= 'ref_mean', datatype = 'float', dimensions='ref_period')
    ref_std = nc.createVariable(varname= 'ref_std', datatype = 'float', dimensions='ref_period')

    ref_period_att = {'standard_name':'ref_period',
                  'long_name':'start year of reference period',
                  'units':'year' }
    ref_median_att = {'standard_name':'ref_median',
                  'long_name':'median for reference period'}
    ref_mean_att = {'standard_name':'ref_mean',
                  'long_name':'mean for reference period'}
    ref_std_att = {'standard_name':'ref_std',
                  'long_name':'standart deviation for reference period'}


    ref_period.setncatts(ref_period_att)
    ref_std.setncatts(ref_std_att)
    ref_mean.setncatts(ref_mean_att)
    ref_median.setncatts(ref_median_att)


    ref_periods = []
    ref_stds = []
    ref_medians = []
    ref_means = []

    for start in years[:-29]:
        end = start + 30
        ref_periods.append(start)
        ref_stds.append(hs[str(start):str(end)].std())
        ref_medians.append(hs[str(start):str(end)].median())
        ref_means.append(hs[str(start):str(end)].mean())


    ref_period[:] = ref_periods
    ref_median[:] = ref_medians
    ref_mean[:] = ref_means
    ref_std[:] = ref_stds

    nc.close()
