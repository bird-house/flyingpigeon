""" processing of timeseries """

from cdo import *   # python version
cdo = Cdo()

def fldmean(resource, dir_output = None ):
  from os.path import join, basename , curdir
  from tempfile import mkdtemp , mkstemp
  """
  retuns a field mean over the whole domain
  gives a statisitcs timeseries of an given polygon"""  
  
  if dir_output == None: 
    dir_output = mkdtemp(dir= curdir)
  
  if resource == str:
    p, output = mkstemp(dir= dir_output, suffix='.nc')
    cdo.fldmean(input = resource , output = output)
  elif resource == list:   
    output = []
    for rs in resource: 
      p, op = mkstemp(dir= dir_output, suffix='.nc')
      cdo.fldmean(input = rs , output = op)
      output.append(op)
  return output

def add_statistic(nc_url, var):
    """ add statistic variables to a given netCDF file timeseries for a given variable. 
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
    
    showyear = cdo.showyear(input = nc_url )
    years = [int(i) for i in showyear[0].split(' ')]

    ref_periods = []
    ref_stds = []
    ref_medians = []
    ref_means = []

    for start in years[:-30]:
        end = start + 31
        ref_periods.append(start)
        ref_stds.append(hs[str(start):str(end)].std())
        ref_medians.append(hs[str(start):str(end)].median())
        ref_means.append(hs[str(start):str(end)].mean())
    

    nc.createDimension('ref_period', len(ref_periods))
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

    ref_period[:] = ref_periods
    ref_median[:] = ref_medians
    ref_mean[:] = ref_means
    ref_std[:] = ref_stds

    nc.close()
    
def merge(resource, dir_output=None, historical_concatination=False): 
  """
  returns list of paths of sorted and merged netCDF files.
  sort according to filename (in DSR convention) and merge appropriate netCDF files
  
  :param resource: list of netCDF file pathes equal domain and DSR name convention is required for merging
  :param historical_concatination : concatination of historical files to rcp szenarios (default = False)
  ;param dir_output : path to directory for output 
  """
  
  from os.path import curdir, basename, join
  from os import rename
  #from tempfile import mkdtemp
  import utils
  from ocgis import RequestDataset , OcgOperations
  
  #if type(resource) == list: 
    #resource = {'merge':resource}
    
  res_dic = utils.sort_by_filename(resource, historical_concatination = historical_concatination)
  merged_files = []
  
  if dir_output == None:
    dir_output = curdir
  
  for key in res_dic:
    if len(res_dic[key]) > 1: 
      ncs = res_dic[key]
      var = key.split('_')[0]
      rd = RequestDataset(uri=ncs, variable=var )
      ops = OcgOperations( dataset=rd, prefix = key, output_format='nc', dir_output = dir_output, add_auxiliary_files=False)
      m_file = ops.execute()
      var = key.split('_')[0]
      merged_files.append(utils.drs_filename(m_file, variable=var))
    else:
      bn = basename(res_dic[key][0])
      newname = str(join(dir_output, bn))
      rename(bn,newname)
      merged_files.append(newname)
  return merged_files



def get_yearmean(resources, variable=None, dir_output = None): 
  """
  returns a list of file pathes 
  
  :param resources: str or list of input file pathes
  """
  from flyingpigeon import utils
  import ocgis 
 
  if type(resources) == str :
    resources = list([resources])
 
  rs = []
  for nc in resources: 
    rs.append(utils.drs_filename(nc, variable=None, rename_file=True, 
                                 add_file_path=True))
  ncs = utils.sort_by_filename(rs)
  
  if variable==None: 
    name = 'mean'
  else: 
    name = variable
  calc = [{'func':'mean','name':name}]
  calc_grouping = ['year']
  
  year_means = []
  
  for prefix in ncs: 
    rd = ocgis.RequestDataset(ncs[prefix], variable=variable)
    f = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping, 
                                    prefix=prefix, output_format='nc',dir_output=dir_output)
    year_means.append(f.execute())
  # sort files
  # aggregate mean years
  
  return ncs # resources # ncs # year_means