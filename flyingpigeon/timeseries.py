""" processing of timeseries """
from malleefowl import wpslogging as logging
#import logging
logger = logging.getLogger(__name__)

from netCDF4 import  Dataset

from cdo import *   # python version
cdo = Cdo()

def fldmean(resource, prefix=None, dir_output = None ):
  from os import path # join, basename , curdir
  from os import mkdir
  """
  retuns a str or list of netCDF filepathes containing the 
  field mean over the whole domain of the resource file
  
  :param resources: string or list of input file path(es)
  :param dir_output: string for path of output dircetory. in None (default) 
  temporare directory will be created
  :param prefix: prefix of the output file. If None (default) is set, 
  basename of infile will be set as basename
  """  
  print 'start'
  if dir_output == None:
    if not path.exists('fldmeans'):
      dir_output = mkdir('fldmeans')
    else:
      dir_output = 'fldmeans'
  print dir_output
  if type(resource) == str:
    try:
      if prefix == None: 
        name = path.basename(resource)
      else: 
        name = prefix+'.nc'
      output = path.join(dir_output,name) 
      cdo.fldmean(input = resource , output = output)
      logger.debug('fieldmean for string path processed.')
    except Exception as e:
      logger.exception('fieldmean for sting path failed: %s\n' % (e))
    
  elif type(resource) == list:
    try:
      output = []
      for i , rs in enumerate(resource):
        if prefix == None: 
          name = path.basename(rs)
        else: 
          name = prefix[i]+'.nc'
        op = path.join(dir_output,name) 
        cdo.fldmean(input = rs , output = op)
        output.append(op)
      logger.debug('fieldmean for list processed.')  
    except Exception as e:
      logger.exception('fieldmean for list of pathes failed: %s\n' % (e))
  else:
    logger.exception('resouce is %s , expect str or list: ' % (type(resource)))
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
    ref_per05s = []
    ref_per95s = []

    for start in years[:-30]:
      try:
        end = start + 29
        ref_periods.append(start)
        ref_stds.append(hs[str(start):str(end)].std())
        ref_medians.append(hs[str(start):str(end)].median())
        ref_means.append(hs[str(start):str(end)].mean())
        ref_per05s.append(hs[str(start):str(end)].quantile(0.05))
        ref_per95s.append(hs[str(start):str(end)].quantile(0.95))
      except Exception as e:
        logger.exception('calculation of statistics failed: %s\n' % (e))
    
    try: 
      nc.createDimension('ref_period', size=len(ref_periods))
      ref_period = nc.createVariable(varname= 'ref_period', datatype = 'i', dimensions='ref_period')
      ref_median = nc.createVariable(varname= 'ref_median', datatype = 'float', dimensions='ref_period')
      ref_mean = nc.createVariable(varname= 'ref_mean', datatype = 'float', dimensions='ref_period')
      ref_std = nc.createVariable(varname= 'ref_std', datatype = 'float', dimensions='ref_period')
      ref_per05 = nc.createVariable(varname= 'ref_per05', datatype = 'float', dimensions='ref_period')
      ref_per95 = nc.createVariable(varname= 'ref_per95', datatype = 'float', dimensions='ref_period')
    except Exception as e:
      logger.exception('create Dimension or Variable failed : %s\n' % (e))

    try: 
      ref_period_att = {'standard_name':'ref_period',
                    'long_name':'start year of reference period',
                    'units':'year' }
      ref_median_att = {'standard_name':'ref_median',
                    'long_name':'median for reference period'}
      ref_mean_att = {'standard_name':'ref_mean',
                    'long_name':'mean for reference period'}
      ref_std_att = {'standard_name':'ref_std',
                    'long_name':'standart deviation for reference period'}
      ref_per05_att = {'standard_name':'ref_per05',
                    'long_name':'5th percentile for reference period'}
      ref_per95_att = {'standard_name':'ref_per95',
                    'long_name':'95th percentile for reference period'}
                    
      ref_period.setncatts(ref_period_att)
      ref_std.setncatts(ref_std_att)
      ref_mean.setncatts(ref_mean_att)
      ref_median.setncatts(ref_median_att)
      ref_per05.setncatts(ref_per05_att)
      ref_per95.setncatts(ref_per95_att)
    except Exception as e:
      logger.exception('add attribution failed : %s\n' % (e))

    try: 
      ref_period[:] = ref_periods
      ref_median[:] = ref_medians
      ref_mean[:] = ref_means
      ref_std[:] = ref_stds
      ref_per05[:] = ref_per05s
      ref_per95[:] = ref_per95s
    except Exception as e:
      logger.exception('add attribution failed : %s\n' % (e))
      
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
  ocgis.env.OVERWRITE = True 
 
  if type(resources) == str :
    resources = list([resources])
 
  rs = []
  for nc in resources: 
    rs.append(utils.drs_filename(nc, 
                                 variable=variable, 
                                 rename_file=True, 
                                 add_file_path=True))
  ncs = utils.sort_by_filename(rs)
  
  if variable==None: 
    name = 'mean'
  else: 
    name = variable
  calc = [{'func':'mean','name':name}]
  calc_grouping = ['year']
  ocgis.env.DIR_OUTPUT = dir_output
  year_means = []
  
  for prefix in ncs: 
    rd = ocgis.RequestDataset(ncs[prefix], variable=variable)
    f = ocgis.OcgOperations(dataset=rd, calc=calc, calc_grouping=calc_grouping,
                            prefix=prefix, output_format='nc', dir_output=dir_output, add_auxiliary_files=False )
    year_means.append(f.execute())
  # sort files
  # aggregate mean years
  
  return year_means # resources # ncs # year_means


def get_time(nc_file):
    """
    returns all timestamps of given netcdf file as datetime list.
    
    :param nc_file: NetCDF file
    """
    ds = Dataset(nc_file)
    time_list = ds.variables['time']
    from datetime import datetime, timedelta
    reftime = datetime.strptime('1949-12-01', '%Y-%m-%d')
    ts = [reftime + timedelta(days= t) for t in time_list]
    
    return ts
  
def get_variable(nc_file, variable=None):
    """
    returns a list of variabel values
    
    :param nc_file: NetCDF file
    """
    
    from flyingpigeon import utils
    from numpy import squeeze
    
    if variable == None: 
      var = utils.get_variable(nc_file)
    else:
      var = variable
    
    ds = Dataset(nc_file)
    values = squeeze(ds.variables[var]).tolist()
    
    return values


def normalize(resource, grouping='year', region='AUT', start_date="1971-01-01", end_date="2010-12-31", out_dir=None):
    """
    noramlize netcdf file for region and timeperiod

    :param resource: netcdf filename
    :param start_date: string with start date
    :param end_date: string with end date
    :param out_dir: output directory for result file (netcdf)

    :return: normalized netcdf file
    """
    rd = ocgis.RequestDataset(resource)
    variable = rd.variable
    from dateutil import parser as date_parser
    time_range=[ date_parser.parse(start_date) , date_parser.parse(end_date) ]
    calc = [{'func':'mean','name':'ref_' + variable }] 

    filename = drs_filename(resource)
    filename = filename.replace("EUR", region)
    from os.path import join
    output = join(out_dir, filename)
    prefix = "ref_%s" % filename
    prefix = prefix.replace('.nc', '')
    
    try: 
        reference = ocgis.OcgOperations(
            dataset=rd,
            geom=COUNTRY_SHP,
            dir_output=out_dir,
            output_format="nc",
            select_ugid=select_ugid(region),
            prefix=prefix,
            add_auxiliary_files=False,
            calc=calc,
            calc_grouping=calc_grouping(grouping),
            time_range=time_range  ).execute()

        from tempfile import mkstemp
        from cdo import Cdo   
        cdo = Cdo()
        _,out_resource = mkstemp(prefix="out_resource_", dir=out_dir)
        cdo.fldmean(input = resource , output = out_resource)
        _,out_ref = mkstemp(prefix="out_ref_", dir=out_dir)
        cdo.fldmean(input = reference , output = out_ref )
        cdo.sub(input = "%s %s" % (out_resource, out_ref) , output = output)
    except:
        msg = 'normalize failed for file : %s ' % filename
        logger.exception(msg)
        raise CalculationException(msg)
    return output    