import os
import tempfile
import shutil
from datetime import datetime, timedelta
from netCDF4 import Dataset

from cdo import *
cdo = Cdo()
cdo.forceOutput = True

anom_dir = '/homel/nhempel/data/cviewer/anomalies/'
ens_dir  = '/homel/nhempel/data/cviewer/ensemble/'

def listdir_fullpath(d):
  return [os.path.join(d, f) for f in os.listdir(d)]

def get_timestamps(nc_file):
    """
    returns from/to timestamp of given netcdf file.
    
    :param nc_file: NetCDF file
    returns tuple (from_timestamp, to_timestamp)
    """
    ds = Dataset(nc_file)
    time_list = ds.variables['time']
    from datetime import datetime, timedelta
    reftime = datetime.strptime('1949-12-01', '%Y-%m-%d')
    from_timestamp = datetime.strftime(reftime + timedelta(days=time_list[0]), '%Y%m%d') 
    to_timestamp = datetime.strftime(reftime + timedelta(days=time_list[-1]), '%Y%m%d')
    return (from_timestamp, to_timestamp)

aggregation = ['yr', 'DJF', 'MAM','JJA','SON']

long_name_dic = {'ID':'Ice days (TX < 0C)',
                 'CDD':'Maximum number of consecutive dry days (RR < 1 mm)',
                 'CSU':'Maximum number of consecutive summer days (TX > 25C)',
                 'TXx':'Maximum value of daily maximum temperature',
                 'SU':'Summer days (TX > 25C)',
                 'R20mm':'Very heavy precipitation days (precipitation >= 20 mm)',
                 'RR':'Precipitation sum',
                 'RX5day':'Highest 5-day precipitation amount',
                 'TX':'Mean of daily maximum temperature',
                 }

units_dic = {'ID':'days',
             'CDD':'days',
             'CSU':'days',
             'TXx':'Celcius',
             'SU':'days',
             'R20mm':'days',
             'RR':'mm',
             'RX5day':'mm',
             'TX':'Celcius',
             }

i = os.listdir(anom_dir)

for indice in i : # /RR/rcp45/AUT/
  s = os.listdir(os.path.join(anom_dir,indice))
  for scenario in s:
    l = os.listdir(os.path.join(anom_dir,indice,scenario))
    for land in l: 
      ncs = listdir_fullpath(os.path.join(anom_dir,indice,scenario, land ))

      if not os.path.exists(os.path.join(ens_dir,indice,scenario, land)):
        os.makedirs(os.path.join(ens_dir,indice,scenario, land ))
      OUT_DIR = os.path.join(ens_dir,indice,scenario, land)
      print 'Preparing %s %s %s ' % (indice,scenario,  land )
      try : 
        for agg in aggregation:
          files = []
          files = [nc for nc in ncs if '_%s.nc' %(agg) in nc]
          m = len(files) 
          if m > 0:
            tmp_dir = tempfile.mkdtemp()
            try:
              year = '1971-2100'
              for f in files: 
                p1, tmp1 = tempfile.mkstemp(dir=tmp_dir, suffix='_%s_.nc' %(year))
                cdo.inttime('1971-01-01,00:00:00,1years', input=f, output = tmp1)
                os.close(p1)
              f_list = listdir_fullpath(tmp_dir)
              f_year = [n for n in f_list if '_%s_.nc' %(year) in n ]
              
              mean_dic = {'NumberEnsembles': int(m) }
              median_dic = {'NumberEnsembles': int(m) }
              pctl33_dic = {'NumberEnsembles': int(m) }
              pctl66_dic = {'NumberEnsembles': int(m) }
              input1 = ' '.join(f_year)
              
              try: 
                n_mean = 'mean_%s_%s.nc' % (year, agg)            
                f_mean = os.path.join(OUT_DIR, n_mean )
                cdo.ensmean( input = f_year , output = f_mean)
                ds = Dataset(f_mean, 'a')
                for key in ds.ncattrs(): 
                  mean_dic[key] = ds.getncattr(key)
                ds.setncatts(mean_dic)
                var = ds.variables.get(indice)
                var.setncattr('units', units_dic[indice])
                var.setncattr('standard_name', indice)
                var.setncattr('long_name', long_name_dic[indice])
                ds.close()
                print 'Ensmean done for %s %s %s' % (indice, land , n_mean)
              except Exception as e:
                msg = 'ensmean failed : %s %s %s ' % ( indice, land, e)
                print msg
                
              try:
                n_median = 'median_%s_%s.nc' % (year, agg)            
                f_median = os.path.join(OUT_DIR, n_median )
                cdo.enspctl('50', input = input1 , output = f_median)
                ds = Dataset(f_median, 'a')
                for key in ds.ncattrs(): 
                  median_dic[key] = ds.getncattr(key)
                ds.setncatts(median_dic)
                var = ds.variables.get(indice)
                var.setncattr('units', units_dic[indice])
                var.setncattr('standard_name', indice)
                var.setncattr('long_name', long_name_dic[indice])
                ds.close()
                print 'Ensmedian done for %s %s %s' % (indice, land , n_median)
              except Exception as e:
                msg = 'ensmedian failed : %s %s %s ' % ( indice, land, e)
                print msg
              
              try:  
                n_pctl33 = 'pctl33_%s_%s.nc' % (year, agg)            
                f_pctl33 = os.path.join(OUT_DIR, n_pctl33 )
                cdo.enspctl('33', input = input1 , output = f_pctl33)
                ds = Dataset(f_pctl33, 'a')
                for key in ds.ncattrs(): 
                  pctl33_dic[key] = ds.getncattr(key)
                ds.setncatts(pctl33_dic)
                var = ds.variables.get(indice)
                var.setncattr('units', units_dic[indice])
                var.setncattr('standard_name', indice)
                var.setncattr('long_name', long_name_dic[indice])
                ds.close()
                print 'Enspctl33 done for %s %s %s ' % (indice, land , n_pctl33 )
              except Exception as e:
                msg = 'Enspctl33 failed : %s %s %s ' % ( indice, land, e)
                print msg
              
              try:
                n_pctl66 = 'pctl66_%s_%s.nc' % (year, agg)            
                f_pctl66 = os.path.join(OUT_DIR, n_pctl66 )
                cdo.enspctl('66', input = input1 , output = f_pctl66)
                ds = Dataset(f_pctl66, 'a')
                for key in ds.ncattrs(): 
                  pctl66_dic[key] = ds.getncattr(key)
                ds.setncatts(pctl66_dic)
                var = ds.variables.get(indice)
                var.setncattr('units', units_dic[indice])
                var.setncattr('standard_name', indice)
                var.setncattr('long_name', long_name_dic[indice])
                ds.close()
                print 'Enspctl66 done for %s %s %s ' % (indice, land, n_pctl66)
              except Exception as e:
                msg = 'Enspctl66 failed : %s %s %s ' % ( indice, land, e)
                print msg
                
            except Exception as e:
              msg = 'ensemble statistic failed : %s %s %s ' % ( indice, land, e)
              print msg
          
            shutil.rmtree(tmp_dir)
      except Exception as e:
        msg = 'LAND failed for : %s %s %s ' % (indice, land, e)
        print msg       
    
  

