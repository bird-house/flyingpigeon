import os
import tempfile
import shutil
from datetime import datetime, timedelta
from netCDF4 import Dataset

from cdo import *
cdo = Cdo()

norm_dir = '/homel/nhempel/data/cviewer/normalized/'
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


aggregation = ['yr','DJF','MAM','JJA','SON']

for indice in os.listdir(norm_dir): # /RR/rcp45/AUT/
  for scenario in os.listdir(os.path.join(norm_dir,indice)): 
    for land in os.listdir(os.path.join(norm_dir,indice,scenario)): 
      
      ncs = listdir_fullpath(os.path.join(norm_dir,indice,scenario, land ))
      
      if not os.path.exists(os.path.join(ens_dir,indice,scenario, land)):
        os.makedirs(os.path.join(ens_dir,indice,scenario, land ))
      OUT_DIR = os.path.join(ens_dir,indice,scenario, land)
      try : 
        for agg in aggregation:
          files = []
          files = [nc for nc in ncs if '_%s.nc' %(agg) in nc]
          if len(files) > 0:
            print 'Ensmean preparing %s %s ' % (indice, land )
            tmp_dir = tempfile.mkdtemp()
            try: 
              # for year in range(1950,2101,10): 
              try:
                year = '1971-2100'
                for f in files: 
                  p1, tmp1 = tempfile.mkstemp(dir=tmp_dir, suffix='_%s_.nc' %(year))
                  cdo.inttime('1971-01-01,12:00:00,1years', input=f, output = tmp1)
                  os.close(p1)
                
                f_list = listdir_fullpath(tmp_dir)
                f_year = [n for n in f_list if '_%s_.nc' %(year) in n ]
                
                
                input1 = ' '.join(f_year)
                n_mean = 'mean_%s_%s.nc' % (year, agg)            
                f_mean = os.path.join(OUT_DIR, n_mean )
                cdo.ensmean(input = input1 , output = f_mean)
                print 'Ensmean done for %s %s %s ' % (indice, land , n_mean )
                
                n_pctl33 = 'pctl33_%s_%s.nc' % (year, agg)            
                f_pctl33 = os.path.join(OUT_DIR, n_pctl33 )
                cdo.enspctl('33', input = input1 , output = f_pctl33)
                print 'Ensmean done for %s %s %s ' % (indice, land , n_pctl33 )
                
                n_pctl66 = 'pctl66_%s_%s.nc' % (year, agg)            
                f_pctl66 = os.path.join(OUT_DIR, n_pctl66 )
                cdo.enspctl('66', input = input1 , output = f_pctl66)
                print 'Ensmean done for %s %s %s ' % (indice, land, n_pctl66)
              except Exception as e:
                msg = 'file failed : %s %s %s ' % ( indice, land, e)
                print msg
            except Exception as e:
              msg = 'timeslice failed : %s %s %s ' % ( indice, land, e)
              print msg
              
            #f_list = []
            #f_list = listdir_fullpath(tmp_dir)
            #if len(f_list) > 0:
              #f_mean = [n for n in f_list if 'mean_' in n ]
              #f_pctl33 = [n for n in f_list if 'pctl33_' in n ]
              #f_pctl66 = [n for n in f_list if 'pctl66_' in n ]
              
              #out_mean = os.path.join(OUT_DIR, 'mean_%s_%s.nc' % (indice, agg) )
              #out_pctl33 = os.path.join(OUT_DIR, 'pctl33_%s_%s.nc' % (indice, agg) )
              #out_pctl66 = os.path.join(OUT_DIR, 'pctl66_%s_%s.nc' % (indice, agg) )
              
              #input_mean = ' '.join(f_mean)
              #input_pctl33 = ' '.join(f_pctl33)
              #input_pctl66 = ' '.join(f_pctl66)
              
              #if len(f_mean) > 0:
                #cdo.mergetime(input = input_mean , output = out_mean)
              #if len(f_pctl33) > 0:
                #cdo.mergetime(input = input_pctl33 , output = out_pctl33)
              #if len(f_pctl66) > 0:
                #cdo.mergetime(input = input_pctl66 , output = out_pctl66)
            
            shutil.rmtree(tmp_dir)
      except Exception as e:
        msg = 'Ensemble failed for : %s %s %s ' % (indice, land, e)
        print msg       
    
  

