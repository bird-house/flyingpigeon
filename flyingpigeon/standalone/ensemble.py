import os 
from cdo import *
cdo = Cdo()

norm_dir = '/homel/nhempel/data/cviewer/normalized/'
ens_dir  = '/homel/nhempel/data/cviewer/ensemble/'

def listdir_fullpath(d):
  return [os.path.join(d, f) for f in os.listdir(d)]

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
            input1 = ' '.join(files)
            
            n_mean = 'mean_%i_%s.nc' % (len(files), agg)            
            f_mean = os.path.join(OUT_DIR, n_mean )
            cdo.ensmean(input = input1 , output = f_mean)
            print 'Ensmean done for %s %s ' % (land , n_mean )
            
            n_pctl33 = 'pctl33_%i_%s.nc' % (len(files), agg)            
            f_pctl33 = os.path.join(OUT_DIR, n_pctl33 )
            cdo.enspctl('33', input = input1 , output = f_pctl33)
            print 'Ensmean done for %s %s ' % (land , n_pctl33 )
            
            n_pctl66 = 'pctl66_%i_%s.nc' % (len(files), agg)            
            f_pctl66 = os.path.join(OUT_DIR, n_pctl66 )
            cdo.enspctl('66', input = input1 , output = f_pctl66)
            print 'Ensmean done for %s %s %s ' % (var, land, n_pctl66)
            
      except Exception as e:
        msg = 'Ensemble failed for : %s %s %s ' % (indice, land, e)
        print msg       
    
  

