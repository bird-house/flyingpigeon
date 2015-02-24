import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from numpy import vstack

def returnlevel(ts):
  
  evir = importr('evir')
  
  rp = [ 5 ,10 ,20, 50 ,100 ,200, 300, 400 ,500 ,1000, 10000 ]
  rl = []
  rl_min = []
  rl_max = []
  
  #for r in rp: 
    #y = evir.rlevel_gev(ts , r )
    #rl_min.append(y[0])
    #rl_max.append(y[2])
    #rl.append(y[1])
    
  #rl_c = numpy.vstack((rl_min, rl, rl_max))
  return rp  
  
