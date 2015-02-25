import rpy2.robjects as robjects
from rpy2.robjects.packages import importr
from numpy import vstack


def returnlevel(ts):
  from numpy import vstack
  evir = importr('evir')
  
  rp = [ 5 ,10 ,20, 50 ,100 ,200, 300, 400 ,500 ,1000 ]
  rl = []
  rl_min = []
  rl_max = []
  
  xx = evir.gev(ts)
  for r in rp:
        y= evir.rlevel_gev(xx , r , add= True )
    
        rl_min.append(y[0])
        rl.append(y[1])
        rl_max.append(y[2])
    
  rl_c = vstack((rp, rl_min, rl, rl_max))
  return rl_c   
