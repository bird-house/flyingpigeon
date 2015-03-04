import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

def returnlevels(ts):
    from numpy import vstack, array
    import rpy2.robjects.numpy2ri
    rpy2.robjects.numpy2ri.activate()
    evd = importr('evd')
    
    def RL(T,a,b,s):
        from math import log 
        yT = -1/log(1 - 1/T)
        if(s != 0):
            zT= a + b*((yT**s) -1)/s
        else:
            zT= a + b * log(yT)
        return(zT)

    ts = array(ts)
    RR=evd.fgev(ts)
    a=RR.rx2('estimate')[0] # RR.fgev$estimate[1]
    b=RR.rx2('estimate')[1] # RR.fgev$estimate[2]
    s=RR.rx2('estimate')[2] # fgev$estimate[3]
    
    rp = [ 5. ,10. ,20., 50. ,100. ,200., 300., 400. ,500. ,1000. ]
    rl = []
    # rl_min = []
    # rl_max = []
    # T=1000.
    for T in rp:
        rl.append(RL(T,a,b,s))
    rl_c = vstack((rp,rl)) 
    return (rl_c)


#def returnlevel(ts):
#  from numpy import vstack

#  evir = importr('evir')
#  base = importr('base')
#  gr = importr('grDevices')

  
#  rp = [ 5 ,10 ,20, 50 ,100 ,200, 300, 400 ,500 ,1000 ]
#  rl = []
#  rl_min = []
#  rl_max = []
  
#  xx = evir.gev(ts)
#  for r in rp:
        #gr.dev_new()
#        y= evir.rlevel_gev(xx , r )#, add = True )
    
#        rl_min.append(y[0])
#        rl.append(y[1])
#        rl_max.append(y[2])
    
#  rl_c = vstack((rp, rl_min, rl, rl_max))
#  return rl_c




   
