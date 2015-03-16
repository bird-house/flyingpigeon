import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

def RL(T,a,b,s):
        T = float(T)
        from math import log 
        yT = -1/log(1 - 1/T)
        if(s != 0):
            zT= a + b*((yT**s) -1)/s
        else:
            zT= a + b * log(yT)
        return(zT)

def returnlevels(ts):
    from numpy import vstack, array, percentile
    import rpy2.robjects.numpy2ri
    from rpy2.robjects import FloatVector
    
    rpy2.robjects.numpy2ri.activate()
    evd = importr('evd')
    
    ts = FloatVector(ts)
    RR=evd.fgev(ts)
    a=RR.rx2('estimate')[0] # RR.fgev$estimate[1]
    b=RR.rx2('estimate')[1] # RR.fgev$estimate[2]
    s=RR.rx2('estimate')[2] # fgev$estimate[3]
    
    rp = [ 5. ,10. ,20., 50. ,100. ,200., 300., 400. ,500. ,1000. ]
    rl = []
    per10 = []
    per90 = []

    for T in rp:
        rl.append(RL(T,a,b,s))

##        RL_bt = rl_bootstrap(ts, T)
##        per = percentile(RL_bt,[10,90])
##        per10.append(per[0])
##        per90.append(per[1])

    rl_c = vstack((rp,rl))
    
    return (rl_c)

def rl_bootstrap(ts, T):
    import rpy2.robjects.numpy2ri
    from rpy2.robjects import FloatVector
    from numpy import sort , array
    rpy2.robjects.numpy2ri.activate()
    rsample = robjects.globalenv.get("sample")
    runique = robjects.globalenv.get("unique")
    evd = importr('evd')

    ts = FloatVector(ts)
    II= range(0,len(ts),1)

    ts_fgev = (evd.fgev(ts))
    a=ts_fgev.rx2('estimate')[0]
    b=ts_fgev.rx2('estimate')[1] 
    s=ts_fgev.rx2('estimate')[2]

    RL0 = RL(T,a,b,s)
    nsim=100
    RL_bt=[]
    for i in range(0,nsim,1):
        II_s= sort(runique(rsample(II,replace=True)))
        ts_s = array([ts[i] for i in II_s])
        ts_s_fgev=evd.fgev(ts_s)
        a=ts_s_fgev.rx2('estimate')[0] # RR.fgev$estimate[1]
        b=ts_s_fgev.rx2('estimate')[1] # RR.fgev$estimate[2]
        s=ts_s_fgev.rx2('estimate')[2] # fgev$estimate[3]
        RL_bt.append(RL(T,a,b,s))
        
    return RL_bt

    
    

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




   
