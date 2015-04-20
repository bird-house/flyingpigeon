import rpy2.robjects as robjects
from rpy2.robjects.packages import importr

def RL(T,a,b,s):
        """Calculation of return levels.
        RL(T,a,b,s)
         """
        T = float(T)
        from math import log 
        yT = -1/log(1 - 1/T)
        if(s != 0):
            zT= a + b*((yT**s) -1)/s
        else:
            zT= a + b * log(yT)
        return(zT)

def rl_bootstrap(ts, T=100, nsim=1000):
    """rl_bootstrap(ts, T=100, nsim=1000)
    """
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

def eventdistribution(ts, per=[33,66], nsim=100, rp = [ 5. ,10. ,20., 50. ,100. ,200., 300., 400. ,500., 700, 1000. ]):
    """ eventdistribution(ts, nsim=100, rp = [ 5. ,10. ,20., 50. ,100. ,200., 300., 400. ,500., 700, 1000. ])
    """
    from rpy2.robjects import FloatVector
    from numpy import vstack, array, percentile
    import rpy2.robjects.numpy2ri
    
    ts = FloatVector(ts)    
    rpy2.robjects.numpy2ri.activate()
    evd = importr('evd')
    
    RR = evd.fgev(ts)
    a=RR.rx2('estimate')[0] # RR.fgev$estimate[1]
    b=RR.rx2('estimate')[1] # RR.fgev$estimate[2]
    s=RR.rx2('estimate')[2] # fgev$estimate[3]
    
    rl = []
    edist = []

    per_low = []
    per_high = []

    for T in rp:
        rl.append(RL(T,a,b,s))
        
        RL_bt = rl_bootstrap(ts,T, nsim=100)
        per = percentile(RL_bt,[33,66])
        per_low.append(per[0])
        per_high.append(per[1])

    rl_c = vstack((rp,per_low,rl, per_high))
    
    return (rl_c)

