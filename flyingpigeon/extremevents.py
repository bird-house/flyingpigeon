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

def eventdistribution(ts, per=[5,95], nsim=10, rp = [ 10., 100., 1000. ], rp_scale_factor=1,  std_err = True, white_noise=False):
    """ 
    returns a matrix with (returnperiod,lower_percentil,return_level, upper_percentil)
    
    :param ts: values of timeseries
    :param per: lower and upper percentile defining the uncertainty
    :param nsim: Number of returs for bootstrap calculation
    :param rp: list of return timestepps
    :param rp_scale_factor: scale factor for rp
    :param std_err: default = True
    :param white_noise: add a white noise (random number between 0 to std/10). In case of singular timeseries
    
    """
    from rpy2.robjects import FloatVector
    from numpy import vstack, array, percentile, std 
    from random import uniform
    import rpy2.robjects.numpy2ri
    
    ts = FloatVector(ts) 
    if white_noise == True: 
      s = std(ts)/10
      ts_white_noise = [n + uniform(0,s) for n in ts]
      ts = ts_white_noise
    
    rpy2.robjects.numpy2ri.activate()
    evd = importr('evd')
    
    RR = evd.fgev_norm(ts, std_err = std_err )
    a=RR.rx2('estimate')[0] # RR.fgev$estimate[1]
    b=RR.rx2('estimate')[1] # RR.fgev$estimate[2]
    s=RR.rx2('estimate')[2] # fgev$estimate[3]
    
    rl = []
    edist = []

    per_low = []
    per_high = []

    for T in rp * rp_scale_factor :
        rl.append(RL(T,a,b,s))
        
        RL_bt = rl_bootstrap(ts,T, nsim=100)
        per = percentile(RL_bt,[33,66])
        per_low.append(per[0])
        per_high.append(per[1])

    rl_c = vstack((rp,per_low,rl, per_high))
    
    return (rl_c)

