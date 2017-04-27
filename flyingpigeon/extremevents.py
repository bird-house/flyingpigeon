import logging
LOGGER = logging.getLogger("PYWPS")

from os.path import exists

def RL(T,a,b,s):
    """Calculation of return levels.

    :param T: number of timestepps
    :param a:
    :param b:
    :param s:
    """
    T = float(T)
    from math import log
    yT = -1/log(1 - 1/T)

    s = s * -1

    if(s != 0):
        zT= a + b*((yT**s) -1)/s
    else:
        zT= a + b * log(yT)
    return(zT)

def resample(X, n=None):
    """ Bootstrap resample an array_like Parameters

    :param X: array_like data to resample
    :param n: int, optional length of resampled array, equal to len(X) if n==None Results
    """

    from random import sample, uniform

    if n == None:
      n = int(uniform(1,len(X)))

    X_resample = sample(X, n)

    return X_resample

def rl_bootstrap(data, T=100, nsim=1000):
    """returns a return level

    :param data: list of input data
    :param T: timestep period
    :param nsim: number of recalcualtions
    """

    from scipy.stats import genextreme as gev

    RL_bt=[]
    for i in range(0,nsim,1):
        subset = resample(data)
        s, a, b = gev.fit(subset)
        RL_bt.append(RL(T,a,b,s))
    return RL_bt


def eventdistribution(data, per=[5,95], nsim=1000, rp = [ 10., 20., 50., 100., 200.,500., 1000. ], rp_scale_factor=1, white_noise=False):
    """
    returns a matrix with (returnperiod,lower_percentil,return_level, upper_percentil)

    :param data: values of timeseries
    :param per: lower and upper percentile defining the uncertainty
    :param nsim: Number of returs for bootstrap calculation
    :param rp: list of return timestepps
    :param rp_scale_factor: scale factor for rp
    :param std_err: default = True
    :param white_noise: add a white noise (random number between 0 to std/10). In case of singular timeseries
    """

    from scipy.stats import genextreme as gev
    from numpy import percentile, vstack

    if white_noise == True:
        s = std(data)/10
        ts_white_noise = [n + uniform(0,s) for n in data]
        data = ts_white_noise

    s, a, b = gev.fit(data)

    rl = []
    edist = []

    per_low = []
    per_high = []

    for T in rp * rp_scale_factor :

        rl.append(RL(T,a,b,s))

        RL_bt = rl_bootstrap(data, T=T, nsim=nsim)
        #per, b = percentile(RL_bt,[per[0],per[1]])
        per_low.append(percentile(RL_bt, 5))
        per_high.append(percentile(RL_bt, 95))

    rl_c = vstack((rp, per_low, rl, per_high))

    return (rl_c)
