import pytest


def test_pygbif():
    import pygbif


@pytest.mark.skip(reason="R not available")
def test_mgcv():
    from rpy2.robjects.packages import importr
    import rpy2.robjects as ro
    import rpy2.robjects.numpy2ri

    rpy2.robjects.numpy2ri.activate()
    base = importr("base")
    # stats = importr("stats")
    mgcv = importr("mgcv")
    graphics = importr("graphics")

    base.set_seed(2)  # simulate some data...
    dat = mgcv.gamSim(1, n=400, dist="normal", scale=2)
    eq = ro.Formula('y~s(x0)+s(x1)+s(x2)+s(x3)')
    b = mgcv.gam(eq, data=dat)
    # summary(b)
    graphics.plot(b, pages=1)  # , residuals=base.TRUE)  # show partial residuals
    # plot(b,pages=1,seWithMean=TRUE) ## `with intercept' CIs
    # ## run some basic model checks, including checking
    # ## smoothing basis dimensions...
    mgcv.gam_check(b)
