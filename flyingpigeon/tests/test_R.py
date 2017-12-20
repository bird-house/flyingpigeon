import pytest


def test_R_rpy2():
    import rpy2.robjects
    import rpy2.robjects.numpy2ri
    rpy2.robjects.numpy2ri.activate()


def test_R_imports():
    from rpy2.robjects.packages import importr
    fields = importr("fields")
    stats = importr("stats")
    ncdf4 = importr("ncdf4")
    mclust = importr("mclust")
    maps = importr("maps")
    grDevices = importr("grDevices")
