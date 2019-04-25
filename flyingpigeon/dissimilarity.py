# -*- encoding: utf8 -*-
import numpy as np
from scipy import spatial
from scipy.spatial import cKDTree as KDTree

"""
Methods to compute the (dis)similarity between samples
======================================================

This module implements five of the six methods described in [Grenier20131]_ to measure
the dissimilarity between two samples. Some of these algorithms can be used to
test whether or not two samples have been drawn from the same distribution.
Here, they are used to find areas with analog climate conditions to a target
climate.

Methods available
-----------------
 * Standardized Euclidean distance
 * Nearest Neighbour distance
 * Zech-Aslan energy statistic
 * Friedman-Rafsky runs statistic
 * Kolmogorov-Smirnov statistic
 * Kullback-Leibler divergence


:author: David Huard, Patrick Grenier
:institution: Ouranos inc.
"""

# TODO: Szekely, G, Rizzo, M (2014) Energy statistics: A class of statistics
# based on distances. J Stat Planning & Inference 143: 1249-1272

# TODO: Hellinger distance

__all__ = ['seuclidean', 'nearest_neighbor', 'zech_aslan',
           'kolmogorov_smirnov', 'friedman_rafsky',
           'kldiv']


# ---------------------------------------------------------------------------- #
# -------------------------- Utility functions ------------------------------- #
# ---------------------------------------------------------------------------- #


def reshape_sample(x, y):
    """
    Reshape the input arrays to conform to the conventions used in the
    dissimilarity metrics.

    Parameters
    ----------
    x, y : array_like
      Arrays to be compared.

    Returns
    -------
    x, y : array_like
      Arrays of shape (n,d) and (m,d), where `n` and `m` are the number of
      samples and `d` is the dimension.

    Raises
    ------
    AssertionError
        If x and y have different dimensions.
    """
    x = np.atleast_2d(x)
    y = np.atleast_2d(y)

    # If array is 1D, flip it.
    if x.shape[0] == 1:
        x = x.T
    if y.shape[0] == 1:
        y = y.T

    if x.shape[1] != y.shape[1]:
        raise AttributeError("Shape mismatch")

    return x, y


def standardize(x, y):
    """
    Standardize x and y by the square root of the product of their standard
    deviation.

    Parameters
    ----------
    x, y : ndarray
        Arrays to be compared.

    Returns
    -------
    x, y : ndarray
        Standardized arrays.
    """
    s = np.sqrt(x.std(0, ddof=1) * y.std(0, ddof=1))
    return x / s, y / s


# ---------------------------------------------------------------------------- #
# ------------------------ Dissimilarity metrics ----------------------------- #
# ---------------------------------------------------------------------------- #

def seuclidean(x, y):
    """
    Compute the Euclidean distance between the mean of a multivariate
    candidate sample with respect to the mean of a reference sample.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Standardized Euclidean Distance between the mean of the samples
        ranging from 0 to infinity.

    Notes
    -----
    This metric considers neither the information from individual points nor
    the standard deviation of the candidate distribution.

    References
    ----------
    Veloz et al. (2011) Identifying climatic analogs for Wisconsin under
    21st-century climate-change scenarios. Climatic Change,
    DOI 10.1007/s10584-011-0261-z.
    """
    x, y = reshape_sample(x, y)

    mx = x.mean(0)
    my = y.mean(0)

    return spatial.distance.seuclidean(mx, my, x.var(0, ddof=1))


def nearest_neighbor(x, y):
    """
    Compute a dissimilarity metric based on the number of points in the
    pooled sample whose nearest neighbor belongs to the same distribution.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Nearest-Neighbor dissimilarity metric ranging from 0 to 1.

    References
    ----------
    Henze N. (1988) A Multivariate two-sample test based on the number of
    nearest neighbor type coincidences. Ann. of Stat., Vol. 16, No.2, 772-783.
    """
    x, y = reshape_sample(x, y)
    x, y = standardize(x, y)

    nx, _ = x.shape

    # Pool the samples and find the nearest neighbours
    xy = np.vstack([x, y])
    tree = KDTree(xy)
    _, ind = tree.query(xy, k=2, eps=0, p=2, n_jobs=2)

    # Identify points whose neighbors are from the same sample
    same = ~np.logical_xor(*(ind < nx).T)

    return same.mean()


def zech_aslan(x, y):
    """
    Compute the Zech-Aslan energy distance dissimimilarity metric based on an
    analogy with the energy of a cloud of electrical charges.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Zech-Aslan dissimilarity metric ranging from -infinity to infinity.

    References
    ----------
    Zech G. and Aslan B. (2003) A Multivariate two-sample test based on the
    concept of minimum energy. PHYStat2003, SLAC, Stanford, CA, Sep 8-11.
    Aslan B. and Zech G. (2008) A new class of binning-free, multivariate
    goodness-of-fit tests: the energy tests. arXiV:hep-ex/0203010v5.
    """

    x, y = reshape_sample(x, y)
    nx, d = x.shape
    ny, d = y.shape

    v = x.std(0, ddof=1) * y.std(0, ddof=1)

    dx = spatial.distance.pdist(x, 'seuclidean', V=v)
    dy = spatial.distance.pdist(y, 'seuclidean', V=v)
    dxy = spatial.distance.cdist(x, y, 'seuclidean', V=v)

    phix = -np.log(dx).sum() / nx / (nx - 1)
    phiy = -np.log(dy).sum() / ny / (ny - 1)
    phixy = np.log(dxy).sum() / nx / ny
    return phix + phiy + phixy


def skezely_rizzo(x, y):
    """
    Compute the Skezely-Rizzo energy distance dissimimilarity metric
    based on an analogy with the energy of a cloud of electrical charges.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Skezely-Rizzo dissimilarity metric ranging from -infinity to infinity.

    References
    ----------
    TODO
    """
    raise NotImplementedError

    # x, y = reshape_sample(x, y)
    # nx, d = x.shape
    # ny, d = y.shape
    #
    # v = x.std(0, ddof=1) * y.std(0, ddof=1)
    #
    # dx = spatial.distance.pdist(x, 'seuclidean', V=v)
    # dy = spatial.distance.pdist(y, 'seuclidean', V=v)
    # dxy = spatial.distance.cdist(x, y, 'seuclidean', V=v)
    #
    # phix = -np.log(dx).sum() / nx / (nx - 1)
    # phiy = -np.log(dy).sum() / ny / (ny - 1)
    # phixy = np.log(dxy).sum() / nx / ny

    # z = dxy.sum() * 2. / (nx*ny) - (1./nx**2) *

    # z = (2 / (n * m)) * sum(dxy(:)) - (1 / (n ^ 2)) * sum(2 * dx) - (1 /
    #  (m ^ 2)) * sum(2 * dy);
    # z = ((n * m) / (n + m)) * z;


def friedman_rafsky(x, y):
    """
    Compute a dissimilarity metric based on the Friedman-Rafsky runs statistics.

    The algorithm builds a minimal spanning tree (the subset of edges
    connecting all points that minimizes the total edge length) then counts
    the edges linking points from the same distribution.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Friedman-Rafsky dissimilarity metric ranging from 0 to (m+n-1)/(m+n).

    References
    ----------
    Friedman J.H. and Rafsky L.C. (1979) Multivariate generaliations of the
    Wald-Wolfowitz and Smirnov two-sample tests. Annals of Stat. Vol.7,
    No. 4, 697-717.
    """
    from sklearn import neighbors
    from scipy.sparse.csgraph import minimum_spanning_tree

    x, y = reshape_sample(x, y)
    nx, _ = x.shape
    ny, _ = y.shape
    n = nx + ny

    xy = np.vstack([x, y])

    # Compute the NNs and the minimum spanning tree
    g = neighbors.kneighbors_graph(xy, n_neighbors=n - 1, mode='distance')
    mst = minimum_spanning_tree(g, overwrite=True)
    edges = np.array(mst.nonzero()).T

    # Number of points whose neighbor is from the other sample
    diff = np.logical_xor(*(edges < nx).T).sum()

    return 1. - (1. + diff) / n


def kolmogorov_smirnov(x, y):
    """
    Compute the Kolmogorov-Smirnov statistic applied to two multivariate
    samples as described by Fasano and Franceschini.

    Parameters
    ----------
    x : ndarray (n,d)
        Reference sample.
    y : ndarray (m,d)
        Candidate sample.

    Returns
    -------
    float
        Kolmogorov-Smirnov dissimilarity metric ranging from 0 to 1.

    References
    ----------
    Fasano G. and Francheschini A. (1987) A multidimensional version
    of the Kolmogorov-Smirnov test. Monthly Notices of the Royal
    Astronomical Society, vol. 225, pp. 155-170.
    """
    x, y = reshape_sample(x, y)

    def pivot(x, y):
        nx, d = x.shape
        ny, d = y.shape

        # Multiplicating factor converting d-dim booleans to a unique integer.
        mf = (2 ** np.arange(d)).reshape(1, d, 1)
        minlength = 2 ** d

        # Assign a unique integer according on whether or not x[i] <= sample
        ix = ((x.T <= np.atleast_3d(x)) * mf).sum(1)
        iy = ((x.T <= np.atleast_3d(y)) * mf).sum(1)

        # Count the number of samples in each quadrant
        cx = 1. * np.apply_along_axis(np.bincount, 0, ix, minlength=minlength) / nx
        cy = 1. * np.apply_along_axis(np.bincount, 0, iy, minlength=minlength) / ny

        # This is from https://github.com/syrte/ndtest/blob/master/ndtest.py
        # D = cx - cy
        # D[0,:] -= 1. / nx # I don't understand this...
        # dmin, dmax = -D.min(), D.max() + .1 / nx

        return np.max(np.abs(cx - cy))

    return max(pivot(x, y), pivot(y, x))


def kldiv(x, y, k=1):
    """
    Compute the Kullback-Leibler divergence between two multivariate samples.

    .. math
        D(P||Q) = "\"frac{d}{n} "\"sum_i^n "\"log{"\"frac{r_k(x_i)}{s_k(x_i)}} + "\"log{"\"frac{m}{n-1}}

    where r_k(x_i) and s_k(x_i) are, respectively, the euclidean distance
    to the kth neighbour of x_i in the x array (excepting x_i) and
    in the y array.

    Parameters
    ----------
    x : ndarray (n,d)
        Samples from distribution P, which typically represents the true
        distribution (reference).
    y : ndarray (m,d)
        Samples from distribution Q, which typically represents the
        approximate distribution (candidate)
    k : int or sequence
        The kth neighbours to look for when estimating the density of the
        distributions. Defaults to 1, which can be noisy.

    Returns
    -------
    out : float or sequence
        The estimated Kullback-Leibler divergence D(P||Q) computed from
        the distances to the kth neighbour.

    Notes
    -----
    In information theory, the Kullback–Leibler divergence is a non-symmetric
    measure of the difference between two probability distributions P and Q,
    where P is the "true" distribution and Q an approximation. This nuance is
    important because D(P||Q) is not equal to D(Q||P).

    For probability distributions P and Q of a continuous random variable,
    the K–L  divergence is defined as:

        D_{KL}(P||Q) = "\"int p(x) "\"log{p()/q(x)} dx

    This formula assumes we have a representation of the probability
    densities p(x) and q(x).  In many cases, we only have samples from the
    distribution, and most methods first estimate the densities from the
    samples and then proceed to compute the K-L divergence. In Perez-Cruz,
    the authors propose an algorithm to estimate the K-L divergence directly
    from the sample using an empirical CDF. Even though the CDFs do not
    converge to their true values, the paper proves that the K-L divergence
    almost surely does converge to its true value.

    References
    ----------
    Kullback-Leibler Divergence Estimation of Continuous Distributions (2008).
    Fernando Pérez-Cruz.
    """

    mk = np.iterable(k)
    ka = np.atleast_1d(k)

    x, y = reshape_sample(x, y)

    nx, d = x.shape
    ny, d = y.shape

    # Limit the number of dimensions to 10, too slow otherwise.
    if d > 10:
        raise ValueError("Too many dimensions: {}.".format(d))

    # Not enough data to draw conclusions.
    if nx < 5 or ny < 5:
        return np.nan

    # Build a KD tree representation of the samples.
    xtree = KDTree(x)
    ytree = KDTree(y)

    # Get the k'th nearest neighbour from each points in x for both x and y.
    # We get the values for K + 1 to make sure the output is a 2D array.
    kmax = max(ka) + 1
    r, _ = xtree.query(x, k=kmax, eps=0, p=2, n_jobs=2)
    s, _ = ytree.query(x, k=kmax, eps=0, p=2, n_jobs=2)

    # There is a mistake in the paper. In Eq. 14, the right side misses a
    # negative sign on the first term of the right hand side.
    out = []
    for ki in ka:
        # The 0th nearest neighbour of x[i] in x is x[i] itself.
        # Hence we take the k'th + 1, which in 0-based indexing is given by
        # index k.
        out.append(-np.log(r[:, ki] / s[:, ki - 1]).sum() * d / nx + np.log(ny / (nx - 1.)))

    if mk:
        return out
    else:
        return out[0]
