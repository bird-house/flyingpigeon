import pytest

from flyingpigeon import dissimilarity as dd
import numpy as np
from numpy.testing import assert_equal as aeq, assert_almost_equal as aaeq
from scipy import stats
from scipy import integrate
from scipy import spatial


def matlab_sample(n=30):
    """
    In some of the following tests I'm using Matlab code written by Patrick
    Grenier for the paper "An Assessment of Six Dissimilarity Metrics for
    Climate Analogs" to compare against the functions here. The sample
    created here is identical to the sample used to drive the Matlab code.

    :param n:
    :return:
    """

    z = 1. * (np.arange(n) + 1) / n - .5

    x = np.vstack([
        z * 2 + 30,
        z * 3 + 40,
        z
    ]).T

    y = np.vstack([
        z * 2.2 + 31,
        z[::-1] * 2.8 + 38,
        z * 1.1
    ]).T

    return x, y


def randn(mean, std, shape):
    """Return a random normal sample with exact mean and standard deviation."""
    r = np.random.randn(*shape)
    r1 = r / r.std(0, ddof=1) * np.array(std)
    return r1 - r1.mean(0) + np.array(mean)


def test_randn():
    mu, std = [2, 3], [1, 2]
    r = randn(mu, std, [10, 2])
    aaeq(r.mean(0), mu)
    aaeq(r.std(0, ddof=1), std)


class TestSEuclidean():
    def test_simple(self):
        d = 2
        n, m = 25, 30

        x = randn(0, 1, (n, d))
        y = randn([1, 2], 1, (m, d))
        dm = dd.seuclidean(x, y)
        aaeq(dm, np.hypot(1, 2), 2)

        # Variance of the candidate sample does not affect answer.
        x = randn(0, 1, (n, d))
        y = randn([1, 2], 2, (m, d))
        dm = dd.seuclidean(x, y)
        aaeq(dm, np.hypot(1, 2), 2)

    def test_compare_with_matlab(self):
        x, y = matlab_sample()
        dm = dd.seuclidean(x, y)
        aaeq(dm, 2.8463, 4)


class TestNN():
    def test_simple(self):
        d = 2
        n, m = 200, 200
        np.random.seed(1)
        x = np.random.randn(n, d)
        y = np.random.randn(m, d)

        # Almost identical samples
        dm = dd.nearest_neighbor(x + .001, x)
        aaeq(dm, 0, 2)

        # Same distribution but mixed
        dm = dd.nearest_neighbor(x, y)
        aaeq(dm, 0.5, 1)

        # Two completely different distributions
        dm = dd.nearest_neighbor(x + 10, y)
        aaeq(dm, 1, 2)

    def test_compare_with_matlab(self):
        x, y = matlab_sample()
        dm = dd.nearest_neighbor(x, y)
        aaeq(dm, 1, 4)


class TestZAE():
    def test_simple(self):
        d = 2
        n, m = 200, 200
        x = np.random.randn(n, d)
        y = np.random.randn(m, d)

        # Almost identical samples
        dm = dd.zech_aslan(x + .001, x)
        assert dm < 0

    def test_compare_with_matlab(self):
        x, y = matlab_sample()
        dm = dd.zech_aslan(x, y)
        aaeq(dm, 0.77802, 4)


class TestFR():
    def test_simple(self):
        # Over these 7 points, there are 2 with edges within the same sample.
        # [1,2]-[2,2] & [3,2]-[4,2]
        # |
        # |   x
        # | o o x x
        # | x  o
        # |_ _ _ _ _ _ _
        x = np.array([[1, 2], [2, 2], [3, 1]])
        y = np.array([[1, 1], [2, 4], [3, 2], [4, 2]])

        dm = dd.friedman_rafsky(x, y)
        aaeq(dm, 2. / 7, 3)

    def test_compare_with_matlab(self):
        x, y = matlab_sample()
        dm = dd.friedman_rafsky(x, y)
        aaeq(dm, 0.96667, 4)


class TestKS():
    def test_1D_ks_2samp(self):
        # Compare with scipy.stats.ks_2samp
        x = np.random.randn(50) + 1
        y = np.random.randn(50)
        s, p = stats.ks_2samp(x, y)
        dm = dd.kolmogorov_smirnov(x, y)
        aaeq(dm, s, 3)

    def test_compare_with_matlab(self):
        x, y = matlab_sample()
        dm = dd.kolmogorov_smirnov(x, y)
        aaeq(dm, 0.96667, 4)


# ==================================================================== #
#                       Analytical results
# ==================================================================== #
def analytical_KLDiv(p, q):
    """Return the Kullback-Leibler divergence between two distributions.

    Parameters
    ----------
    p, q : scipy.frozen_rv
      Scipy frozen distribution instances, e.g. stats.norm(0,1)

    Returns
    -------
    out : float
      The Kullback-Leibler divergence computed by numerically integrating
      p(x)*log(p(x)/q(x)).
    """
    func = lambda x: p.pdf(x) * np.log(p.pdf(x) / q.pdf(x))
    a = 1E-5
    return \
    integrate.quad(func, max(p.ppf(a), q.ppf(a)), min(p.isf(a), q.isf(a)))[0]


# ==================================================================== #

@pytest.mark.slow
class TestKLDIV:
    #
    def test_against_analytic(self):
        p = stats.norm(2, 1)
        q = stats.norm(2.6, 1.4)

        ra = analytical_KLDiv(p, q)

        N = 10000
        np.random.seed(2)
        x, y = p.rvs(N), q.rvs(N)

        re = dd.kldiv(p.rvs(N), q.rvs(N))

        aaeq(re, ra, 1)

    def accuracy_vs_kth(self, N=100, trials=100):
        """Evalute the accuracy of the algorithm as a function of k.

        Parameters
        ----------
        N : int
          Number of random samples.
        trials : int
          Number of independent drawing experiments.

        Returns
        -------
        (err, stddev) The mean error and standard deviation around the
        analytical value for different values of k from 1 to 15.
        """
        p = stats.norm(0, 1)
        q = stats.norm(0.2, 0.9)

        k = np.arange(1, 16)

        out = []
        for n in range(trials):
            out.append(dd.kldiv(p.rvs(N), q.rvs(N), k))
        out = np.array(out)

        # Compare with analytical value
        err = out - analytical_KLDiv(p, q)

        # Return mean and standard deviation
        return err.mean(0), err.std(0)

    #
    def check_accuracy(self):
        m, s = self.accuracy_vs_kth(N=500, trials=300)
        aaeq(np.mean(m[0:2]), 0, 2)

    #
    def check_different_sample_size(self):
        p = stats.norm(2, 1)
        q = stats.norm(2.6, 1.4)

        ra = analytical_KLDiv(p, q)

        N = 6000
        # Same sample size for x and y
        re = [dd.kldiv(p.rvs(N), q.rvs(N)) for i in range(30)]
        aaeq(np.mean(re), ra, 2)

        # Different sample sizes
        re = [dd.kldiv(p.rvs(N * 2), q.rvs(N)) for i in range(30)]
        aaeq(np.mean(re), ra, 2)

        re = [dd.kldiv(p.rvs(N), q.rvs(N * 2)) for i in range(30)]
        aaeq(np.mean(re), ra, 2)

    #
    def test_mvnormal(self):
        """Compare the results to the figure 2 in the paper."""
        from numpy.random import normal, multivariate_normal

        N = 30000
        p = normal(0, 1, size=(N, 2))
        np.random.seed(1)
        q = multivariate_normal([.5, -.5], [[.5, .1], [.1, .3]], size=N)

        aaeq(dd.kldiv(p, q), 1.39, 1)
        aaeq(dd.kldiv(q, p), 0.62, 1)


