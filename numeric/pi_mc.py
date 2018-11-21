"""Calculate pi using Monte Carlo Sampling"""

import numpy as np
import multiprocessing as mp

def in_circle(x, y):
    """Determine if an x,y pair is in the unit circle"""
    if x**2 + y**2 <= 1:
        return True
    else:
        return False


def pi_count(n):
    """Use n samples on a quarter circle to estimate pi"""
    return np.sum(np.less(
        np.add(np.square(np.random.rand(n)), np.square(np.random.rand(n))), 1))

def map_handler(n, cores):
    """Handler for our Monte Carlo Sampling

    :returns: none
    :rtype: none
    """
    pool = mp.Pool(cores)
    return pool.map(pi_count, np.repeat(n, cores))

def reduce_handler(lst, n):
    """Reduce our sum and calculate pi

    :param lst: list to sum
    :returns: int
    :rtype: int

    """
    return 4 * (sum(lst) / n)

def multi_pi():
    """Benchmark the monte carlo method"""
    i = 8
    cores = 4
    n = 10**i
    job_split = int(10**i / cores)
    print(reduce_handler(map_handler(job_split, cores), n))

multi_pi()
