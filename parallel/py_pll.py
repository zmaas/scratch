"""Example of using python multiprocessing pools"""

import multiprocessing as mp

def map_worker(num):
    """Just return the input

    :param num:
    :returns:
    :rtype:

    """
    return num

def map_handler():
    """Trivial handler for multiprocessing

    :returns: none
    :rtype: none
    """
    pool = mp.Pool(4)
    data = range(1000)
    return pool.map(map_worker, data)

def reduce_handler(lst):
    """Basic sum reduce...

    :param lst: list to sum
    :returns: int
    :rtype: int

    """
    return sum(lst)

print(reduce_handler(map_handler()))
