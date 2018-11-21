#/env python3

# Author: Zachary Maas

import math
import time

# Global cache to store binomial coefficent values.
cache = {}


def binom(n, k):
    """
    Calculate the binomial coefficent using a cache. The speedup that
    we get from using a cache grows as we increase the number of
    values in the cache.
    """

    # Make sure that k is not greater than n, since that leads to a
    # negative factorial.
    if n < k:
        raise ValueError("Binomial coefficent not defined for k > n")

    # Make the key using our two input values
    key = str(n) + "-" + str(k)

    # Request the key, return the cached value if we have it
    if key in cache:
        return cache[key]

    # If the key isn't in the cache, we need to calculate the binomial
    # coefficent and add it to our cache.
    val = math.factorial(n) / (math.factorial(k) * (math.factorial(n - k)))
    cache[key] = val
    return val


### Helper Function
def binom_test():
    """
    Test our binomial function to see how it works before and after caching.
    Takes about 20 seconds to run the test on my computer.
    """

    def loop_iter():
        """
        Runs our loop for all values from 0 to 1000.
        """

        # The highest value to iterate up to.
        maximum = 1000

        # Start the timer
        t1 = time.time()

        # Loop over all possible binomial coefficents below the limit.
        # O(n²) growth
        for i in range(maximum):
            for j in range(i):
                binom(i, j)

        # Calculate elapsed time
        elapsed = time.time() - t1
        print("Took:", elapsed)
        return elapsed

    # Reset the cache
    cache = {}

    # Time the first loop iteration
    fst = loop_iter()

    # Time the second loop iteration
    snd = loop_iter()

    # Print the speedup
    print("Caching gave us a", fst / snd, "times speedup.")
