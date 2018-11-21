"""Babylonian Square Root"""

import time
import numpy as np
import seaborn as sns

def sqrt(num):
    "Calculate a square root using the babylonian method, with a given\
number of iterations"
    guess = num
    tmp = 0
    # iterations = 0
    t = time.time()
    while True:
        tmp = 0.5 * (guess + (num / guess))
        # iterations += 1
        if tmp != guess:
            guess = tmp
        else:
            # return iterations
            # return guess
            return (time.time() - t) * 1e8

vals = np.random.uniform(2, 1e16, 100000)
iters = list(map(sqrt, vals))
sns.set(style="ticks")
# sns.scatterplot(x=list(vals), y=iters)
