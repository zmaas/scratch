"Experimenting with markov chain monte carlo simulations"

import pymc3 as pm
import numpy as np
from matplotlib import pyplot as plt

count_data = np.loadtxt('txtdata.csv')

print("Setting up model...")
with pm.Model() as model:
    alpha = 1.0 / count_data.mean()

    lambda_1 = pm.Exponential("lambda_1", alpha)
    lambda_2 = pm.Exponential("lambda_2", alpha)

    n_count_data = len(count_data)
    tau = pm.DiscreteUniform("tau", lower=0, upper=n_count_data - 1)

with model:
    print("Create function from our two lambdas...")
    idx = np.arange(n_count_data)  # Index
    lambda_ = pm.math.switch(tau > idx, lambda_1, lambda_2)

    print("Run our observed data through the proposed model...")
    observation = pm.Poisson("obs", lambda_, observed=count_data)

    print("Evaluating model automatically...")
    trace = pm.sample(15000, tune=5000)

    lambda_1_samples = trace['lambda_1']
    lambda_2_samples = trace['lambda_2']
    tau_samples = trace['tau']

print("Showing results...")
pm.traceplot(trace)
