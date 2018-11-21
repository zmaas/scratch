"Experimenting with markov chain monte carlo simulations"

import pymc3 as pm
import numpy as np
from matplotlib import pyplot as plt

def main():
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

    # print("Extract traces from our model...")
    # lambda_1_samples = trace['lambda_1']
    # lambda_2_samples = trace['lambda_2']
    # tau_samples = trace['tau']

    print("Showing results...")
    pm.traceplot(trace)
    # ax = plt.subplot(311)
    # ax.set_autoscaley_on(False)

    # plt.hist(lambda_1_samples, histtype='stepfilled', bins=30, alpha=0.85,
    #         label="posterior of $\lambda_1$", color="#A60628", normed=True)
    # plt.legend(loc="upper left")
    # plt.title(r"""Posterior distributions of the variables
    #     $\lambda_1,\;\lambda_2,\;\tau$""")
    # plt.xlim([15, 30])
    # plt.xlabel("$\lambda_1$ value")

    # ax = plt.subplot(312)
    # ax.set_autoscaley_on(False)
    # plt.hist(lambda_2_samples, histtype='stepfilled', bins=30, alpha=0.85,
    #         label="posterior of $\lambda_2$", color="#7A68A6", normed=True)
    # plt.legend(loc="upper left")
    # plt.xlim([15, 30])
    # plt.xlabel("$\lambda_2$ value")

    # plt.subplot(313)
    # w = 1.0 / tau_samples.shape[0] * np.ones_like(tau_samples)
    # plt.hist(tau_samples, bins=n_count_data, alpha=1,
    #         label=r"posterior of $\tau$",
    #         color="#467821", weights=w, rwidth=2.)
    # plt.xticks(np.arange(n_count_data))

    # plt.legend(loc="upper left")
    # plt.ylim([0, .75])
    # plt.xlim([35, len(count_data)-20])
    # plt.xlabel(r"$\tau$ (in days)")
    # plt.ylabel("probability");

main()
