"""Simple Linear Regression"""
import numpy as np
import seaborn as sns

x = np.transpose(np.linspace(1, 15, 100))
y = 2*x + (x + np.random.rand(len(x)))**2

A = np.vstack((np.ones(len(x)), x)).T
b = np.linalg.lstsq(A, y)[0]

sns.scatterplot(x, y)
sns.scatterplot(x, np.dot(A, b))
