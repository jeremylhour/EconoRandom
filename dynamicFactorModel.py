#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Test of dynamic factor models using statsmodels

basically copied/pasted from https://www.statsmodels.org/devel/examples/notebooks/generated/statespace_dfm_coincident.html

Created on Mon Mar 22 19:58:41 2021

@author: jeremylhour
"""
import numpy as np
import pandas as pd
import pandas_datareader as pdr
import statsmodels.api as sm
import matplotlib.pyplot as plt


# ------------------------
# DOWNLOAD FRED DATA
# ------------------------

start = '1979-01-01'
end = '2020-12-01'

industrialProduction = pdr.get_data_fred('IPMAN', start=start, end=end)
income =  pdr.get_data_fred('W875RX1', start=start, end=end)
sales =  pdr.get_data_fred('CMRMTSPL', start=start, end=end)
emp =  pdr.get_data_fred('PAYEMS', start=start, end=end)

df = pd.concat((industrialProduction, income, sales, emp), axis=1)
df.columns = ['indprod', 'income', 'sales', 'emp']
df.index.freq = df.index.inferred_freq


# ------------------------
# PLOT SERIES AND STANDARDIZE
# ------------------------
df.loc[:, 'indprod':'emp'].plot(subplots=True, layout=(2, 2), figsize=(15, 6))

# Create log-differenced series and standardize
for col in df.columns:
    df['dln_'+col] = (np.log(df[col])).diff()
    df['std_'+col] = (df['dln_'+col] - df['dln_'+col].mean()) / df['dln_'+col].std()


# ------------------------
# INITIALIZE AND ESTIMATE MODEL
# ------------------------

endog = df.loc['1979-02-01':, [col for col in df.columns if col[:3] == 'std']] # get rid of first row

# Create the model
params = {
    'k_factors': 1,
    'factor_order': 3,
    'error_order': 2
    }
dfm = sm.tsa.DynamicFactor(endog, **params)

initFit = dfm.fit(method='powell', disp=False) # initial fit
secondFit = dfm.fit(initFit.params, disp=False)

print(secondFit.summary(separate_params=False))

# ------------------------
# PLOT THE FACTOR
# ------------------------

fig, ax = plt.subplots(figsize=(13,3))

# Plot the factor
dates = endog.index._mpl_repr()
ax.plot(dates, secondFit.factors.filtered[0], label='Factor')
ax.legend()

# Retrieve and also plot the NBER recession indicators
recessions = pdr.get_data_fred('USREC', start=start, end=end)
ylim = ax.get_ylim()
ax.fill_between(dates[:-3], ylim[0], ylim[1], recessions.values[:-4,0], facecolor='k', alpha=0.1)


# ------------------------
# COINCIDENT INDEX
# ------------------------

def computeCoincidentIndex(model, result, df):
    """
    computeCoincidentIndex:
        computes coincident index, useful because factors are not identified.

    @param model (statsmodels.tsa.statespace.dynamic_factor.DynamicFactor): the model
    @param result (): result from estimation
    @param df (pd.DataFrame): the pandas datafame used
    """
    # Estimate W(1)
    design = model.ssm['design']
    transition = model.ssm['transition']
    ssKalmanGain = result.filter_results.kalman_gain[:,:,-1]
    kStates = ssKalmanGain.shape[0]

    W1 = np.linalg.inv(np.eye(kStates) - np.dot(
        np.eye(kStates) - np.dot(ssKalmanGain, design),
        transition
    )).dot(ssKalmanGain)[0]

    # Compute the factor mean vector
    factorMean = np.dot(W1, df.loc['1972-02-01':, [col for col in df.columns if col[:3] == 'dln']].mean())

    # Normalize the factor
    factor = result.factors.filtered[0]
    factor *= 1 / np.std(factor)
    if factor[0] < 0:
        factor = -factor

    # Compute the coincident index
    coincidentIndex = np.zeros(model.nobs+1)
    coincidentIndex[0] = 100
    for t in range(model.nobs):
        coincidentIndex[t+1] = coincidentIndex[t] + factor[t] + factorMean

    # Attach dates
    coincidentIndex = pd.Series(coincidentIndex, index=df.index).iloc[1:]
    return coincidentIndex

coincidentIndex = computeCoincidentIndex(model=dfm,
                                         result=secondFit,
                                         df=df)

fig, ax = plt.subplots(figsize=(13,3))

# Plot the index
dates = coincidentIndex.index._mpl_repr()
ax.plot(dates, coincidentIndex, label='Coincident Index')
ax.legend()

# Retrieve and also plot the NBER recession indicators
recessions = pdr.get_data_fred('USREC', start=start, end=end)
ylim = ax.get_ylim()
ax.fill_between(dates[:-3], ylim[0], ylim[1], recessions.values[:-4,0], facecolor='k', alpha=0.1)
