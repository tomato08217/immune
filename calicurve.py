# -*- coding: utf-8 -*-
"""
Created on Thu Feb  6 10:46:27 2020

@author: z003w4bu
"""

import matplotlib.pyplot as plt
from sklearn.calibration import calibration_curve
from sklearn.metrics import brier_score_loss
import numpy as np
import pandas as pd

data = pd.read_csv('label1_cali_train.csv')

data = data.dropna(axis = 0, how ='any')#delect nan


y_true = data['train']
y_pred = data['pred.train']

a = 4

fraction_of_positives, mean_predicted_value = calibration_curve(y_true, y_pred, n_bins=a)

log_score = brier_score_loss(y_true, y_pred, pos_label=1)


fig = plt.figure(figsize=(10, 10))
ax1 = plt.subplot2grid((3, 1), (0, 0), rowspan=2)
ax2 = plt.subplot2grid((3, 1), (2, 0))

ax1.plot(mean_predicted_value, fraction_of_positives, "s-",
         label="nomogram train (%1.3f)" % log_score)

ax1.plot([0, 1], [0, 1], color='navy', linestyle='--',label="perfect calibrated")

ax2.hist(y_pred, range=(0, 1), bins=a, label="mRMRtrain",
         histtype="step", lw=2)


ax1.set_ylabel("Fraction of positives")
ax1.set_ylim([-0.05, 1.05])
ax1.legend(loc="lower right")
ax1.set_title('Calibration plots  (reliability curve)')

ax2.set_xlabel("Mean predicted value")
ax2.set_ylabel("Count")
ax2.legend(loc="upper center", ncol=2)

plt.tight_layout()
plt.savefig(r'D:\train_label1.pdf')

data = pd.read_csv('label1_cali_test.csv')

data = data.dropna(axis = 0, how ='any')#delect nan


y_true = data['test']
y_pred = data['pred.test']


a = 4

fraction_of_positives, mean_predicted_value = calibration_curve(y_true, y_pred, n_bins=a)

log_score = brier_score_loss(y_true, y_pred, pos_label=1)


fig = plt.figure(figsize=(10, 10))
ax1 = plt.subplot2grid((3, 1), (0, 0), rowspan=2)
ax2 = plt.subplot2grid((3, 1), (2, 0))

ax1.plot(mean_predicted_value, fraction_of_positives, "s-",
         label="nomogram test (%1.3f)" % log_score)

ax1.plot([0, 1], [0, 1], color='navy', linestyle='--',label="perfect calibrated")

ax2.hist(y_pred, range=(0, 1), bins=a, label="mRMRtrain",
         histtype="step", lw=2)


ax1.set_ylabel("Fraction of positives")
ax1.set_ylim([-0.05, 1.05])
ax1.legend(loc="lower right")
ax1.set_title('Calibration plots  (reliability curve)')

ax2.set_xlabel("Mean predicted value")
ax2.set_ylabel("Count")
ax2.legend(loc="upper center", ncol=2)

plt.tight_layout()
plt.savefig(r'D:\test_label1.pdf')
