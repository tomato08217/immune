import pandas as pd
from sklearn.ensemble import RandomForestClassifier
from sklearn.model_selection import train_test_split
import matplotlib.pyplot as plt
from sklearn.metrics import roc_curve
from sklearn.metrics import make_scorer
from sklearn.metrics import accuracy_score
from sklearn.model_selection import GridSearchCV
from sklearn.metrics import roc_auc_score

data = pd.read_csv("sp_reduced1.csv")

X = data.iloc[:, 4:]
X = X.dropna(axis=1, how='any')
y = data.loc[:, "label1"]
randomstate=1000
Xtrain, Xtest, Ytrain, Ytest = train_test_split(X, y, test_size=0.3,randomstate=randomstate)

#grid search
best_auc_index = []
best_auc_index_t = []
# GridSearch for best parameters
for i in range(0, 40, 1):
    scoring = {'AUC': 'roc_auc', 'Accuracy': make_scorer(accuracy_score)}
    parameters = {"n_estimators": [*range(2, 6)],
                  "max_depth": [*range(1, 3)],
                  'max_features': [*range(1, 5)]
                  }
    clf = RandomForestClassifier(random_state=i, criterion="gini")
    gs = GridSearchCV(clf,
                      parameters,
                      scoring=scoring,
                      refit='AUC',
                      return_train_score=True,
                      n_jobs=-1)
    gs.fit(Xtrain, Ytrain)

    y_predict_train = gs.predict_proba(Xtrain)[:, 1]
    y_predict_test = gs.predict_proba(Xtest)[:, 1]

    fpr, tpr, thresholds = roc_curve(Ytrain, y_predict_train)
    fpr_t, tpr_t, thresholds_t = roc_curve(Ytest, y_predict_test)

    best_auc_index.append(roc_auc_score(Ytrain, y_predict_train))
    best_auc_index_t.append(roc_auc_score(Ytest, y_predict_test))

scoring = {'AUC': 'roc_auc', 'Accuracy': make_scorer(accuracy_score)}
parameters = {"n_estimators": [*range(2, 6)],
              "max_depth": [*range(1, 3)],
              'max_features': [*range(1, 5)]
              }
clf = RandomForestClassifier(random_state=25, criterion="gini")
gs = GridSearchCV(clf,
                  parameters,
                  scoring=scoring,
                  refit='AUC',
                  return_train_score=True,
                  n_jobs=-1)
gs.fit(Xtrain, Ytrain)

y_predict_train = gs.predict_proba(Xtrain)[:, 1]
y_predict_test = gs.predict_proba(Xtest)[:, 1]

fpr, tpr, thresholds = roc_curve(Ytrain, y_predict_train)
fpr_t, tpr_t, thresholds_t = roc_curve(Ytest, y_predict_test)

train_roc = roc_auc_score(Ytrain, y_predict_train)
test_roc = roc_auc_score(Ytest, y_predict_test)


RF = RandomForestClassifier(bootstrap=True, ccp_alpha=0.0, class_weight=None,
                            criterion='gini', max_depth=4, max_features=9,
                            max_leaf_nodes=None, max_samples=None,
                            min_impurity_decrease=0.0, min_impurity_split=None,
                            min_samples_leaf=7, min_samples_split=2,
                            min_weight_fraction_leaf=0.0, n_estimators=19,
                            n_jobs=None, oob_score=False, random_state=25, verbose=0,
                            warm_start=False)
RF = RF.fit(Xtrain, Ytrain)
gs.best_params_
# model performance
y_predict_train = RF.predict_proba(Xtrain)[:, 1]
y_predict_test = RF.predict_proba(Xtest)[:, 1]

roc_auc = roc_auc_score(Ytrain, y_predict_train)
roc_auc_t = roc_auc_score(Ytest, y_predict_test)
df = pd.DataFrame(y_predict_train)

df.to_csv('1.csv')

df = pd.DataFrame(Ytrain)

df.to_csv('2.csv')

df = pd.DataFrame(y_predict_test)

df.to_csv('3.csv')

df = pd.DataFrame(Ytest)

df.to_csv('4.csv')

train = train.copy()

train["risk_score"] = y_predict_train 

train.to_csv("train_riskscore.csv")

test = test.copy()

test["risk_score"] = y_predict_test 

test.to_csv("test_riskscore.csv")