import pandas as pd
import pickle


dataset = pd.read_csv('wine.csv')


X = dataset.iloc[:, :13]
y = dataset.iloc[:, -1]

from sklearn.ensemble import RandomForestClassifier
rf = RandomForestClassifier()

#Fitting model with training data
rf.fit(X, y)

# Saving model to disk
pickle.dump(rf, open('model_classfication.pkl','wb'))