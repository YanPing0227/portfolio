import numpy as np
from flask import Flask, request,render_template
import pickle

app = Flask(__name__)
model = pickle.load(open('model_classfication.pkl', 'rb'))

@app.route('/')
def home():
    return render_template('html_week4.html')

@app.route('/predict',methods=['POST'])
def predict():
    '''
    For rendering results on HTML GUI
    '''
    features = [x for x in request.form.values()]
    final_features = [np.array(features)]
    prediction = model.predict(final_features)

    output = prediction[0]

    return render_template('html_week4.html', prediction_text='The class of wine should be {}'.format(output))

if __name__ == "__main__":
    app.run(debug=True)