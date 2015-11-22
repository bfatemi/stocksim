from flask import Flask, request
import urllib
import urllib2
import json
import stocksim

app = Flask(__name__)

@app.route('/execute', methods=['POST'])
def index():
	ticker = request.form.get('ticker')
	if ticker is None:
		raise ValueError('Expected ticker value in POST form')
	return stocksim.execute(ticker)

if __name__ == '__main__':
	app.run(host='0.0.0.0',port=5001)
