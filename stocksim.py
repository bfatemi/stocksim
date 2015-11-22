import urllib2
import urllib
import json

def execute(ticker):
	if ticker is None:
		raise ValueError('expected ticker to be not None')
	
	headers = {
		'Content-Type':'application/x-www-form-urlencoded; charset=utf-8'
	}

	ticker = ticker.split(',')	
	csvinputs = ','.join(sum( map(list, zip( ('symbols',)*len(ticker) , ticker)), []))

	data = {
		#"inputs":[{
    		#	"name": "symbols",
    		#	"type": "vector",
    		#	"rclass": "character",
    		#	"value": ticker
		#}],
		"csvinputs":csvinputs,
		"format":"json",
		"project": "PROJECT-4668b9fe-fa32-4f0e-9561-9e323f6082ab",
		"enableConsoleEvents": True,
		"author": "bfatemi",
		"filename": "v2_stcksim.R",
		"directory": "root",
		"robjects": "htmlcode",
	}
	
	req = urllib2.Request("http://localhost:7400/deployr/r/repository/script/execute",data=urllib.urlencode(data),headers=headers)
	resp = urllib2.urlopen(req)
	if resp is None:
		return None
	
	#turn json object into python nested dictionary
	respobj = json.loads(resp.read())
	if respobj is None:
		return None
	
	#grab the list containing the html
	htmlcode = respobj.get('deployr',{}).get('response',{}).get('workspace',{}).get('objects')[0].get('value')
	
	html = ''.join(htmlcode)
	return html	
