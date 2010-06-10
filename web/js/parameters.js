/**
 * Returns a query parameter string
 *
 * @method queryString
 * @private
 * @param {String} key
 * @param {String} value
 * @param {Array} value
 * return {String}
 */
var queryString = function(key, value) {
	var sQueryString = '';
	if(key=="filter") {
		if(value&&value.length>0) {
			var sValue = YAHOO.lang.JSON.stringify(value);
		    	sQueryString += encodeURIComponent(key)+"=";
		    	sQueryString += encodeURIComponent(sValue);
		}
    }
	else if(value&&value.constructor==Object) {
		var sValue = YAHOO.lang.JSON.stringify(value);
    	sQueryString += encodeURIComponent(key)+"=";
    	sQueryString += encodeURIComponent(sValue);
    }
	else if(value&&value.constructor==Array) {
		for (var i=0; i < value.length; i++) {
			sQueryString += queryString(key, value[i]);
			if(i<value.length-1) {
			    sQueryString += "&";
			}
		}
	}
	else if(value===false) {
		sQueryString = encodeURIComponent(key)+"=false";
	}
	else if(value) {
		sQueryString = encodeURIComponent(key)+"="+encodeURIComponent(value);
	}
	return sQueryString;
};

/**
 * Initializes query parameters which are appended to the http query
 *
 * @method initQueryAppend
 * @private
 * return {String} queryAppend
 */
var initQueryAppend = function(oParams) {
	var sScriptQuery = '';
	// Add all params to sScriptQuery
	for(var sParam in oParams) {
		if(sParam) {
		    var sPair = queryString(sParam, oParams[sParam]);
		    if(sPair) {
		        sScriptQuery += sPair+"&";
		    }    
		}
	}
	return sScriptQuery;
};

function PageQuery(q) {
    if(q.length > 1) {
        this.q = q.substring(1, q.length);
    }
    else {
        this.q = null;
    }
    this.keyValuePairs = [];
    if(q) {
        for(var i=0; i < this.q.split("&").length; i++) {
            this.keyValuePairs[i] = this.q.split("&")[i];
        }
    }
    
    this.getKeyValuePairs = function() {
        return this.keyValuePairs;
    };
    this.getValue = function(s) {
        for(var j=0; j < this.keyValuePairs.length; j++) {
            if(this.keyValuePairs[j].split("=")[0] == s) {
                return this.keyValuePairs[j].split("=")[1];
            }
        }
        return false;
    };
    this.getParameters = function() {
        var a = [];
        for(var j=0; j < this.keyValuePairs.length; j++) {
            a[j] = this.keyValuePairs[j].split("=")[0];
        }
        return a;
    };
    this.replaceParameter = function(key, value) {
        var sQueryString = queryString(key, value);
        for(var j=0; j < this.keyValuePairs.length; j++) {
            if(this.keyValuePairs[j].split("=")[0] == key) {
                this.keyValuePairs[j] = sQueryString;
                return true;
            }
        }
        this.keyValuePairs.push(sQueryString);
    };
    this.addParameter = function(key, value) {
        var sQueryString = queryString(key, value);
        this.keyValuePairs.push(sQueryString);
        return true;
    };
    this.createQueryString = function(encode) {
        var sQueryString = '';
        if(this.keyValuePairs.length>0) {
            sQueryString = "?";
            for(var j=0; j < this.keyValuePairs.length; j++) {
                if(j>0) {
                    sQueryString += '&';
                }
                if(encode) {
                    var pair = this.keyValuePairs[j].split("=");
                    sQueryString += queryString(pair[0],pair[1]);
                }
                else {
                    sQueryString += this.keyValuePairs[j];
                }
            }
        }
        return sQueryString;
    };
    
    this.getLength = function() {
        return this.keyValuePairs.length;
    };
}
