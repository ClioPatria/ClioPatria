function initApiPanel() {
    apiPanel = new YAHOO.widget.Panel("win", {
		//modal:true,
        underlay:"shadow",
        close:true,
        visible:false
	});
    apiPanel.setHeader("autocomplete API test");
    apiPanel.setBody("");
    apiPanel.render(document.body);
}

function apiTest(elInput, sPath) {
    if ( elInput ) {
	var search = YAHOO.util.Dom.get(elInput).value;
        apiRequest(sPath, search);
    } else {
  	apiRequest(sPath, "");
    }
};

function apiRequest(sPath, sSearchString) {
    var sLink = sPath;
    if ( sSearchString != "" ) {
        if ( sSearchString.substr(1)!="?") {
        sSearchString = "?"+sSearchString;
	}
        var params = new PageQuery(sSearchString);
        sLink = sLink + params.createQueryString(true)+"&indent=true";
    }

    function successHandler(oResponse) {
        sHTML  = "<div class='url'>";
        sHTML += sLink;
        sHTML += "</div>";

        sHTML += "<div class='result' style='max-width:800px;max-height:500px;overflow:auto'>\n";
//	if ( oResponse.responseText.match(/<html>(.|\n)*<\/html>/im) ) {
	if ( oResponse.responseText.match(/<html>/i) ) {
	    sHTML += oResponse.responseText;
	} else {
	    sHTML += "<pre><code>"
	    sHTML += oResponse.responseText;
	    sHTML += "</div></code></pre>";
	}

        apiPanel.setBody(sHTML);
        apiPanel.center();
        apiPanel.show();
    }
    function failureHandler(oResponse) {
	    YAHOO.util.Dom.get("api_result").innerHTML = oResponse.responseText;
    }
    var oCallback = {
		success:successHandler,
		failure:failureHandler
	};
	var request = YAHOO.util.Connect.asyncRequest('GET', sLink, oCallback);
}
