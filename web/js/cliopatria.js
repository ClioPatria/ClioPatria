/* - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
General JavaScript utilities in use by ClioPatria
- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - */

/* Change the dropped values for "a.rdf-r" resources to drop the
 * resource in a Prolog friendly representation.
 */

$(function() {

  $(document.body).on('dragstart', "a.rdf-r", function(ev) {
    var a = ev.originalEvent.dataTransfer;
    var href = $(ev.target).attr("href");
    var listResource = "/browse/list_resource?r=";
    var start = href.indexOf(listResource);

    if ( start >= 0 ) {
      var r = decodeURIComponent(href.substring(start+listResource.length));
      var b = href.substring(0, start);

      a.setData("Text", r);
      $.ajax({ url: b+"/api/json/resource_representation",
               data:{ r:r,
	              language:"prolog"
	            },
	       async:false,
	       timeout:1000,
	       success:function(data) {
		 a.setData("Text", data);
	       }
             });

      return true;
    }
  });
});
