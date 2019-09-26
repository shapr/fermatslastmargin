var params = new URLSearchParams(document.location.search);
var pagenum = parseInt(params.get("pagenum"), 10) || 1; // this can't be a good idea
if (pagenum < 1) {
    pagenum = 1;
}

var uid = "todo";

function setAnnotation(a) {
    document.getElementById("content").value = a["content"] || ""; // might even work, who knows?
}

// from https://stackoverflow.com/a/32147146/39683
function setHeight(jq_in){
    jq_in.each(function(index, elem){
	// This line will work with pure Javascript (taken from NicB's answer):
	elem.style.height = elem.scrollHeight+'px';
    });
}
setHeight($('#content'));

$(document).ready(function(){
    $("#status").val(hereagain());
    $("#pgup").val(pagenum + 1);
    $("#pgdn").val(pagenum - 1 || 1); // gosh that's ugly
    $.get("/annotate/" + uid + "/" + pagenum, "", setAnnotation, "json");
});

function hereagain() {
    return window.location.protocol + "//" + window.location.host + window.location.pathname;
}

function buildup () {
    return hereagain() + "?pagenum=" + (pagenum + 1) + "&uid=" + uid;

}
function builddown () {
    return hereagain() + "?pagenum=" + (pagenum - 1) + "&uid=" + uid;
}


document.addEventListener('keydown', (e) => {
    content = document.getElementById("content");

    if (e.key == "PageDown") {

	// location.href = "http://localhost:3000/pagedown"
	// location.href = hereagain() + "?pagenum=" + (pagenum + 1) + "&uid=" + uid // "http://localhost:3000/pagedown"
	location.href = builddown();
    }
    if (e.key == "PageUp") {
	if (pagenum >= 1) { location.href = buildup(); // hereagain() + "?pagenum=" + (pagenum - 1) + "&uid=" uid;
			  } else { location.href = hereagain() + "?pagenum=1" + "&uid=" + uid;
				   // location.href = buildurl();
				 }
    }
    if (e.key == "Enter") {
	// check focus
	if (content == document.activeElement) {
	    // content already has focus, enter should submit to the server if the contents are non-empty
	    // submit, check for write, if success then redirect to this page
	    if (content.value.length > 0) { // check for empty textarea, this might work?
		// submit to server
		$.post("/annotate/" + uid + "/" + pagenum, JSON.stringify({ pageNumber: pagenum, content: content.value, paperuid: uid }));
		content.blur();
		// display saved, and drop focus
	    } else { alert("not saving empty annotation"); }
	} else { content.focus(); // put focus on content
		 content.style.height = content.scrollHeight + "px";
	       }
    }
    if (e.key == "Escape") {
	content.blur();
    }
});
