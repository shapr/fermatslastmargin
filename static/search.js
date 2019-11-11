// stolen from https://stackoverflow.com/a/31113246/39683
function postSelectedPapers() {
    // get the checked boxes
    var checkedBoxes = document.querySelectorAll('input[name=selectedpaper]:checked');
    var body = JSON.stringify( Array.from(checkedBoxes).map(e => JSON.parse( e.value)));
    $.post("/newpaper", body, goHome, ""); // on success, go back to front page
}

function postEditedPaper() {
    // get the checked boxes
    var theDoi = document.getElementById("doi");
    var theAuthor = document.getElementById("author");
    var pubDate = document.getElementById("published");
    var theTitle = document.getElementById("title");
    var body = JSON.stringify( {
	uid: theDoi.innerHTML,
	author: theAuthor.value,
	published: pubDate.value,
	title: theTitle.value,
	notes: [],
    });
    $.post("/editmetadata", body, goHome, ""); // on success, go back to front page
}

function goHome() {
    location.href = "/";
}

$(document).ready(() => {
    $("#save").click(postSelectedPapers);
    $("#editmetadata").click(postEditedPaper);
});
