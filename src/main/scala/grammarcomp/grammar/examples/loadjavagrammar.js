// To run in the chrome console of http://cui.unige.ch/isi/bnf/JAVA/BNFindex.html

function loadjscssfile(filename, filetype){
 if (filetype=="js"){ //if filename is a external JavaScript file
  var fileref=document.createElement('script')
  fileref.setAttribute("type","text/javascript")
  fileref.setAttribute("src", filename)
 }
 if (typeof fileref!="undefined")
  document.getElementsByTagName("head")[0].appendChild(fileref)
}
loadjscssfile("http://code.jquery.com/jquery-2.1.0.min.js","js")

outputelementContent = function(index, element) {
  $.ajax({
    url: element.href,
    async:   false,
    success: function(the_data) {
      var v = $($(the_data)[4]).text();
      console.log( v);
    }
});
}

$("li a").each(outputelementContent);
