// scrape whistler snow data

var webPage = require('webpage');
var page = webPage.create();

var fs = require('fs');
var path = '/tmp/whistler.html';

var myurl = 'https://www.whistlerblackcomb.com/mountain-info/snow-report/';

page.open(myurl, function (status) {
  if(status==="success"){
    console.log("SUCCESS.");
    var content = page.content;
    fs.write(path,content,'w');
    phantom.exit(0);
  } else {
    console.log("FAILED");
    phantom.exit(1);
  }
});