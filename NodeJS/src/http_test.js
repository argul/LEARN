var http = require('http');
var util = require("util");
var ip = "127.0.0.1";
var port = 8080;

http.createServer(function (req, res){
    console.log(util.inspect(req));
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Hello World\n');
    console.log(util.inspect(res));
}).listen(port, ip);
