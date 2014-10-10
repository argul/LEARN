var http = require('http');
var ip = "127.0.0.1";
var port = 8080;

http.createServer(function (req, res){
    res.writeHead(200, {'Content-Type': 'text/plain'});
    res.end('Hello World\n');
}).listen(port, ip);
