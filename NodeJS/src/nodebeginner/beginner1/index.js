var server = require("./server.js");
var router = require("./router.js");

server.run(router, 8080)
