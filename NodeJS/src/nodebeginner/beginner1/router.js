var handlers = require("./requestHandlers.js");
var url = require("url");

var handlerMap = {}
handlerMap["/"] = handlers.list;
handlerMap["/start"] = handlers.start;
handlerMap["/upload"] = handlers.upload;
handlerMap["/show"] = handlers.show;

function route(request, response){
	var pathname = url.parse(request.url).pathname;
	var handler = handlerMap[pathname];
	if ('function' === typeof handler){
		return handler(request, response);
	}else{
		response.writeHead(404, {"Content-Type" : "text/plain"});
		response.write("404 Not found");
		response.end();
	}
}

exports.route = route;
