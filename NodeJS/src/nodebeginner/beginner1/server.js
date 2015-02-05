var http = require("http");

function run(router, port){
	function onRequest(request, response){
		console.log("incoming url : " + request.url);
		router.route(request, response);
	}

	http.createServer(onRequest).listen(port)
}

exports.run = run;
