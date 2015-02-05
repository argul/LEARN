var exec = require("child_process").exec;
var formidable = require("formidable");
var querystring = require("querystring");
var fs = require("fs");

function list(request, response){
	exec("ls -lah", function(error, stdout, stderr){
		response.write(stdout);
		response.end()
	});
}

function start(request, response){
	var body = '<html>'+
	    '<head>'+
		'<meta http-equiv="Content-Type" '+
		'content="text/html; charset=UTF-8" />'+
		'</head>'+
		'<body>'+
		'<form action="/upload" enctype="multipart/form-data" '+
		'method="post">'+
		'<input type="file" name="upload">'+
		'<input type="submit" value="Upload file" />'+
		'</form>'+
		'</body>'+
		'</html>';

	response.writeHead(200, {"Content-Type": "text/html"});
	response.write(body);
	response.end();
}

function upload(request, response){
	var form = new formidable.IncomingForm();

	form.parse(request, function(error, fields, files){
		fs.renameSync(files.upload.path, "./tmp/test.png");
		response.writeHead(200, {"Content-Type": "text/html"});
		response.write("received image:<br/>");
		response.write("<image src='/show' />");
		response.end();
	});
}

function show(request, response){
	fs.readFile("./tmp/test.png", "binary", function(err, file){
		if (err){
			response.writeHead(500, {"Content-Type": "text/plain"});
			response.write(err + "\n");
			response.end();
		}else{
			response.writeHead(200, {"Content-Type": "image/png"});
			response.write(file, "binary");
			response.end();
		}
	});
}

exports.list = list;
exports.start = start;
exports.upload = upload;
exports.show = show;
