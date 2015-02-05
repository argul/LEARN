require('./types.js');
require('./debug_tool.js');
require('./random.js');
require('./object_oriented.js');

//console.log(util);
//console.log(Class);
(function()
{
	var f1 = function()
	{
		util.print(util.random(100, 200));
	}
	var f2 = function()
	{
		util.print(util.random(100));
	}
	var f3 = function()
	{
		util.print(util.random());
	}
	for (var i = 0; i < 10; ++i)
	{
		f1();
	}
	for (var i = 0; i < 10; ++i)
	{
		f2();
	}
	for (var i = 0; i < 10; ++i)
	{
		f3();
	}
})();
