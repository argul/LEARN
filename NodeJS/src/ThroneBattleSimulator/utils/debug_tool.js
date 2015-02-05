require('./types.js');

(function()
{
	this.util = this.util || {};
	var p = function(str)
	{
		console.log(str);
	}
	var getStack = function()
	{
		var err = new Error();
		var ret = err.stack.split('\n');
		ret.splice(0, 3);
		return ret.join('\n');
	}

	util.print = function(any)
	{
		if (util.isUndefined(any))
		{
			p("undefined");
		}
		else
		{
			p(any.toString());
		}
	}

	util.error = function(reason)
	{
		throw new Error(reason);
	}

	util.stacktrace = function()
	{
		return getStack();
	}

	util.dump = function(any)
	{
		doDump(any);
	}

	util.assertDefined = function(any)
	{
		if (util.isUndefined(any))
		{
			throw new Error();
		}
	}
	util.assertIsTrue = function(any)
	{
		if (true !== any)
		{
			throw new Error();
		}
	}

	util.perf = function(token, procedure)
	{
		//TODO
	}

	var doDump = function(any)
	{
		//TODO
		util.print(any);
	}

	var test_debug_tool = function()
	{
		util.print(util.stacktrace());
	}
	//test_debug_tool();
}
)();
