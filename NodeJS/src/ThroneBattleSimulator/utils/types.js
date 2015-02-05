(function ()
{
	this.util = this.util || {};
	var p = function(any) { console.log(any); };
	var protoStr = function(any)
	{
		var meta = Object.prototype.toString.call(any);
		return meta.substring(8, meta.length - 1);
	}
	
	util.isUndefined = function(any)
	{
		return "Undefined" === protoStr(any);
	}

	util.isNull = function(any)
	{
		return "Null" === protoStr(any);
	}

	util.isString = function(any)
	{
		return "String" === protoStr(any);
	}

	util.isNumber = function(any)
	{
		return "Number" === protoStr(any);
	}
	
	util.isArray = function(any)
	{
		return "Array" === protoStr(any);
	}

	util.isObject = function(any)
	{
		return "Object" === protoStr(any);
	}

	var isBoolean = function(any)
	{
		return "Boolean" == protoStr(any);
	}
	var isBooleanEqual = function(any, b)
	{
		if (isBoolean(any))
		{
			return b == any;
		}
		else
		{
			return false;
		}
	}

	util.isTrue = function(any)
	{
		return isBooleanEqual(any, true);
	}

	util.isFalse = function(any)
	{
		return isBooleanEqual(any, false);
	}

	util.isNaN = function(any)
	{
		return any !== any;
	}
})();
