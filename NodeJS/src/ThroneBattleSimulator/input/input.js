require('../utils/utils.js')
(function()
{
	this.input = this.input || {};
	input.parse = function(data)
	{
		var ret;
		if (util.isString(data))
		{
			ret = JSON.parse(data);
		}
		else //TODO : binaryArray
		{
			ret = data;
		}
		util.AssertIsTrue(doCheck(ret));
		return ret;
	}

	var doCheck = function(input)
	{
		//TODO
		return true;
	}
})();
