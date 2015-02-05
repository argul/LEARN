require("./types.js");
require("./debug_tool.js");

(function()
{
	this.util = this.util || {};

	util.forArray = function(arr, procedure)
	{
		util.AssertDefined(procedure);
		util.AssertIsTrue(util.isArray(arr));
		var len = arr.length;
		for (var i = 0; i < len; ++i)
		{
			procedure(arr[i]);
		}
	}
})();
