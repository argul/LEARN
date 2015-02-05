require('./types');

(function()
{
	this.util = this.util || {};

	util.random = function(a, b)
	{
		var seed = Math.random();
		if (util.isNumber(a) && util.isNumber(b))
		{
			return Math.floor(seed * (b - a + 1) + a);
		}
		else if (util.isNumber(a))
		{
			return Math.floor(seed * (a + 1));
		}
		else
		{
			return 0 === Math.floor(seed * 2); 
		}
	}
})();
