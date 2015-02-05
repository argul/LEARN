require('./simple_throne_rule.js');

(function()
{
	this.rule = this.rule || {};

	rule.createInstance = function(rule_type)
	{
		switch (rule_type)
		{
			case "simple_throne":
			{
				return new SimpleThroneRule();
			}
			break;
		}
	}
})();
