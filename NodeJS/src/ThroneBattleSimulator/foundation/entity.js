require('../utils/utils.js');

(function()
 {
	var entity_id = 0; 
	this.Entity = Class.extend({
		name : 'Entity',
		id : -1,
		init : function()
		{
			this.id = ++entity_id;
		},
		equal : function(another)
		{
			return this.id === another.id;
		}
	});
 })();
