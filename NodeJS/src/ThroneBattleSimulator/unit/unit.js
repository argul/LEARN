require('../utils/utils.js');
require('../foundation/entity.js');

(function()
{
	var state_enum = {
		ERROR : "error",
		IDLE : "idle",
		ATTACK : "attack",
		HITTED : "hitted",
		CAST : "cast",
		DEAD : "dead"
	};

	var doFrameUpdate = function()
	{
		
	}

	this.Unit = Entity.extend({
		attr : undefined,
		faction_id : undefined,
		ai : undefined,
		state : state_enum.IDLE,
		skills : undefined,
		effects : undefined,
		space_info : undefined,
		frameUpdate : doFrameUpdate
	});

	Unit.StateEnum = state_enum;

	util.print("unit module loaded");
})();
