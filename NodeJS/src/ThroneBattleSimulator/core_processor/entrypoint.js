require('../utils/utils.js');
require('../foundation/foundation.js');
require('../ai/ai.js');
require('../output/output.js');
require('../spatialization/space.js');
require('../effect/effect.js');
require('../rule/rule.js');
require('../unit/unit.js');
require('../context/context.js');
require('../error_handle/error_handle.js');
require('../skill/skill.js');
require('../formula/formula.js');
require('../flow_archive/flow_archive.js');

(function() {
	this.simulator = this.simulator || {};

	simulator.run = function(input_data) {
		var input = input.parse(input_data);

		var ctx = context.createContext(input);

		while (!ctx.rule.isBattleEnd(ctx)) {
			runBattleFrame(ctx);
		}

		var ret = output.getResult(ctx);

		return ret;
	}	

	var runBattleFrame = function(ctx) {
		
	}
})();
