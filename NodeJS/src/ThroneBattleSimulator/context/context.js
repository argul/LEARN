require('../ai/ai.js');
require('../unit/unit.js');
require('../skill/skill.js');
require('../utils/utils.js');
require('../effect/effect.js');
require('../spatialization/space.js');

(function()
{
	this.context = this.context || {};

	var Context = Class.extend({
		rule : undefined,
		units : [],
		unitDict : {},
		frameCount : 0,
		factions : [],
		field : {},
		effects : [],
		flow : undefined
	});

	context.createContext = function(input)
	{
		var rule_type = input.rule_type;
		var ret = new Context();

		ret.rule = createRule(rule_type);
		util.forArray(input.units, function(u)
		{
			ret.units.push(createUnit(u, rule_type));
		});
		util.forArray(ret.units, function(u)
		{
			ret.unitDict[u.id] = u;	
		});
		
		ret.factions = input.factions;

		ret.field = createBattleField(rule_type);

		util.forArray(input.global_effects, function(e)
		{
			ret.effects.push(effect.createInstance(e));
		});

		ret.flow = flow_archive.createNew();
	}

	var createRule = function(rule_type)
	{
		return rule.createInstance(rule_type);
	}

	var createUnit = function(unit_data, rule_type)
	{
		var u = new Unit(ud);

		u.attr = createAttrCluster(unit_data);
		u.ai = createAI(unit_data, rule_type);
		u.skills = createSkills(unit_data.skills);
		u.effets = [];
		u.space_info = space.createInstance(unit_data.pos, rule_type);

		u.faction_id = unit_data.faction_id;
		return u;
	}

	var createAttrCluster = function(ud)
	{
		var attr = {};
		var base = {};
		base.hp = ud.hp;
		base.atk = ud.atk;
		base.def = ud.def;
		base.speed = ud.speed;
		base.range = ud.range;

		attr.number = ud.number;
		attr.cur_hp = attr.number * base.hp;
		attr.cur_atk = base.atk;
		attr.cur_def = base.def;
		attr.cur_speed = base.speed;
		attr.cur_range = base.range;

		attr.base = base;

		return attr;
	}
	var createAI = function(ud, rule_type)
	{
		var ai_type;
		if (ud.type === "horseman")
		{
			ai_type = "melee";
		}
		else if (ud.type === "footman")
		{
			ai_type = "melee";
		}
		else if (ud.type === "marksman")
		{
			ai_type = "ranged";
		}
		else if (ud.type === "siege_machine")
		{
			ai_type = "siege";
		}
		else if (ud.type === "wall")
		{
			ai_type = "wall";
		}
		else if (ud.type === "arrow_tower")
		{
			ai_type = "tower";
		}
		return ai.createInstance(rule_type, ai_type);
	}
	var createSkill = function(sks)
	{
		var ret = [];
		util.forArray(sks, function(s)
		{
			ret.push(skill.create(s));
		});
		return ret;
	}

	var createBattleField = function(rule_type)
	{
		return battle_field.createInstance(rule_type);	
	}

	util.print("context module loaded");
})();
