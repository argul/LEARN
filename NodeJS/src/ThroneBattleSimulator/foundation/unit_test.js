require('./entity.js');

(function()
 {
	var unit_test = function()
	{
		var e1 = new Entity();
		var e2 = new Entity();

		util.print(e1.id);
		util.print(e2.id);
		util.print(e1 == e2);
		e2.id = e1.id;
		util.print(e1 == e2);
		util.print(e1 === e2);
		util.print(e1.equal(e2));

		var Test = Entity.extend({
			init : function()
			{
				this._super();
				util.print("init of Test is called!");
			}
		});

		var t1 = new Test();
		var t2 = new Test();
		util.print(t1.id);
		util.print(t2.id);
		util.print(t1.equal(t2));
	}
	unit_test();
 })();
