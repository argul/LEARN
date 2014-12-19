// region context
function dumpObj(obj)
{
    if (is_undefined(obj))
    {
        return "undefined";
    }
    else
    {
        return obj.toString();
    }
}

function is_array(arr)
{
    return Object.prototype.toString.call(arr) === '[object Array]';   
}

function is_undefined(any)
{
    return (typeof(any) == "undefined");
}

function is_node_id(any)
{
    return (typeof(any) == "string");
}

function is_node(node)
{
    if (!is_node_id(node.id)) { return false; }
    if (is_undefined(node.reachable_array)) { return false; }

    return true;
}

function error_handler(error_msg)
{
    console.log(error_msg);
    var err = new Error();
    console.log(err.stack);

    throw err;
}

function array_foreach(arr, proc)
{
    if (!is_array(arr))
    {
        error_handler("invalid array! " + dumpObj(arr));
    }
    var len = arr.length;
    for (var i = 0; i < len; ++i)
    {
        proc(arr[i]);
    }
}

function dict_foreach(dict, proc)
{
    for (var key in dict)
    {
        if (dict.hasOwnProperty(key))
        {
            proc(key, dict[key]);
        }
    }
}

function make_context()
{
    var ctx = new Object();
    ctx.error_handler = error_handler;
    ctx.is_array = is_array;
    ctx.is_undefined = is_undefined;
    ctx.is_node = is_node;
    
    var map = {};
    var get_node_proc = function(id)
    {
        var key = id.toString();
        return map[id];
    };

    var add_node_proc = function(node)
    {
        if (!is_node(node))
        {
            error_handler("invalid node! " + dumpObj(node));
        }
        if (!is_undefined(get_node_proc(node.id)))
        {
            error_handler("duplicate node! " + dumpObj(node));
        }
        map[node.id] = node;
    };

    ctx.get_node = get_node_proc;
    ctx.add_node = add_node_proc;

    ctx.start_id = undefined;
    ctx.end_id = undefined;

    ctx.result = [];

    return ctx;
}

function init_context(ctx, node_array, id_start, id_end, distance_evaluator)
{
    if (!ctx.is_array(node_array))
    {
        ctx.error_handler("invalid input node_array! not array form.");
    }
    array_foreach(node_array, function (node) { ctx.add_node(node); });

    if (ctx.is_undefined(ctx.get_node(id_start))
       || ctx.is_undefined(ctx.get_node(id_end)))
    {
        ctx.error_handler("invalid terminals! " + ctx.dumpObj(id_start) + "|" + ctx.dumpObj(id_end));
    }

    ctx.start_id = id_start;
    ctx.end_id = id_end;

    ctx.result = undefined;

    ctx.distance_evaluator = distance_evaluator;
}
// end region

function diagram_router(node_array, id_start, id_end, distance_evaluator)
{
    var ctx = make_context();
    init_context(ctx, node_array, id_start, id_end, distance_evaluator);
    do_diagram_router(ctx);
    return ctx.result;
}

function do_diagram_router(ctx)
{
    var env = {};
    env.opened = {};
    env.is_opened = function (id)
    {
        return !ctx.is_undefined(env.opened[id]);
    };
    env.remove_from_opened = function (id)
    {
        delete env.opened[id];
    };
    env.add_to_opened = function (id, node)
    {
        env.opened[id] = node;
    };

    env.closed = {};
    env.is_closed = function (id)
    {
        return !ctx.is_undefined(env.closed[id]);
    }
    env.add_to_closed = function (id, node)
    {
        env.closed[id] = node;
    };

    env.path_info = {};
    env.current_id = undefined;

    router_startup(ctx, env);

    while (ctx.is_undefined(env.path_info[ctx.end_id])
          && !ctx.is_undefined(env.current_id))
    {
        router_step(ctx, env);
    }

    ctx.result = router_result(ctx, env);

    //console.log(env.path_info);

    env = null;
}

function make_path_info(parent, distance, evaluated_demand)
{
    var info = {};
    info.parent = parent;
    info.distance = distance;
    info.evaluated_demand = evaluated_demand;
    info.get_estimated_distance = function()
    {
        return info.distance + info.evaluated_demand;
    };

    return info;
}

function router_startup(ctx, env)
{
    env.add_to_opened(ctx.start_id, ctx.get_node(ctx.start_id));
    env.current_id = ctx.start_id;

    env.path_info[env.current_id] = make_path_info(undefined, 0, 0);
}

function router_step(ctx, env)
{
    var node = ctx.get_node(env.current_id);
    var current_path_info = env.path_info[env.current_id];

    array_foreach(node.reachable_array, function (reachable)
    {
        var weight = ctx.is_undefined(node.weights) ? 1 : node.weights[reachable.id]; 
        //console.log(reachable.id + " " + env.is_closed(reachable.id) + " " + env.is_opened(reachable.id));
        //console.log(env.opened);
        //console.log(env.closed);
        if (env.is_closed(reachable.id)) { return; }
        if (env.is_opened(reachable.id))
        {
            var n = env.opened[reachable.id];
            var distance = current_path_info.distance + weight;
            var estimate = distance + ctx.distance_evaluator(reachable.id, ctx.end_id);

            var sniffer_path_info = env.path_info[reachable.id]; 
            if (sniffer_path_info.get_estimated_distance() >= estimate)
            {
                sniffer_path_info.parent = node.id;
                sniffer_path_info.distance = distance;
            }
        }
        else
        {
            var new_info = make_path_info(node.id, 
                                          weight + current_path_info.distance,
                                          ctx.distance_evaluator(reachable.id, ctx.end_id));

            env.path_info[reachable.id] = new_info;
            env.add_to_opened(reachable.id, reachable);
        }
    });

    env.remove_from_opened(node.id);
    env.add_to_closed(node.id, node);

    pickup_next_checkpoint(ctx, env);
}

function router_result(ctx, env)
{
    var result = undefined;
    if (ctx.is_undefined(env.path_info[ctx.end_id]))
    {
        result = ["not reachable"];
    }
    else
    {
        result = [];
        var router_id = ctx.end_id;
        do 
        {
            result.push(router_id);
            router_id = env.path_info[router_id].parent;
        } while (!ctx.is_undefined(router_id));
    }
    return result;
}

function pickup_next_checkpoint(ctx, env)
{
    var target_id = undefined;
    dict_foreach(env.opened, function (id, node)
    {
        if (ctx.is_undefined(target_id))
        {
            target_id = id;
        }
        else
        {
            if (env.path_info[target_id].get_estimated_distance() > env.path_info[id].get_estimated_distance())
            {
                target_id = id;
            }
        }
    });
    env.current_id = target_id;
}

// region TEST
function array_to_dict(array)
{
    var dict = {};
    array_foreach(array, function(obj)
    {
        dict[obj.toString()] = true;
    });
    return dict;
}

function generate_node_array(width, height, walls, holes)
{
    var make_id = function(first, second)
    {
        return first + ":" + second;
    };

    var node_array = [];
    var node_dict = {};

    var hole_dict = array_to_dict(holes);
    var wall_dict = array_to_dict(walls);

    for (var i = 1; i <= height; ++i)
    {
        for (var j = 1; j <= width; ++j)
        {
            var node_id = make_id(i, j);
            if (true == hole_dict[node_id])
            {
                continue;
            }
            var n = { id : node_id };
            node_array.push(n);
            n.reachable_array = [];
            node_dict[n.id] = n;
        }
    }

    var filter = function(key, check_x, check_y)
    {
        var check_id = make_id(check_x, check_y);
        if (check_x <= 0 || check_x > height) { return false; }
        if (check_y <= 0 || check_y > width) { return false; }

        if (true == hole_dict[check_id])
        {
            return false;
        }
        if (true == wall_dict[key + "|" + check_id])
        {
            return false;
        }
        if (true == wall_dict[check_id + "|" + key])
        {
            return false;
        }
        return true;
    };

    array_foreach(node_array, function(node)
    {
        var xy = node.id.split(":");
        var x = Number(xy[0]);
        var y = Number(xy[1]);
        var candidates = 
            [[node.id, x+1, y], 
            [node.id, x-1, y], 
            [node.id, x, y+1], 
            [node.id, x, y-1]];
        array_foreach(candidates, function (pair)
        {

            var is_valid = filter(pair[0], pair[1], pair[2]);
            if (is_valid)
            {
                var target_node = node_dict[make_id(pair[1], pair[2])];
                node.reachable_array.push(target_node);
            }
        });
    });

    return node_array;
}

var test_node_array = undefined;
var start = undefined;
var end = undefined;

(function()
{
    var n11 = { id : "1:1" };
    var n12 = { id : "1:2" };
    var n21 = { id : "2:1" };
    var n22 = { id : "2:2" };
    n11.reachable_array = [n12, n21];
    n12.reachable_array = [n11, n22];
    n21.reachable_array = [n11, n22];
    n22.reachable_array = [n12, n21];
    test_node_array = [n11, n12, n21, n22];
    start = "1:1";
    end = "2:2";

    //console.log(test_node_array);
})();


(function(){
    var width = 8;
    var height = 4;
    var walls = ["4:2|3:2", "3:2|3:3", "2:2|2:3", "2:3|1:3", "2:4|1:4", "2:4|2:5", "3:4|3:5", "4:5|3:5", "3:5|2:5", "3:6|2:6", "1:5|1:6", "2:6|1:6", "1:6|1:7"];
    var holes = ["3:7", "3:4"];
    test_node_array = generate_node_array(width, height, walls, holes);

    start = "1:1";
    end = "1:6";

    //console.log(test_node_array);
})();

function global_test()
{
    var result = diagram_router(test_node_array, start, end, function (id1, id2)
                   {
                        var coord1 = id1.split(":");
                        var coord2 = id2.split(":");
                        return Math.abs(Number(coord1[0]) - Number(coord2[0])) + Math.abs(Number(coord1[1]) - Number(coord2[1]));
                   });

    array_foreach(result, function(obj)
                  {
                      console.log(obj.toString() + " , ");
                  });
}


global_test();


// end region
