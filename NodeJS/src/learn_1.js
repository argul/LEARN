(function(){
    try {
        //{ "x" : 5 };
        console.log({ "x" : 5 });
    } catch (err) {
        console.log(err);    
    }
}());

(function(){
    var foo = function(){
        bar();
    };
    var bar = function(){
        debugger;
    };
    foo();
}());
