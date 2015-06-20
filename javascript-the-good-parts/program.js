document.writeln('Hello, world!')
//var a = implements.q;

//(function () {
//    return;
//        asd;
//})()

//function () {
//    var a = 1;
//    var b = 2;
//    a === b;
//    var c = 3;
//}

var stooge = {
    'first-name'  : "Stooge",
    'middle-name' : "Lester"
};

document.writeln("// all object properties");
for (name in stooge) {
    document.writeln(name);
}

document.writeln("// own object properties");
for (name in stooge) {
    if (! stooge.hasOwnProperty(name))
        continue;
    document.writeln(name);
}
document.writeln("");

var add = function (a, b) {
    "use strict";
    if (typeof a !== 'number' || typeof b !== 'number') {
        throw {
            name: 'TypeError',
            message: 'add needs numbers'
        };
    }
    return a + b;
}

// Make a try_it function that calls the new add
// function incorrectly.
var tryIt = function () {
    try {
        add("seven");
    } catch (e) {
        document.writeln(e.name + ': ' + e.message);
    }
}

//tryIt();

Function.prototype.method = function (name, func) {
    this.prototype[name] = func;
    return this;
};

//Number.method('integer', function ( ) {
//    return Math[this < 0 ? 'ceiling' : 'floor'](this);
//});
//document.writeln((-10 / 3).integer());

// Define a function that sets a DOM node's color
// to yellow and then fades it to white.
var fade = function (node) {
    var level = 1;
    var step = function () {
        var hex = level.toString(16);
        node.style.backgroundColor = '#FFFF' + hex + hex;
        if (level < 15) {
            level += 1;
            setTimeout(step, 100);
        }
    };
    setTimeout(step, 100);
};
fade(document.body);

// module pattern

var serial_maker = function () {

    // Produce an object that produces unique strings. A
    // unique string is made up of two parts: a prefix
    // and a sequence number. The object comes with
    // methods for setting the prefix and sequence
    // number, and a gensym method that produces unique
    // strings.

    var prefix = '';
    var seq = 0;
    return {
        set_prefix: function (p) {
            prefix = String(p);
        },
        set_seq: function (s) {
            seq = s;
        },
        gensym: function () {
            var result = prefix + seq;
            seq += 1;
            return result;
        }
    };
};

var seqer = serial_maker();
seqer.set_prefix('Q');
seqer.set_seq(1000);
var unique1 = seqer.gensym(); // unique is "Q1000"
var unique2 = seqer.gensym(); // unique is "Q1000"
document.writeln("unique1: " + unique1);
document.writeln("unique2: " + unique2);
