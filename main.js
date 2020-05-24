(function(scope){
'use strict';

function F(arity, fun, wrapper) {
  wrapper.a = arity;
  wrapper.f = fun;
  return wrapper;
}

function F2(fun) {
  return F(2, fun, function(a) { return function(b) { return fun(a,b); }; })
}
function F3(fun) {
  return F(3, fun, function(a) {
    return function(b) { return function(c) { return fun(a, b, c); }; };
  });
}
function F4(fun) {
  return F(4, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return fun(a, b, c, d); }; }; };
  });
}
function F5(fun) {
  return F(5, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return fun(a, b, c, d, e); }; }; }; };
  });
}
function F6(fun) {
  return F(6, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return fun(a, b, c, d, e, f); }; }; }; }; };
  });
}
function F7(fun) {
  return F(7, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return fun(a, b, c, d, e, f, g); }; }; }; }; }; };
  });
}
function F8(fun) {
  return F(8, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) {
    return fun(a, b, c, d, e, f, g, h); }; }; }; }; }; }; };
  });
}
function F9(fun) {
  return F(9, fun, function(a) { return function(b) { return function(c) {
    return function(d) { return function(e) { return function(f) {
    return function(g) { return function(h) { return function(i) {
    return fun(a, b, c, d, e, f, g, h, i); }; }; }; }; }; }; }; };
  });
}

function A2(fun, a, b) {
  return fun.a === 2 ? fun.f(a, b) : fun(a)(b);
}
function A3(fun, a, b, c) {
  return fun.a === 3 ? fun.f(a, b, c) : fun(a)(b)(c);
}
function A4(fun, a, b, c, d) {
  return fun.a === 4 ? fun.f(a, b, c, d) : fun(a)(b)(c)(d);
}
function A5(fun, a, b, c, d, e) {
  return fun.a === 5 ? fun.f(a, b, c, d, e) : fun(a)(b)(c)(d)(e);
}
function A6(fun, a, b, c, d, e, f) {
  return fun.a === 6 ? fun.f(a, b, c, d, e, f) : fun(a)(b)(c)(d)(e)(f);
}
function A7(fun, a, b, c, d, e, f, g) {
  return fun.a === 7 ? fun.f(a, b, c, d, e, f, g) : fun(a)(b)(c)(d)(e)(f)(g);
}
function A8(fun, a, b, c, d, e, f, g, h) {
  return fun.a === 8 ? fun.f(a, b, c, d, e, f, g, h) : fun(a)(b)(c)(d)(e)(f)(g)(h);
}
function A9(fun, a, b, c, d, e, f, g, h, i) {
  return fun.a === 9 ? fun.f(a, b, c, d, e, f, g, h, i) : fun(a)(b)(c)(d)(e)(f)(g)(h)(i);
}

console.warn('Compiled in DEV mode. Follow the advice at https://elm-lang.org/0.19.1/optimize for better performance and smaller assets.');


// EQUALITY

function _Utils_eq(x, y)
{
	for (
		var pair, stack = [], isEqual = _Utils_eqHelp(x, y, 0, stack);
		isEqual && (pair = stack.pop());
		isEqual = _Utils_eqHelp(pair.a, pair.b, 0, stack)
		)
	{}

	return isEqual;
}

function _Utils_eqHelp(x, y, depth, stack)
{
	if (depth > 100)
	{
		stack.push(_Utils_Tuple2(x,y));
		return true;
	}

	if (x === y)
	{
		return true;
	}

	if (typeof x !== 'object' || x === null || y === null)
	{
		typeof x === 'function' && _Debug_crash(5);
		return false;
	}

	/**/
	if (x.$ === 'Set_elm_builtin')
	{
		x = $elm$core$Set$toList(x);
		y = $elm$core$Set$toList(y);
	}
	if (x.$ === 'RBNode_elm_builtin' || x.$ === 'RBEmpty_elm_builtin')
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	/**_UNUSED/
	if (x.$ < 0)
	{
		x = $elm$core$Dict$toList(x);
		y = $elm$core$Dict$toList(y);
	}
	//*/

	for (var key in x)
	{
		if (!_Utils_eqHelp(x[key], y[key], depth + 1, stack))
		{
			return false;
		}
	}
	return true;
}

var _Utils_equal = F2(_Utils_eq);
var _Utils_notEqual = F2(function(a, b) { return !_Utils_eq(a,b); });



// COMPARISONS

// Code in Generate/JavaScript.hs, Basics.js, and List.js depends on
// the particular integer values assigned to LT, EQ, and GT.

function _Utils_cmp(x, y, ord)
{
	if (typeof x !== 'object')
	{
		return x === y ? /*EQ*/ 0 : x < y ? /*LT*/ -1 : /*GT*/ 1;
	}

	/**/
	if (x instanceof String)
	{
		var a = x.valueOf();
		var b = y.valueOf();
		return a === b ? 0 : a < b ? -1 : 1;
	}
	//*/

	/**_UNUSED/
	if (typeof x.$ === 'undefined')
	//*/
	/**/
	if (x.$[0] === '#')
	//*/
	{
		return (ord = _Utils_cmp(x.a, y.a))
			? ord
			: (ord = _Utils_cmp(x.b, y.b))
				? ord
				: _Utils_cmp(x.c, y.c);
	}

	// traverse conses until end of a list or a mismatch
	for (; x.b && y.b && !(ord = _Utils_cmp(x.a, y.a)); x = x.b, y = y.b) {} // WHILE_CONSES
	return ord || (x.b ? /*GT*/ 1 : y.b ? /*LT*/ -1 : /*EQ*/ 0);
}

var _Utils_lt = F2(function(a, b) { return _Utils_cmp(a, b) < 0; });
var _Utils_le = F2(function(a, b) { return _Utils_cmp(a, b) < 1; });
var _Utils_gt = F2(function(a, b) { return _Utils_cmp(a, b) > 0; });
var _Utils_ge = F2(function(a, b) { return _Utils_cmp(a, b) >= 0; });

var _Utils_compare = F2(function(x, y)
{
	var n = _Utils_cmp(x, y);
	return n < 0 ? $elm$core$Basics$LT : n ? $elm$core$Basics$GT : $elm$core$Basics$EQ;
});


// COMMON VALUES

var _Utils_Tuple0_UNUSED = 0;
var _Utils_Tuple0 = { $: '#0' };

function _Utils_Tuple2_UNUSED(a, b) { return { a: a, b: b }; }
function _Utils_Tuple2(a, b) { return { $: '#2', a: a, b: b }; }

function _Utils_Tuple3_UNUSED(a, b, c) { return { a: a, b: b, c: c }; }
function _Utils_Tuple3(a, b, c) { return { $: '#3', a: a, b: b, c: c }; }

function _Utils_chr_UNUSED(c) { return c; }
function _Utils_chr(c) { return new String(c); }


// RECORDS

function _Utils_update(oldRecord, updatedFields)
{
	var newRecord = {};

	for (var key in oldRecord)
	{
		newRecord[key] = oldRecord[key];
	}

	for (var key in updatedFields)
	{
		newRecord[key] = updatedFields[key];
	}

	return newRecord;
}


// APPEND

var _Utils_append = F2(_Utils_ap);

function _Utils_ap(xs, ys)
{
	// append Strings
	if (typeof xs === 'string')
	{
		return xs + ys;
	}

	// append Lists
	if (!xs.b)
	{
		return ys;
	}
	var root = _List_Cons(xs.a, ys);
	xs = xs.b
	for (var curr = root; xs.b; xs = xs.b) // WHILE_CONS
	{
		curr = curr.b = _List_Cons(xs.a, ys);
	}
	return root;
}



var _List_Nil_UNUSED = { $: 0 };
var _List_Nil = { $: '[]' };

function _List_Cons_UNUSED(hd, tl) { return { $: 1, a: hd, b: tl }; }
function _List_Cons(hd, tl) { return { $: '::', a: hd, b: tl }; }


var _List_cons = F2(_List_Cons);

function _List_fromArray(arr)
{
	var out = _List_Nil;
	for (var i = arr.length; i--; )
	{
		out = _List_Cons(arr[i], out);
	}
	return out;
}

function _List_toArray(xs)
{
	for (var out = []; xs.b; xs = xs.b) // WHILE_CONS
	{
		out.push(xs.a);
	}
	return out;
}

var _List_map2 = F3(function(f, xs, ys)
{
	for (var arr = []; xs.b && ys.b; xs = xs.b, ys = ys.b) // WHILE_CONSES
	{
		arr.push(A2(f, xs.a, ys.a));
	}
	return _List_fromArray(arr);
});

var _List_map3 = F4(function(f, xs, ys, zs)
{
	for (var arr = []; xs.b && ys.b && zs.b; xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A3(f, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map4 = F5(function(f, ws, xs, ys, zs)
{
	for (var arr = []; ws.b && xs.b && ys.b && zs.b; ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A4(f, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_map5 = F6(function(f, vs, ws, xs, ys, zs)
{
	for (var arr = []; vs.b && ws.b && xs.b && ys.b && zs.b; vs = vs.b, ws = ws.b, xs = xs.b, ys = ys.b, zs = zs.b) // WHILE_CONSES
	{
		arr.push(A5(f, vs.a, ws.a, xs.a, ys.a, zs.a));
	}
	return _List_fromArray(arr);
});

var _List_sortBy = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		return _Utils_cmp(f(a), f(b));
	}));
});

var _List_sortWith = F2(function(f, xs)
{
	return _List_fromArray(_List_toArray(xs).sort(function(a, b) {
		var ord = A2(f, a, b);
		return ord === $elm$core$Basics$EQ ? 0 : ord === $elm$core$Basics$LT ? -1 : 1;
	}));
});



var _JsArray_empty = [];

function _JsArray_singleton(value)
{
    return [value];
}

function _JsArray_length(array)
{
    return array.length;
}

var _JsArray_initialize = F3(function(size, offset, func)
{
    var result = new Array(size);

    for (var i = 0; i < size; i++)
    {
        result[i] = func(offset + i);
    }

    return result;
});

var _JsArray_initializeFromList = F2(function (max, ls)
{
    var result = new Array(max);

    for (var i = 0; i < max && ls.b; i++)
    {
        result[i] = ls.a;
        ls = ls.b;
    }

    result.length = i;
    return _Utils_Tuple2(result, ls);
});

var _JsArray_unsafeGet = F2(function(index, array)
{
    return array[index];
});

var _JsArray_unsafeSet = F3(function(index, value, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[index] = value;
    return result;
});

var _JsArray_push = F2(function(value, array)
{
    var length = array.length;
    var result = new Array(length + 1);

    for (var i = 0; i < length; i++)
    {
        result[i] = array[i];
    }

    result[length] = value;
    return result;
});

var _JsArray_foldl = F3(function(func, acc, array)
{
    var length = array.length;

    for (var i = 0; i < length; i++)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_foldr = F3(function(func, acc, array)
{
    for (var i = array.length - 1; i >= 0; i--)
    {
        acc = A2(func, array[i], acc);
    }

    return acc;
});

var _JsArray_map = F2(function(func, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = func(array[i]);
    }

    return result;
});

var _JsArray_indexedMap = F3(function(func, offset, array)
{
    var length = array.length;
    var result = new Array(length);

    for (var i = 0; i < length; i++)
    {
        result[i] = A2(func, offset + i, array[i]);
    }

    return result;
});

var _JsArray_slice = F3(function(from, to, array)
{
    return array.slice(from, to);
});

var _JsArray_appendN = F3(function(n, dest, source)
{
    var destLen = dest.length;
    var itemsToCopy = n - destLen;

    if (itemsToCopy > source.length)
    {
        itemsToCopy = source.length;
    }

    var size = destLen + itemsToCopy;
    var result = new Array(size);

    for (var i = 0; i < destLen; i++)
    {
        result[i] = dest[i];
    }

    for (var i = 0; i < itemsToCopy; i++)
    {
        result[i + destLen] = source[i];
    }

    return result;
});



// LOG

var _Debug_log_UNUSED = F2(function(tag, value)
{
	return value;
});

var _Debug_log = F2(function(tag, value)
{
	console.log(tag + ': ' + _Debug_toString(value));
	return value;
});


// TODOS

function _Debug_todo(moduleName, region)
{
	return function(message) {
		_Debug_crash(8, moduleName, region, message);
	};
}

function _Debug_todoCase(moduleName, region, value)
{
	return function(message) {
		_Debug_crash(9, moduleName, region, value, message);
	};
}


// TO STRING

function _Debug_toString_UNUSED(value)
{
	return '<internals>';
}

function _Debug_toString(value)
{
	return _Debug_toAnsiString(false, value);
}

function _Debug_toAnsiString(ansi, value)
{
	if (typeof value === 'function')
	{
		return _Debug_internalColor(ansi, '<function>');
	}

	if (typeof value === 'boolean')
	{
		return _Debug_ctorColor(ansi, value ? 'True' : 'False');
	}

	if (typeof value === 'number')
	{
		return _Debug_numberColor(ansi, value + '');
	}

	if (value instanceof String)
	{
		return _Debug_charColor(ansi, "'" + _Debug_addSlashes(value, true) + "'");
	}

	if (typeof value === 'string')
	{
		return _Debug_stringColor(ansi, '"' + _Debug_addSlashes(value, false) + '"');
	}

	if (typeof value === 'object' && '$' in value)
	{
		var tag = value.$;

		if (typeof tag === 'number')
		{
			return _Debug_internalColor(ansi, '<internals>');
		}

		if (tag[0] === '#')
		{
			var output = [];
			for (var k in value)
			{
				if (k === '$') continue;
				output.push(_Debug_toAnsiString(ansi, value[k]));
			}
			return '(' + output.join(',') + ')';
		}

		if (tag === 'Set_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Set')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Set$toList(value));
		}

		if (tag === 'RBNode_elm_builtin' || tag === 'RBEmpty_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Dict')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Dict$toList(value));
		}

		if (tag === 'Array_elm_builtin')
		{
			return _Debug_ctorColor(ansi, 'Array')
				+ _Debug_fadeColor(ansi, '.fromList') + ' '
				+ _Debug_toAnsiString(ansi, $elm$core$Array$toList(value));
		}

		if (tag === '::' || tag === '[]')
		{
			var output = '[';

			value.b && (output += _Debug_toAnsiString(ansi, value.a), value = value.b)

			for (; value.b; value = value.b) // WHILE_CONS
			{
				output += ',' + _Debug_toAnsiString(ansi, value.a);
			}
			return output + ']';
		}

		var output = '';
		for (var i in value)
		{
			if (i === '$') continue;
			var str = _Debug_toAnsiString(ansi, value[i]);
			var c0 = str[0];
			var parenless = c0 === '{' || c0 === '(' || c0 === '[' || c0 === '<' || c0 === '"' || str.indexOf(' ') < 0;
			output += ' ' + (parenless ? str : '(' + str + ')');
		}
		return _Debug_ctorColor(ansi, tag) + output;
	}

	if (typeof DataView === 'function' && value instanceof DataView)
	{
		return _Debug_stringColor(ansi, '<' + value.byteLength + ' bytes>');
	}

	if (typeof File === 'function' && value instanceof File)
	{
		return _Debug_internalColor(ansi, '<' + value.name + '>');
	}

	if (typeof value === 'object')
	{
		var output = [];
		for (var key in value)
		{
			var field = key[0] === '_' ? key.slice(1) : key;
			output.push(_Debug_fadeColor(ansi, field) + ' = ' + _Debug_toAnsiString(ansi, value[key]));
		}
		if (output.length === 0)
		{
			return '{}';
		}
		return '{ ' + output.join(', ') + ' }';
	}

	return _Debug_internalColor(ansi, '<internals>');
}

function _Debug_addSlashes(str, isChar)
{
	var s = str
		.replace(/\\/g, '\\\\')
		.replace(/\n/g, '\\n')
		.replace(/\t/g, '\\t')
		.replace(/\r/g, '\\r')
		.replace(/\v/g, '\\v')
		.replace(/\0/g, '\\0');

	if (isChar)
	{
		return s.replace(/\'/g, '\\\'');
	}
	else
	{
		return s.replace(/\"/g, '\\"');
	}
}

function _Debug_ctorColor(ansi, string)
{
	return ansi ? '\x1b[96m' + string + '\x1b[0m' : string;
}

function _Debug_numberColor(ansi, string)
{
	return ansi ? '\x1b[95m' + string + '\x1b[0m' : string;
}

function _Debug_stringColor(ansi, string)
{
	return ansi ? '\x1b[93m' + string + '\x1b[0m' : string;
}

function _Debug_charColor(ansi, string)
{
	return ansi ? '\x1b[92m' + string + '\x1b[0m' : string;
}

function _Debug_fadeColor(ansi, string)
{
	return ansi ? '\x1b[37m' + string + '\x1b[0m' : string;
}

function _Debug_internalColor(ansi, string)
{
	return ansi ? '\x1b[94m' + string + '\x1b[0m' : string;
}

function _Debug_toHexDigit(n)
{
	return String.fromCharCode(n < 10 ? 48 + n : 55 + n);
}


// CRASH


function _Debug_crash_UNUSED(identifier)
{
	throw new Error('https://github.com/elm/core/blob/1.0.0/hints/' + identifier + '.md');
}


function _Debug_crash(identifier, fact1, fact2, fact3, fact4)
{
	switch(identifier)
	{
		case 0:
			throw new Error('What node should I take over? In JavaScript I need something like:\n\n    Elm.Main.init({\n        node: document.getElementById("elm-node")\n    })\n\nYou need to do this with any Browser.sandbox or Browser.element program.');

		case 1:
			throw new Error('Browser.application programs cannot handle URLs like this:\n\n    ' + document.location.href + '\n\nWhat is the root? The root of your file system? Try looking at this program with `elm reactor` or some other server.');

		case 2:
			var jsonErrorString = fact1;
			throw new Error('Problem with the flags given to your Elm program on initialization.\n\n' + jsonErrorString);

		case 3:
			var portName = fact1;
			throw new Error('There can only be one port named `' + portName + '`, but your program has multiple.');

		case 4:
			var portName = fact1;
			var problem = fact2;
			throw new Error('Trying to send an unexpected type of value through port `' + portName + '`:\n' + problem);

		case 5:
			throw new Error('Trying to use `(==)` on functions.\nThere is no way to know if functions are "the same" in the Elm sense.\nRead more about this at https://package.elm-lang.org/packages/elm/core/latest/Basics#== which describes why it is this way and what the better version will look like.');

		case 6:
			var moduleName = fact1;
			throw new Error('Your page is loading multiple Elm scripts with a module named ' + moduleName + '. Maybe a duplicate script is getting loaded accidentally? If not, rename one of them so I know which is which!');

		case 8:
			var moduleName = fact1;
			var region = fact2;
			var message = fact3;
			throw new Error('TODO in module `' + moduleName + '` ' + _Debug_regionToString(region) + '\n\n' + message);

		case 9:
			var moduleName = fact1;
			var region = fact2;
			var value = fact3;
			var message = fact4;
			throw new Error(
				'TODO in module `' + moduleName + '` from the `case` expression '
				+ _Debug_regionToString(region) + '\n\nIt received the following value:\n\n    '
				+ _Debug_toString(value).replace('\n', '\n    ')
				+ '\n\nBut the branch that handles it says:\n\n    ' + message.replace('\n', '\n    ')
			);

		case 10:
			throw new Error('Bug in https://github.com/elm/virtual-dom/issues');

		case 11:
			throw new Error('Cannot perform mod 0. Division by zero error.');
	}
}

function _Debug_regionToString(region)
{
	if (region.start.line === region.end.line)
	{
		return 'on line ' + region.start.line;
	}
	return 'on lines ' + region.start.line + ' through ' + region.end.line;
}



// MATH

var _Basics_add = F2(function(a, b) { return a + b; });
var _Basics_sub = F2(function(a, b) { return a - b; });
var _Basics_mul = F2(function(a, b) { return a * b; });
var _Basics_fdiv = F2(function(a, b) { return a / b; });
var _Basics_idiv = F2(function(a, b) { return (a / b) | 0; });
var _Basics_pow = F2(Math.pow);

var _Basics_remainderBy = F2(function(b, a) { return a % b; });

// https://www.microsoft.com/en-us/research/wp-content/uploads/2016/02/divmodnote-letter.pdf
var _Basics_modBy = F2(function(modulus, x)
{
	var answer = x % modulus;
	return modulus === 0
		? _Debug_crash(11)
		:
	((answer > 0 && modulus < 0) || (answer < 0 && modulus > 0))
		? answer + modulus
		: answer;
});


// TRIGONOMETRY

var _Basics_pi = Math.PI;
var _Basics_e = Math.E;
var _Basics_cos = Math.cos;
var _Basics_sin = Math.sin;
var _Basics_tan = Math.tan;
var _Basics_acos = Math.acos;
var _Basics_asin = Math.asin;
var _Basics_atan = Math.atan;
var _Basics_atan2 = F2(Math.atan2);


// MORE MATH

function _Basics_toFloat(x) { return x; }
function _Basics_truncate(n) { return n | 0; }
function _Basics_isInfinite(n) { return n === Infinity || n === -Infinity; }

var _Basics_ceiling = Math.ceil;
var _Basics_floor = Math.floor;
var _Basics_round = Math.round;
var _Basics_sqrt = Math.sqrt;
var _Basics_log = Math.log;
var _Basics_isNaN = isNaN;


// BOOLEANS

function _Basics_not(bool) { return !bool; }
var _Basics_and = F2(function(a, b) { return a && b; });
var _Basics_or  = F2(function(a, b) { return a || b; });
var _Basics_xor = F2(function(a, b) { return a !== b; });



var _String_cons = F2(function(chr, str)
{
	return chr + str;
});

function _String_uncons(string)
{
	var word = string.charCodeAt(0);
	return word
		? $elm$core$Maybe$Just(
			0xD800 <= word && word <= 0xDBFF
				? _Utils_Tuple2(_Utils_chr(string[0] + string[1]), string.slice(2))
				: _Utils_Tuple2(_Utils_chr(string[0]), string.slice(1))
		)
		: $elm$core$Maybe$Nothing;
}

var _String_append = F2(function(a, b)
{
	return a + b;
});

function _String_length(str)
{
	return str.length;
}

var _String_map = F2(function(func, string)
{
	var len = string.length;
	var array = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = string.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			array[i] = func(_Utils_chr(string[i] + string[i+1]));
			i += 2;
			continue;
		}
		array[i] = func(_Utils_chr(string[i]));
		i++;
	}
	return array.join('');
});

var _String_filter = F2(function(isGood, str)
{
	var arr = [];
	var len = str.length;
	var i = 0;
	while (i < len)
	{
		var char = str[i];
		var word = str.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += str[i];
			i++;
		}

		if (isGood(_Utils_chr(char)))
		{
			arr.push(char);
		}
	}
	return arr.join('');
});

function _String_reverse(str)
{
	var len = str.length;
	var arr = new Array(len);
	var i = 0;
	while (i < len)
	{
		var word = str.charCodeAt(i);
		if (0xD800 <= word && word <= 0xDBFF)
		{
			arr[len - i] = str[i + 1];
			i++;
			arr[len - i] = str[i - 1];
			i++;
		}
		else
		{
			arr[len - i] = str[i];
			i++;
		}
	}
	return arr.join('');
}

var _String_foldl = F3(function(func, state, string)
{
	var len = string.length;
	var i = 0;
	while (i < len)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		i++;
		if (0xD800 <= word && word <= 0xDBFF)
		{
			char += string[i];
			i++;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_foldr = F3(function(func, state, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		state = A2(func, _Utils_chr(char), state);
	}
	return state;
});

var _String_split = F2(function(sep, str)
{
	return str.split(sep);
});

var _String_join = F2(function(sep, strs)
{
	return strs.join(sep);
});

var _String_slice = F3(function(start, end, str) {
	return str.slice(start, end);
});

function _String_trim(str)
{
	return str.trim();
}

function _String_trimLeft(str)
{
	return str.replace(/^\s+/, '');
}

function _String_trimRight(str)
{
	return str.replace(/\s+$/, '');
}

function _String_words(str)
{
	return _List_fromArray(str.trim().split(/\s+/g));
}

function _String_lines(str)
{
	return _List_fromArray(str.split(/\r\n|\r|\n/g));
}

function _String_toUpper(str)
{
	return str.toUpperCase();
}

function _String_toLower(str)
{
	return str.toLowerCase();
}

var _String_any = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (isGood(_Utils_chr(char)))
		{
			return true;
		}
	}
	return false;
});

var _String_all = F2(function(isGood, string)
{
	var i = string.length;
	while (i--)
	{
		var char = string[i];
		var word = string.charCodeAt(i);
		if (0xDC00 <= word && word <= 0xDFFF)
		{
			i--;
			char = string[i] + char;
		}
		if (!isGood(_Utils_chr(char)))
		{
			return false;
		}
	}
	return true;
});

var _String_contains = F2(function(sub, str)
{
	return str.indexOf(sub) > -1;
});

var _String_startsWith = F2(function(sub, str)
{
	return str.indexOf(sub) === 0;
});

var _String_endsWith = F2(function(sub, str)
{
	return str.length >= sub.length &&
		str.lastIndexOf(sub) === str.length - sub.length;
});

var _String_indexes = F2(function(sub, str)
{
	var subLen = sub.length;

	if (subLen < 1)
	{
		return _List_Nil;
	}

	var i = 0;
	var is = [];

	while ((i = str.indexOf(sub, i)) > -1)
	{
		is.push(i);
		i = i + subLen;
	}

	return _List_fromArray(is);
});


// TO STRING

function _String_fromNumber(number)
{
	return number + '';
}


// INT CONVERSIONS

function _String_toInt(str)
{
	var total = 0;
	var code0 = str.charCodeAt(0);
	var start = code0 == 0x2B /* + */ || code0 == 0x2D /* - */ ? 1 : 0;

	for (var i = start; i < str.length; ++i)
	{
		var code = str.charCodeAt(i);
		if (code < 0x30 || 0x39 < code)
		{
			return $elm$core$Maybe$Nothing;
		}
		total = 10 * total + code - 0x30;
	}

	return i == start
		? $elm$core$Maybe$Nothing
		: $elm$core$Maybe$Just(code0 == 0x2D ? -total : total);
}


// FLOAT CONVERSIONS

function _String_toFloat(s)
{
	// check if it is a hex, octal, or binary number
	if (s.length === 0 || /[\sxbo]/.test(s))
	{
		return $elm$core$Maybe$Nothing;
	}
	var n = +s;
	// faster isNaN check
	return n === n ? $elm$core$Maybe$Just(n) : $elm$core$Maybe$Nothing;
}

function _String_fromList(chars)
{
	return _List_toArray(chars).join('');
}




function _Char_toCode(char)
{
	var code = char.charCodeAt(0);
	if (0xD800 <= code && code <= 0xDBFF)
	{
		return (code - 0xD800) * 0x400 + char.charCodeAt(1) - 0xDC00 + 0x10000
	}
	return code;
}

function _Char_fromCode(code)
{
	return _Utils_chr(
		(code < 0 || 0x10FFFF < code)
			? '\uFFFD'
			:
		(code <= 0xFFFF)
			? String.fromCharCode(code)
			:
		(code -= 0x10000,
			String.fromCharCode(Math.floor(code / 0x400) + 0xD800, code % 0x400 + 0xDC00)
		)
	);
}

function _Char_toUpper(char)
{
	return _Utils_chr(char.toUpperCase());
}

function _Char_toLower(char)
{
	return _Utils_chr(char.toLowerCase());
}

function _Char_toLocaleUpper(char)
{
	return _Utils_chr(char.toLocaleUpperCase());
}

function _Char_toLocaleLower(char)
{
	return _Utils_chr(char.toLocaleLowerCase());
}



/**/
function _Json_errorToString(error)
{
	return $elm$json$Json$Decode$errorToString(error);
}
//*/


// CORE DECODERS

function _Json_succeed(msg)
{
	return {
		$: 0,
		a: msg
	};
}

function _Json_fail(msg)
{
	return {
		$: 1,
		a: msg
	};
}

function _Json_decodePrim(decoder)
{
	return { $: 2, b: decoder };
}

var _Json_decodeInt = _Json_decodePrim(function(value) {
	return (typeof value !== 'number')
		? _Json_expecting('an INT', value)
		:
	(-2147483647 < value && value < 2147483647 && (value | 0) === value)
		? $elm$core$Result$Ok(value)
		:
	(isFinite(value) && !(value % 1))
		? $elm$core$Result$Ok(value)
		: _Json_expecting('an INT', value);
});

var _Json_decodeBool = _Json_decodePrim(function(value) {
	return (typeof value === 'boolean')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a BOOL', value);
});

var _Json_decodeFloat = _Json_decodePrim(function(value) {
	return (typeof value === 'number')
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FLOAT', value);
});

var _Json_decodeValue = _Json_decodePrim(function(value) {
	return $elm$core$Result$Ok(_Json_wrap(value));
});

var _Json_decodeString = _Json_decodePrim(function(value) {
	return (typeof value === 'string')
		? $elm$core$Result$Ok(value)
		: (value instanceof String)
			? $elm$core$Result$Ok(value + '')
			: _Json_expecting('a STRING', value);
});

function _Json_decodeList(decoder) { return { $: 3, b: decoder }; }
function _Json_decodeArray(decoder) { return { $: 4, b: decoder }; }

function _Json_decodeNull(value) { return { $: 5, c: value }; }

var _Json_decodeField = F2(function(field, decoder)
{
	return {
		$: 6,
		d: field,
		b: decoder
	};
});

var _Json_decodeIndex = F2(function(index, decoder)
{
	return {
		$: 7,
		e: index,
		b: decoder
	};
});

function _Json_decodeKeyValuePairs(decoder)
{
	return {
		$: 8,
		b: decoder
	};
}

function _Json_mapMany(f, decoders)
{
	return {
		$: 9,
		f: f,
		g: decoders
	};
}

var _Json_andThen = F2(function(callback, decoder)
{
	return {
		$: 10,
		b: decoder,
		h: callback
	};
});

function _Json_oneOf(decoders)
{
	return {
		$: 11,
		g: decoders
	};
}


// DECODING OBJECTS

var _Json_map1 = F2(function(f, d1)
{
	return _Json_mapMany(f, [d1]);
});

var _Json_map2 = F3(function(f, d1, d2)
{
	return _Json_mapMany(f, [d1, d2]);
});

var _Json_map3 = F4(function(f, d1, d2, d3)
{
	return _Json_mapMany(f, [d1, d2, d3]);
});

var _Json_map4 = F5(function(f, d1, d2, d3, d4)
{
	return _Json_mapMany(f, [d1, d2, d3, d4]);
});

var _Json_map5 = F6(function(f, d1, d2, d3, d4, d5)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5]);
});

var _Json_map6 = F7(function(f, d1, d2, d3, d4, d5, d6)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6]);
});

var _Json_map7 = F8(function(f, d1, d2, d3, d4, d5, d6, d7)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7]);
});

var _Json_map8 = F9(function(f, d1, d2, d3, d4, d5, d6, d7, d8)
{
	return _Json_mapMany(f, [d1, d2, d3, d4, d5, d6, d7, d8]);
});


// DECODE

var _Json_runOnString = F2(function(decoder, string)
{
	try
	{
		var value = JSON.parse(string);
		return _Json_runHelp(decoder, value);
	}
	catch (e)
	{
		return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'This is not valid JSON! ' + e.message, _Json_wrap(string)));
	}
});

var _Json_run = F2(function(decoder, value)
{
	return _Json_runHelp(decoder, _Json_unwrap(value));
});

function _Json_runHelp(decoder, value)
{
	switch (decoder.$)
	{
		case 2:
			return decoder.b(value);

		case 5:
			return (value === null)
				? $elm$core$Result$Ok(decoder.c)
				: _Json_expecting('null', value);

		case 3:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('a LIST', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _List_fromArray);

		case 4:
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			return _Json_runArrayDecoder(decoder.b, value, _Json_toElmArray);

		case 6:
			var field = decoder.d;
			if (typeof value !== 'object' || value === null || !(field in value))
			{
				return _Json_expecting('an OBJECT with a field named `' + field + '`', value);
			}
			var result = _Json_runHelp(decoder.b, value[field]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, field, result.a));

		case 7:
			var index = decoder.e;
			if (!_Json_isArray(value))
			{
				return _Json_expecting('an ARRAY', value);
			}
			if (index >= value.length)
			{
				return _Json_expecting('a LONGER array. Need index ' + index + ' but only see ' + value.length + ' entries', value);
			}
			var result = _Json_runHelp(decoder.b, value[index]);
			return ($elm$core$Result$isOk(result)) ? result : $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, index, result.a));

		case 8:
			if (typeof value !== 'object' || value === null || _Json_isArray(value))
			{
				return _Json_expecting('an OBJECT', value);
			}

			var keyValuePairs = _List_Nil;
			// TODO test perf of Object.keys and switch when support is good enough
			for (var key in value)
			{
				if (value.hasOwnProperty(key))
				{
					var result = _Json_runHelp(decoder.b, value[key]);
					if (!$elm$core$Result$isOk(result))
					{
						return $elm$core$Result$Err(A2($elm$json$Json$Decode$Field, key, result.a));
					}
					keyValuePairs = _List_Cons(_Utils_Tuple2(key, result.a), keyValuePairs);
				}
			}
			return $elm$core$Result$Ok($elm$core$List$reverse(keyValuePairs));

		case 9:
			var answer = decoder.f;
			var decoders = decoder.g;
			for (var i = 0; i < decoders.length; i++)
			{
				var result = _Json_runHelp(decoders[i], value);
				if (!$elm$core$Result$isOk(result))
				{
					return result;
				}
				answer = answer(result.a);
			}
			return $elm$core$Result$Ok(answer);

		case 10:
			var result = _Json_runHelp(decoder.b, value);
			return (!$elm$core$Result$isOk(result))
				? result
				: _Json_runHelp(decoder.h(result.a), value);

		case 11:
			var errors = _List_Nil;
			for (var temp = decoder.g; temp.b; temp = temp.b) // WHILE_CONS
			{
				var result = _Json_runHelp(temp.a, value);
				if ($elm$core$Result$isOk(result))
				{
					return result;
				}
				errors = _List_Cons(result.a, errors);
			}
			return $elm$core$Result$Err($elm$json$Json$Decode$OneOf($elm$core$List$reverse(errors)));

		case 1:
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, decoder.a, _Json_wrap(value)));

		case 0:
			return $elm$core$Result$Ok(decoder.a);
	}
}

function _Json_runArrayDecoder(decoder, value, toElmValue)
{
	var len = value.length;
	var array = new Array(len);
	for (var i = 0; i < len; i++)
	{
		var result = _Json_runHelp(decoder, value[i]);
		if (!$elm$core$Result$isOk(result))
		{
			return $elm$core$Result$Err(A2($elm$json$Json$Decode$Index, i, result.a));
		}
		array[i] = result.a;
	}
	return $elm$core$Result$Ok(toElmValue(array));
}

function _Json_isArray(value)
{
	return Array.isArray(value) || (typeof FileList !== 'undefined' && value instanceof FileList);
}

function _Json_toElmArray(array)
{
	return A2($elm$core$Array$initialize, array.length, function(i) { return array[i]; });
}

function _Json_expecting(type, value)
{
	return $elm$core$Result$Err(A2($elm$json$Json$Decode$Failure, 'Expecting ' + type, _Json_wrap(value)));
}


// EQUALITY

function _Json_equality(x, y)
{
	if (x === y)
	{
		return true;
	}

	if (x.$ !== y.$)
	{
		return false;
	}

	switch (x.$)
	{
		case 0:
		case 1:
			return x.a === y.a;

		case 2:
			return x.b === y.b;

		case 5:
			return x.c === y.c;

		case 3:
		case 4:
		case 8:
			return _Json_equality(x.b, y.b);

		case 6:
			return x.d === y.d && _Json_equality(x.b, y.b);

		case 7:
			return x.e === y.e && _Json_equality(x.b, y.b);

		case 9:
			return x.f === y.f && _Json_listEquality(x.g, y.g);

		case 10:
			return x.h === y.h && _Json_equality(x.b, y.b);

		case 11:
			return _Json_listEquality(x.g, y.g);
	}
}

function _Json_listEquality(aDecoders, bDecoders)
{
	var len = aDecoders.length;
	if (len !== bDecoders.length)
	{
		return false;
	}
	for (var i = 0; i < len; i++)
	{
		if (!_Json_equality(aDecoders[i], bDecoders[i]))
		{
			return false;
		}
	}
	return true;
}


// ENCODE

var _Json_encode = F2(function(indentLevel, value)
{
	return JSON.stringify(_Json_unwrap(value), null, indentLevel) + '';
});

function _Json_wrap(value) { return { $: 0, a: value }; }
function _Json_unwrap(value) { return value.a; }

function _Json_wrap_UNUSED(value) { return value; }
function _Json_unwrap_UNUSED(value) { return value; }

function _Json_emptyArray() { return []; }
function _Json_emptyObject() { return {}; }

var _Json_addField = F3(function(key, value, object)
{
	object[key] = _Json_unwrap(value);
	return object;
});

function _Json_addEntry(func)
{
	return F2(function(entry, array)
	{
		array.push(_Json_unwrap(func(entry)));
		return array;
	});
}

var _Json_encodeNull = _Json_wrap(null);



// TASKS

function _Scheduler_succeed(value)
{
	return {
		$: 0,
		a: value
	};
}

function _Scheduler_fail(error)
{
	return {
		$: 1,
		a: error
	};
}

function _Scheduler_binding(callback)
{
	return {
		$: 2,
		b: callback,
		c: null
	};
}

var _Scheduler_andThen = F2(function(callback, task)
{
	return {
		$: 3,
		b: callback,
		d: task
	};
});

var _Scheduler_onError = F2(function(callback, task)
{
	return {
		$: 4,
		b: callback,
		d: task
	};
});

function _Scheduler_receive(callback)
{
	return {
		$: 5,
		b: callback
	};
}


// PROCESSES

var _Scheduler_guid = 0;

function _Scheduler_rawSpawn(task)
{
	var proc = {
		$: 0,
		e: _Scheduler_guid++,
		f: task,
		g: null,
		h: []
	};

	_Scheduler_enqueue(proc);

	return proc;
}

function _Scheduler_spawn(task)
{
	return _Scheduler_binding(function(callback) {
		callback(_Scheduler_succeed(_Scheduler_rawSpawn(task)));
	});
}

function _Scheduler_rawSend(proc, msg)
{
	proc.h.push(msg);
	_Scheduler_enqueue(proc);
}

var _Scheduler_send = F2(function(proc, msg)
{
	return _Scheduler_binding(function(callback) {
		_Scheduler_rawSend(proc, msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});

function _Scheduler_kill(proc)
{
	return _Scheduler_binding(function(callback) {
		var task = proc.f;
		if (task.$ === 2 && task.c)
		{
			task.c();
		}

		proc.f = null;

		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
}


/* STEP PROCESSES

type alias Process =
  { $ : tag
  , id : unique_id
  , root : Task
  , stack : null | { $: SUCCEED | FAIL, a: callback, b: stack }
  , mailbox : [msg]
  }

*/


var _Scheduler_working = false;
var _Scheduler_queue = [];


function _Scheduler_enqueue(proc)
{
	_Scheduler_queue.push(proc);
	if (_Scheduler_working)
	{
		return;
	}
	_Scheduler_working = true;
	while (proc = _Scheduler_queue.shift())
	{
		_Scheduler_step(proc);
	}
	_Scheduler_working = false;
}


function _Scheduler_step(proc)
{
	while (proc.f)
	{
		var rootTag = proc.f.$;
		if (rootTag === 0 || rootTag === 1)
		{
			while (proc.g && proc.g.$ !== rootTag)
			{
				proc.g = proc.g.i;
			}
			if (!proc.g)
			{
				return;
			}
			proc.f = proc.g.b(proc.f.a);
			proc.g = proc.g.i;
		}
		else if (rootTag === 2)
		{
			proc.f.c = proc.f.b(function(newRoot) {
				proc.f = newRoot;
				_Scheduler_enqueue(proc);
			});
			return;
		}
		else if (rootTag === 5)
		{
			if (proc.h.length === 0)
			{
				return;
			}
			proc.f = proc.f.b(proc.h.shift());
		}
		else // if (rootTag === 3 || rootTag === 4)
		{
			proc.g = {
				$: rootTag === 3 ? 0 : 1,
				b: proc.f.b,
				i: proc.g
			};
			proc.f = proc.f.d;
		}
	}
}



function _Process_sleep(time)
{
	return _Scheduler_binding(function(callback) {
		var id = setTimeout(function() {
			callback(_Scheduler_succeed(_Utils_Tuple0));
		}, time);

		return function() { clearTimeout(id); };
	});
}




// PROGRAMS


var _Platform_worker = F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function() { return function() {} }
	);
});



// INITIALIZE A PROGRAM


function _Platform_initialize(flagDecoder, args, init, update, subscriptions, stepperBuilder)
{
	var result = A2(_Json_run, flagDecoder, _Json_wrap(args ? args['flags'] : undefined));
	$elm$core$Result$isOk(result) || _Debug_crash(2 /**/, _Json_errorToString(result.a) /**/);
	var managers = {};
	result = init(result.a);
	var model = result.a;
	var stepper = stepperBuilder(sendToApp, model);
	var ports = _Platform_setupEffects(managers, sendToApp);

	function sendToApp(msg, viewMetadata)
	{
		result = A2(update, msg, model);
		stepper(model = result.a, viewMetadata);
		_Platform_dispatchEffects(managers, result.b, subscriptions(model));
	}

	_Platform_dispatchEffects(managers, result.b, subscriptions(model));

	return ports ? { ports: ports } : {};
}



// TRACK PRELOADS
//
// This is used by code in elm/browser and elm/http
// to register any HTTP requests that are triggered by init.
//


var _Platform_preload;


function _Platform_registerPreload(url)
{
	_Platform_preload.add(url);
}



// EFFECT MANAGERS


var _Platform_effectManagers = {};


function _Platform_setupEffects(managers, sendToApp)
{
	var ports;

	// setup all necessary effect managers
	for (var key in _Platform_effectManagers)
	{
		var manager = _Platform_effectManagers[key];

		if (manager.a)
		{
			ports = ports || {};
			ports[key] = manager.a(key, sendToApp);
		}

		managers[key] = _Platform_instantiateManager(manager, sendToApp);
	}

	return ports;
}


function _Platform_createManager(init, onEffects, onSelfMsg, cmdMap, subMap)
{
	return {
		b: init,
		c: onEffects,
		d: onSelfMsg,
		e: cmdMap,
		f: subMap
	};
}


function _Platform_instantiateManager(info, sendToApp)
{
	var router = {
		g: sendToApp,
		h: undefined
	};

	var onEffects = info.c;
	var onSelfMsg = info.d;
	var cmdMap = info.e;
	var subMap = info.f;

	function loop(state)
	{
		return A2(_Scheduler_andThen, loop, _Scheduler_receive(function(msg)
		{
			var value = msg.a;

			if (msg.$ === 0)
			{
				return A3(onSelfMsg, router, value, state);
			}

			return cmdMap && subMap
				? A4(onEffects, router, value.i, value.j, state)
				: A3(onEffects, router, cmdMap ? value.i : value.j, state);
		}));
	}

	return router.h = _Scheduler_rawSpawn(A2(_Scheduler_andThen, loop, info.b));
}



// ROUTING


var _Platform_sendToApp = F2(function(router, msg)
{
	return _Scheduler_binding(function(callback)
	{
		router.g(msg);
		callback(_Scheduler_succeed(_Utils_Tuple0));
	});
});


var _Platform_sendToSelf = F2(function(router, msg)
{
	return A2(_Scheduler_send, router.h, {
		$: 0,
		a: msg
	});
});



// BAGS


function _Platform_leaf(home)
{
	return function(value)
	{
		return {
			$: 1,
			k: home,
			l: value
		};
	};
}


function _Platform_batch(list)
{
	return {
		$: 2,
		m: list
	};
}


var _Platform_map = F2(function(tagger, bag)
{
	return {
		$: 3,
		n: tagger,
		o: bag
	}
});



// PIPE BAGS INTO EFFECT MANAGERS


function _Platform_dispatchEffects(managers, cmdBag, subBag)
{
	var effectsDict = {};
	_Platform_gatherEffects(true, cmdBag, effectsDict, null);
	_Platform_gatherEffects(false, subBag, effectsDict, null);

	for (var home in managers)
	{
		_Scheduler_rawSend(managers[home], {
			$: 'fx',
			a: effectsDict[home] || { i: _List_Nil, j: _List_Nil }
		});
	}
}


function _Platform_gatherEffects(isCmd, bag, effectsDict, taggers)
{
	switch (bag.$)
	{
		case 1:
			var home = bag.k;
			var effect = _Platform_toEffect(isCmd, home, taggers, bag.l);
			effectsDict[home] = _Platform_insert(isCmd, effect, effectsDict[home]);
			return;

		case 2:
			for (var list = bag.m; list.b; list = list.b) // WHILE_CONS
			{
				_Platform_gatherEffects(isCmd, list.a, effectsDict, taggers);
			}
			return;

		case 3:
			_Platform_gatherEffects(isCmd, bag.o, effectsDict, {
				p: bag.n,
				q: taggers
			});
			return;
	}
}


function _Platform_toEffect(isCmd, home, taggers, value)
{
	function applyTaggers(x)
	{
		for (var temp = taggers; temp; temp = temp.q)
		{
			x = temp.p(x);
		}
		return x;
	}

	var map = isCmd
		? _Platform_effectManagers[home].e
		: _Platform_effectManagers[home].f;

	return A2(map, applyTaggers, value)
}


function _Platform_insert(isCmd, newEffect, effects)
{
	effects = effects || { i: _List_Nil, j: _List_Nil };

	isCmd
		? (effects.i = _List_Cons(newEffect, effects.i))
		: (effects.j = _List_Cons(newEffect, effects.j));

	return effects;
}



// PORTS


function _Platform_checkPortName(name)
{
	if (_Platform_effectManagers[name])
	{
		_Debug_crash(3, name)
	}
}



// OUTGOING PORTS


function _Platform_outgoingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		e: _Platform_outgoingPortMap,
		r: converter,
		a: _Platform_setupOutgoingPort
	};
	return _Platform_leaf(name);
}


var _Platform_outgoingPortMap = F2(function(tagger, value) { return value; });


function _Platform_setupOutgoingPort(name)
{
	var subs = [];
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Process_sleep(0);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, cmdList, state)
	{
		for ( ; cmdList.b; cmdList = cmdList.b) // WHILE_CONS
		{
			// grab a separate reference to subs in case unsubscribe is called
			var currentSubs = subs;
			var value = _Json_unwrap(converter(cmdList.a));
			for (var i = 0; i < currentSubs.length; i++)
			{
				currentSubs[i](value);
			}
		}
		return init;
	});

	// PUBLIC API

	function subscribe(callback)
	{
		subs.push(callback);
	}

	function unsubscribe(callback)
	{
		// copy subs into a new array in case unsubscribe is called within a
		// subscribed callback
		subs = subs.slice();
		var index = subs.indexOf(callback);
		if (index >= 0)
		{
			subs.splice(index, 1);
		}
	}

	return {
		subscribe: subscribe,
		unsubscribe: unsubscribe
	};
}



// INCOMING PORTS


function _Platform_incomingPort(name, converter)
{
	_Platform_checkPortName(name);
	_Platform_effectManagers[name] = {
		f: _Platform_incomingPortMap,
		r: converter,
		a: _Platform_setupIncomingPort
	};
	return _Platform_leaf(name);
}


var _Platform_incomingPortMap = F2(function(tagger, finalTagger)
{
	return function(value)
	{
		return tagger(finalTagger(value));
	};
});


function _Platform_setupIncomingPort(name, sendToApp)
{
	var subs = _List_Nil;
	var converter = _Platform_effectManagers[name].r;

	// CREATE MANAGER

	var init = _Scheduler_succeed(null);

	_Platform_effectManagers[name].b = init;
	_Platform_effectManagers[name].c = F3(function(router, subList, state)
	{
		subs = subList;
		return init;
	});

	// PUBLIC API

	function send(incomingValue)
	{
		var result = A2(_Json_run, converter, _Json_wrap(incomingValue));

		$elm$core$Result$isOk(result) || _Debug_crash(4, name, result.a);

		var value = result.a;
		for (var temp = subs; temp.b; temp = temp.b) // WHILE_CONS
		{
			sendToApp(temp.a(value));
		}
	}

	return { send: send };
}



// EXPORT ELM MODULES
//
// Have DEBUG and PROD versions so that we can (1) give nicer errors in
// debug mode and (2) not pay for the bits needed for that in prod mode.
//


function _Platform_export_UNUSED(exports)
{
	scope['Elm']
		? _Platform_mergeExportsProd(scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsProd(obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6)
				: _Platform_mergeExportsProd(obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}


function _Platform_export(exports)
{
	scope['Elm']
		? _Platform_mergeExportsDebug('Elm', scope['Elm'], exports)
		: scope['Elm'] = exports;
}


function _Platform_mergeExportsDebug(moduleName, obj, exports)
{
	for (var name in exports)
	{
		(name in obj)
			? (name == 'init')
				? _Debug_crash(6, moduleName)
				: _Platform_mergeExportsDebug(moduleName + '.' + name, obj[name], exports[name])
			: (obj[name] = exports[name]);
	}
}




// HELPERS


var _VirtualDom_divertHrefToApp;

var _VirtualDom_doc = typeof document !== 'undefined' ? document : {};


function _VirtualDom_appendChild(parent, child)
{
	parent.appendChild(child);
}

var _VirtualDom_init = F4(function(virtualNode, flagDecoder, debugMetadata, args)
{
	// NOTE: this function needs _Platform_export available to work

	/**_UNUSED/
	var node = args['node'];
	//*/
	/**/
	var node = args && args['node'] ? args['node'] : _Debug_crash(0);
	//*/

	node.parentNode.replaceChild(
		_VirtualDom_render(virtualNode, function() {}),
		node
	);

	return {};
});



// TEXT


function _VirtualDom_text(string)
{
	return {
		$: 0,
		a: string
	};
}



// NODE


var _VirtualDom_nodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 1,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_node = _VirtualDom_nodeNS(undefined);



// KEYED NODE


var _VirtualDom_keyedNodeNS = F2(function(namespace, tag)
{
	return F2(function(factList, kidList)
	{
		for (var kids = [], descendantsCount = 0; kidList.b; kidList = kidList.b) // WHILE_CONS
		{
			var kid = kidList.a;
			descendantsCount += (kid.b.b || 0);
			kids.push(kid);
		}
		descendantsCount += kids.length;

		return {
			$: 2,
			c: tag,
			d: _VirtualDom_organizeFacts(factList),
			e: kids,
			f: namespace,
			b: descendantsCount
		};
	});
});


var _VirtualDom_keyedNode = _VirtualDom_keyedNodeNS(undefined);



// CUSTOM


function _VirtualDom_custom(factList, model, render, diff)
{
	return {
		$: 3,
		d: _VirtualDom_organizeFacts(factList),
		g: model,
		h: render,
		i: diff
	};
}



// MAP


var _VirtualDom_map = F2(function(tagger, node)
{
	return {
		$: 4,
		j: tagger,
		k: node,
		b: 1 + (node.b || 0)
	};
});



// LAZY


function _VirtualDom_thunk(refs, thunk)
{
	return {
		$: 5,
		l: refs,
		m: thunk,
		k: undefined
	};
}

var _VirtualDom_lazy = F2(function(func, a)
{
	return _VirtualDom_thunk([func, a], function() {
		return func(a);
	});
});

var _VirtualDom_lazy2 = F3(function(func, a, b)
{
	return _VirtualDom_thunk([func, a, b], function() {
		return A2(func, a, b);
	});
});

var _VirtualDom_lazy3 = F4(function(func, a, b, c)
{
	return _VirtualDom_thunk([func, a, b, c], function() {
		return A3(func, a, b, c);
	});
});

var _VirtualDom_lazy4 = F5(function(func, a, b, c, d)
{
	return _VirtualDom_thunk([func, a, b, c, d], function() {
		return A4(func, a, b, c, d);
	});
});

var _VirtualDom_lazy5 = F6(function(func, a, b, c, d, e)
{
	return _VirtualDom_thunk([func, a, b, c, d, e], function() {
		return A5(func, a, b, c, d, e);
	});
});

var _VirtualDom_lazy6 = F7(function(func, a, b, c, d, e, f)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f], function() {
		return A6(func, a, b, c, d, e, f);
	});
});

var _VirtualDom_lazy7 = F8(function(func, a, b, c, d, e, f, g)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g], function() {
		return A7(func, a, b, c, d, e, f, g);
	});
});

var _VirtualDom_lazy8 = F9(function(func, a, b, c, d, e, f, g, h)
{
	return _VirtualDom_thunk([func, a, b, c, d, e, f, g, h], function() {
		return A8(func, a, b, c, d, e, f, g, h);
	});
});



// FACTS


var _VirtualDom_on = F2(function(key, handler)
{
	return {
		$: 'a0',
		n: key,
		o: handler
	};
});
var _VirtualDom_style = F2(function(key, value)
{
	return {
		$: 'a1',
		n: key,
		o: value
	};
});
var _VirtualDom_property = F2(function(key, value)
{
	return {
		$: 'a2',
		n: key,
		o: value
	};
});
var _VirtualDom_attribute = F2(function(key, value)
{
	return {
		$: 'a3',
		n: key,
		o: value
	};
});
var _VirtualDom_attributeNS = F3(function(namespace, key, value)
{
	return {
		$: 'a4',
		n: key,
		o: { f: namespace, o: value }
	};
});



// XSS ATTACK VECTOR CHECKS


function _VirtualDom_noScript(tag)
{
	return tag == 'script' ? 'p' : tag;
}

function _VirtualDom_noOnOrFormAction(key)
{
	return /^(on|formAction$)/i.test(key) ? 'data-' + key : key;
}

function _VirtualDom_noInnerHtmlOrFormAction(key)
{
	return key == 'innerHTML' || key == 'formAction' ? 'data-' + key : key;
}

function _VirtualDom_noJavaScriptUri_UNUSED(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,'')) ? '' : value;
}

function _VirtualDom_noJavaScriptUri(value)
{
	return /^javascript:/i.test(value.replace(/\s/g,''))
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}

function _VirtualDom_noJavaScriptOrHtmlUri_UNUSED(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value) ? '' : value;
}

function _VirtualDom_noJavaScriptOrHtmlUri(value)
{
	return /^\s*(javascript:|data:text\/html)/i.test(value)
		? 'javascript:alert("This is an XSS vector. Please use ports or web components instead.")'
		: value;
}



// MAP FACTS


var _VirtualDom_mapAttribute = F2(function(func, attr)
{
	return (attr.$ === 'a0')
		? A2(_VirtualDom_on, attr.n, _VirtualDom_mapHandler(func, attr.o))
		: attr;
});

function _VirtualDom_mapHandler(func, handler)
{
	var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

	// 0 = Normal
	// 1 = MayStopPropagation
	// 2 = MayPreventDefault
	// 3 = Custom

	return {
		$: handler.$,
		a:
			!tag
				? A2($elm$json$Json$Decode$map, func, handler.a)
				:
			A3($elm$json$Json$Decode$map2,
				tag < 3
					? _VirtualDom_mapEventTuple
					: _VirtualDom_mapEventRecord,
				$elm$json$Json$Decode$succeed(func),
				handler.a
			)
	};
}

var _VirtualDom_mapEventTuple = F2(function(func, tuple)
{
	return _Utils_Tuple2(func(tuple.a), tuple.b);
});

var _VirtualDom_mapEventRecord = F2(function(func, record)
{
	return {
		message: func(record.message),
		stopPropagation: record.stopPropagation,
		preventDefault: record.preventDefault
	}
});



// ORGANIZE FACTS


function _VirtualDom_organizeFacts(factList)
{
	for (var facts = {}; factList.b; factList = factList.b) // WHILE_CONS
	{
		var entry = factList.a;

		var tag = entry.$;
		var key = entry.n;
		var value = entry.o;

		if (tag === 'a2')
		{
			(key === 'className')
				? _VirtualDom_addClass(facts, key, _Json_unwrap(value))
				: facts[key] = _Json_unwrap(value);

			continue;
		}

		var subFacts = facts[tag] || (facts[tag] = {});
		(tag === 'a3' && key === 'class')
			? _VirtualDom_addClass(subFacts, key, value)
			: subFacts[key] = value;
	}

	return facts;
}

function _VirtualDom_addClass(object, key, newClass)
{
	var classes = object[key];
	object[key] = classes ? classes + ' ' + newClass : newClass;
}



// RENDER


function _VirtualDom_render(vNode, eventNode)
{
	var tag = vNode.$;

	if (tag === 5)
	{
		return _VirtualDom_render(vNode.k || (vNode.k = vNode.m()), eventNode);
	}

	if (tag === 0)
	{
		return _VirtualDom_doc.createTextNode(vNode.a);
	}

	if (tag === 4)
	{
		var subNode = vNode.k;
		var tagger = vNode.j;

		while (subNode.$ === 4)
		{
			typeof tagger !== 'object'
				? tagger = [tagger, subNode.j]
				: tagger.push(subNode.j);

			subNode = subNode.k;
		}

		var subEventRoot = { j: tagger, p: eventNode };
		var domNode = _VirtualDom_render(subNode, subEventRoot);
		domNode.elm_event_node_ref = subEventRoot;
		return domNode;
	}

	if (tag === 3)
	{
		var domNode = vNode.h(vNode.g);
		_VirtualDom_applyFacts(domNode, eventNode, vNode.d);
		return domNode;
	}

	// at this point `tag` must be 1 or 2

	var domNode = vNode.f
		? _VirtualDom_doc.createElementNS(vNode.f, vNode.c)
		: _VirtualDom_doc.createElement(vNode.c);

	if (_VirtualDom_divertHrefToApp && vNode.c == 'a')
	{
		domNode.addEventListener('click', _VirtualDom_divertHrefToApp(domNode));
	}

	_VirtualDom_applyFacts(domNode, eventNode, vNode.d);

	for (var kids = vNode.e, i = 0; i < kids.length; i++)
	{
		_VirtualDom_appendChild(domNode, _VirtualDom_render(tag === 1 ? kids[i] : kids[i].b, eventNode));
	}

	return domNode;
}



// APPLY FACTS


function _VirtualDom_applyFacts(domNode, eventNode, facts)
{
	for (var key in facts)
	{
		var value = facts[key];

		key === 'a1'
			? _VirtualDom_applyStyles(domNode, value)
			:
		key === 'a0'
			? _VirtualDom_applyEvents(domNode, eventNode, value)
			:
		key === 'a3'
			? _VirtualDom_applyAttrs(domNode, value)
			:
		key === 'a4'
			? _VirtualDom_applyAttrsNS(domNode, value)
			:
		((key !== 'value' && key !== 'checked') || domNode[key] !== value) && (domNode[key] = value);
	}
}



// APPLY STYLES


function _VirtualDom_applyStyles(domNode, styles)
{
	var domNodeStyle = domNode.style;

	for (var key in styles)
	{
		domNodeStyle[key] = styles[key];
	}
}



// APPLY ATTRS


function _VirtualDom_applyAttrs(domNode, attrs)
{
	for (var key in attrs)
	{
		var value = attrs[key];
		typeof value !== 'undefined'
			? domNode.setAttribute(key, value)
			: domNode.removeAttribute(key);
	}
}



// APPLY NAMESPACED ATTRS


function _VirtualDom_applyAttrsNS(domNode, nsAttrs)
{
	for (var key in nsAttrs)
	{
		var pair = nsAttrs[key];
		var namespace = pair.f;
		var value = pair.o;

		typeof value !== 'undefined'
			? domNode.setAttributeNS(namespace, key, value)
			: domNode.removeAttributeNS(namespace, key);
	}
}



// APPLY EVENTS


function _VirtualDom_applyEvents(domNode, eventNode, events)
{
	var allCallbacks = domNode.elmFs || (domNode.elmFs = {});

	for (var key in events)
	{
		var newHandler = events[key];
		var oldCallback = allCallbacks[key];

		if (!newHandler)
		{
			domNode.removeEventListener(key, oldCallback);
			allCallbacks[key] = undefined;
			continue;
		}

		if (oldCallback)
		{
			var oldHandler = oldCallback.q;
			if (oldHandler.$ === newHandler.$)
			{
				oldCallback.q = newHandler;
				continue;
			}
			domNode.removeEventListener(key, oldCallback);
		}

		oldCallback = _VirtualDom_makeCallback(eventNode, newHandler);
		domNode.addEventListener(key, oldCallback,
			_VirtualDom_passiveSupported
			&& { passive: $elm$virtual_dom$VirtualDom$toHandlerInt(newHandler) < 2 }
		);
		allCallbacks[key] = oldCallback;
	}
}



// PASSIVE EVENTS


var _VirtualDom_passiveSupported;

try
{
	window.addEventListener('t', null, Object.defineProperty({}, 'passive', {
		get: function() { _VirtualDom_passiveSupported = true; }
	}));
}
catch(e) {}



// EVENT HANDLERS


function _VirtualDom_makeCallback(eventNode, initialHandler)
{
	function callback(event)
	{
		var handler = callback.q;
		var result = _Json_runHelp(handler.a, event);

		if (!$elm$core$Result$isOk(result))
		{
			return;
		}

		var tag = $elm$virtual_dom$VirtualDom$toHandlerInt(handler);

		// 0 = Normal
		// 1 = MayStopPropagation
		// 2 = MayPreventDefault
		// 3 = Custom

		var value = result.a;
		var message = !tag ? value : tag < 3 ? value.a : value.message;
		var stopPropagation = tag == 1 ? value.b : tag == 3 && value.stopPropagation;
		var currentEventNode = (
			stopPropagation && event.stopPropagation(),
			(tag == 2 ? value.b : tag == 3 && value.preventDefault) && event.preventDefault(),
			eventNode
		);
		var tagger;
		var i;
		while (tagger = currentEventNode.j)
		{
			if (typeof tagger == 'function')
			{
				message = tagger(message);
			}
			else
			{
				for (var i = tagger.length; i--; )
				{
					message = tagger[i](message);
				}
			}
			currentEventNode = currentEventNode.p;
		}
		currentEventNode(message, stopPropagation); // stopPropagation implies isSync
	}

	callback.q = initialHandler;

	return callback;
}

function _VirtualDom_equalEvents(x, y)
{
	return x.$ == y.$ && _Json_equality(x.a, y.a);
}



// DIFF


// TODO: Should we do patches like in iOS?
//
// type Patch
//   = At Int Patch
//   | Batch (List Patch)
//   | Change ...
//
// How could it not be better?
//
function _VirtualDom_diff(x, y)
{
	var patches = [];
	_VirtualDom_diffHelp(x, y, patches, 0);
	return patches;
}


function _VirtualDom_pushPatch(patches, type, index, data)
{
	var patch = {
		$: type,
		r: index,
		s: data,
		t: undefined,
		u: undefined
	};
	patches.push(patch);
	return patch;
}


function _VirtualDom_diffHelp(x, y, patches, index)
{
	if (x === y)
	{
		return;
	}

	var xType = x.$;
	var yType = y.$;

	// Bail if you run into different types of nodes. Implies that the
	// structure has changed significantly and it's not worth a diff.
	if (xType !== yType)
	{
		if (xType === 1 && yType === 2)
		{
			y = _VirtualDom_dekey(y);
			yType = 1;
		}
		else
		{
			_VirtualDom_pushPatch(patches, 0, index, y);
			return;
		}
	}

	// Now we know that both nodes are the same $.
	switch (yType)
	{
		case 5:
			var xRefs = x.l;
			var yRefs = y.l;
			var i = xRefs.length;
			var same = i === yRefs.length;
			while (same && i--)
			{
				same = xRefs[i] === yRefs[i];
			}
			if (same)
			{
				y.k = x.k;
				return;
			}
			y.k = y.m();
			var subPatches = [];
			_VirtualDom_diffHelp(x.k, y.k, subPatches, 0);
			subPatches.length > 0 && _VirtualDom_pushPatch(patches, 1, index, subPatches);
			return;

		case 4:
			// gather nested taggers
			var xTaggers = x.j;
			var yTaggers = y.j;
			var nesting = false;

			var xSubNode = x.k;
			while (xSubNode.$ === 4)
			{
				nesting = true;

				typeof xTaggers !== 'object'
					? xTaggers = [xTaggers, xSubNode.j]
					: xTaggers.push(xSubNode.j);

				xSubNode = xSubNode.k;
			}

			var ySubNode = y.k;
			while (ySubNode.$ === 4)
			{
				nesting = true;

				typeof yTaggers !== 'object'
					? yTaggers = [yTaggers, ySubNode.j]
					: yTaggers.push(ySubNode.j);

				ySubNode = ySubNode.k;
			}

			// Just bail if different numbers of taggers. This implies the
			// structure of the virtual DOM has changed.
			if (nesting && xTaggers.length !== yTaggers.length)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			// check if taggers are "the same"
			if (nesting ? !_VirtualDom_pairwiseRefEqual(xTaggers, yTaggers) : xTaggers !== yTaggers)
			{
				_VirtualDom_pushPatch(patches, 2, index, yTaggers);
			}

			// diff everything below the taggers
			_VirtualDom_diffHelp(xSubNode, ySubNode, patches, index + 1);
			return;

		case 0:
			if (x.a !== y.a)
			{
				_VirtualDom_pushPatch(patches, 3, index, y.a);
			}
			return;

		case 1:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKids);
			return;

		case 2:
			_VirtualDom_diffNodes(x, y, patches, index, _VirtualDom_diffKeyedKids);
			return;

		case 3:
			if (x.h !== y.h)
			{
				_VirtualDom_pushPatch(patches, 0, index, y);
				return;
			}

			var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
			factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

			var patch = y.i(x.g, y.g);
			patch && _VirtualDom_pushPatch(patches, 5, index, patch);

			return;
	}
}

// assumes the incoming arrays are the same length
function _VirtualDom_pairwiseRefEqual(as, bs)
{
	for (var i = 0; i < as.length; i++)
	{
		if (as[i] !== bs[i])
		{
			return false;
		}
	}

	return true;
}

function _VirtualDom_diffNodes(x, y, patches, index, diffKids)
{
	// Bail if obvious indicators have changed. Implies more serious
	// structural changes such that it's not worth it to diff.
	if (x.c !== y.c || x.f !== y.f)
	{
		_VirtualDom_pushPatch(patches, 0, index, y);
		return;
	}

	var factsDiff = _VirtualDom_diffFacts(x.d, y.d);
	factsDiff && _VirtualDom_pushPatch(patches, 4, index, factsDiff);

	diffKids(x, y, patches, index);
}



// DIFF FACTS


// TODO Instead of creating a new diff object, it's possible to just test if
// there *is* a diff. During the actual patch, do the diff again and make the
// modifications directly. This way, there's no new allocations. Worth it?
function _VirtualDom_diffFacts(x, y, category)
{
	var diff;

	// look for changes and removals
	for (var xKey in x)
	{
		if (xKey === 'a1' || xKey === 'a0' || xKey === 'a3' || xKey === 'a4')
		{
			var subDiff = _VirtualDom_diffFacts(x[xKey], y[xKey] || {}, xKey);
			if (subDiff)
			{
				diff = diff || {};
				diff[xKey] = subDiff;
			}
			continue;
		}

		// remove if not in the new facts
		if (!(xKey in y))
		{
			diff = diff || {};
			diff[xKey] =
				!category
					? (typeof x[xKey] === 'string' ? '' : null)
					:
				(category === 'a1')
					? ''
					:
				(category === 'a0' || category === 'a3')
					? undefined
					:
				{ f: x[xKey].f, o: undefined };

			continue;
		}

		var xValue = x[xKey];
		var yValue = y[xKey];

		// reference equal, so don't worry about it
		if (xValue === yValue && xKey !== 'value' && xKey !== 'checked'
			|| category === 'a0' && _VirtualDom_equalEvents(xValue, yValue))
		{
			continue;
		}

		diff = diff || {};
		diff[xKey] = yValue;
	}

	// add new stuff
	for (var yKey in y)
	{
		if (!(yKey in x))
		{
			diff = diff || {};
			diff[yKey] = y[yKey];
		}
	}

	return diff;
}



// DIFF KIDS


function _VirtualDom_diffKids(xParent, yParent, patches, index)
{
	var xKids = xParent.e;
	var yKids = yParent.e;

	var xLen = xKids.length;
	var yLen = yKids.length;

	// FIGURE OUT IF THERE ARE INSERTS OR REMOVALS

	if (xLen > yLen)
	{
		_VirtualDom_pushPatch(patches, 6, index, {
			v: yLen,
			i: xLen - yLen
		});
	}
	else if (xLen < yLen)
	{
		_VirtualDom_pushPatch(patches, 7, index, {
			v: xLen,
			e: yKids
		});
	}

	// PAIRWISE DIFF EVERYTHING ELSE

	for (var minLen = xLen < yLen ? xLen : yLen, i = 0; i < minLen; i++)
	{
		var xKid = xKids[i];
		_VirtualDom_diffHelp(xKid, yKids[i], patches, ++index);
		index += xKid.b || 0;
	}
}



// KEYED DIFF


function _VirtualDom_diffKeyedKids(xParent, yParent, patches, rootIndex)
{
	var localPatches = [];

	var changes = {}; // Dict String Entry
	var inserts = []; // Array { index : Int, entry : Entry }
	// type Entry = { tag : String, vnode : VNode, index : Int, data : _ }

	var xKids = xParent.e;
	var yKids = yParent.e;
	var xLen = xKids.length;
	var yLen = yKids.length;
	var xIndex = 0;
	var yIndex = 0;

	var index = rootIndex;

	while (xIndex < xLen && yIndex < yLen)
	{
		var x = xKids[xIndex];
		var y = yKids[yIndex];

		var xKey = x.a;
		var yKey = y.a;
		var xNode = x.b;
		var yNode = y.b;

		var newMatch = undefined;
		var oldMatch = undefined;

		// check if keys match

		if (xKey === yKey)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNode, localPatches, index);
			index += xNode.b || 0;

			xIndex++;
			yIndex++;
			continue;
		}

		// look ahead 1 to detect insertions and removals.

		var xNext = xKids[xIndex + 1];
		var yNext = yKids[yIndex + 1];

		if (xNext)
		{
			var xNextKey = xNext.a;
			var xNextNode = xNext.b;
			oldMatch = yKey === xNextKey;
		}

		if (yNext)
		{
			var yNextKey = yNext.a;
			var yNextNode = yNext.b;
			newMatch = xKey === yNextKey;
		}


		// swap x and y
		if (newMatch && oldMatch)
		{
			index++;
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			_VirtualDom_insertNode(changes, localPatches, xKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNextNode, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		// insert y
		if (newMatch)
		{
			index++;
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			_VirtualDom_diffHelp(xNode, yNextNode, localPatches, index);
			index += xNode.b || 0;

			xIndex += 1;
			yIndex += 2;
			continue;
		}

		// remove x
		if (oldMatch)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 1;
			continue;
		}

		// remove x, insert y
		if (xNext && xNextKey === yNextKey)
		{
			index++;
			_VirtualDom_removeNode(changes, localPatches, xKey, xNode, index);
			_VirtualDom_insertNode(changes, localPatches, yKey, yNode, yIndex, inserts);
			index += xNode.b || 0;

			index++;
			_VirtualDom_diffHelp(xNextNode, yNextNode, localPatches, index);
			index += xNextNode.b || 0;

			xIndex += 2;
			yIndex += 2;
			continue;
		}

		break;
	}

	// eat up any remaining nodes with removeNode and insertNode

	while (xIndex < xLen)
	{
		index++;
		var x = xKids[xIndex];
		var xNode = x.b;
		_VirtualDom_removeNode(changes, localPatches, x.a, xNode, index);
		index += xNode.b || 0;
		xIndex++;
	}

	while (yIndex < yLen)
	{
		var endInserts = endInserts || [];
		var y = yKids[yIndex];
		_VirtualDom_insertNode(changes, localPatches, y.a, y.b, undefined, endInserts);
		yIndex++;
	}

	if (localPatches.length > 0 || inserts.length > 0 || endInserts)
	{
		_VirtualDom_pushPatch(patches, 8, rootIndex, {
			w: localPatches,
			x: inserts,
			y: endInserts
		});
	}
}



// CHANGES FROM KEYED DIFF


var _VirtualDom_POSTFIX = '_elmW6BL';


function _VirtualDom_insertNode(changes, localPatches, key, vnode, yIndex, inserts)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		entry = {
			c: 0,
			z: vnode,
			r: yIndex,
			s: undefined
		};

		inserts.push({ r: yIndex, A: entry });
		changes[key] = entry;

		return;
	}

	// this key was removed earlier, a match!
	if (entry.c === 1)
	{
		inserts.push({ r: yIndex, A: entry });

		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(entry.z, vnode, subPatches, entry.r);
		entry.r = yIndex;
		entry.s.s = {
			w: subPatches,
			A: entry
		};

		return;
	}

	// this key has already been inserted or moved, a duplicate!
	_VirtualDom_insertNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, yIndex, inserts);
}


function _VirtualDom_removeNode(changes, localPatches, key, vnode, index)
{
	var entry = changes[key];

	// never seen this key before
	if (!entry)
	{
		var patch = _VirtualDom_pushPatch(localPatches, 9, index, undefined);

		changes[key] = {
			c: 1,
			z: vnode,
			r: index,
			s: patch
		};

		return;
	}

	// this key was inserted earlier, a match!
	if (entry.c === 0)
	{
		entry.c = 2;
		var subPatches = [];
		_VirtualDom_diffHelp(vnode, entry.z, subPatches, index);

		_VirtualDom_pushPatch(localPatches, 9, index, {
			w: subPatches,
			A: entry
		});

		return;
	}

	// this key has already been removed or moved, a duplicate!
	_VirtualDom_removeNode(changes, localPatches, key + _VirtualDom_POSTFIX, vnode, index);
}



// ADD DOM NODES
//
// Each DOM node has an "index" assigned in order of traversal. It is important
// to minimize our crawl over the actual DOM, so these indexes (along with the
// descendantsCount of virtual nodes) let us skip touching entire subtrees of
// the DOM if we know there are no patches there.


function _VirtualDom_addDomNodes(domNode, vNode, patches, eventNode)
{
	_VirtualDom_addDomNodesHelp(domNode, vNode, patches, 0, 0, vNode.b, eventNode);
}


// assumes `patches` is non-empty and indexes increase monotonically.
function _VirtualDom_addDomNodesHelp(domNode, vNode, patches, i, low, high, eventNode)
{
	var patch = patches[i];
	var index = patch.r;

	while (index === low)
	{
		var patchType = patch.$;

		if (patchType === 1)
		{
			_VirtualDom_addDomNodes(domNode, vNode.k, patch.s, eventNode);
		}
		else if (patchType === 8)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var subPatches = patch.s.w;
			if (subPatches.length > 0)
			{
				_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
			}
		}
		else if (patchType === 9)
		{
			patch.t = domNode;
			patch.u = eventNode;

			var data = patch.s;
			if (data)
			{
				data.A.s = domNode;
				var subPatches = data.w;
				if (subPatches.length > 0)
				{
					_VirtualDom_addDomNodesHelp(domNode, vNode, subPatches, 0, low, high, eventNode);
				}
			}
		}
		else
		{
			patch.t = domNode;
			patch.u = eventNode;
		}

		i++;

		if (!(patch = patches[i]) || (index = patch.r) > high)
		{
			return i;
		}
	}

	var tag = vNode.$;

	if (tag === 4)
	{
		var subNode = vNode.k;

		while (subNode.$ === 4)
		{
			subNode = subNode.k;
		}

		return _VirtualDom_addDomNodesHelp(domNode, subNode, patches, i, low + 1, high, domNode.elm_event_node_ref);
	}

	// tag must be 1 or 2 at this point

	var vKids = vNode.e;
	var childNodes = domNode.childNodes;
	for (var j = 0; j < vKids.length; j++)
	{
		low++;
		var vKid = tag === 1 ? vKids[j] : vKids[j].b;
		var nextLow = low + (vKid.b || 0);
		if (low <= index && index <= nextLow)
		{
			i = _VirtualDom_addDomNodesHelp(childNodes[j], vKid, patches, i, low, nextLow, eventNode);
			if (!(patch = patches[i]) || (index = patch.r) > high)
			{
				return i;
			}
		}
		low = nextLow;
	}
	return i;
}



// APPLY PATCHES


function _VirtualDom_applyPatches(rootDomNode, oldVirtualNode, patches, eventNode)
{
	if (patches.length === 0)
	{
		return rootDomNode;
	}

	_VirtualDom_addDomNodes(rootDomNode, oldVirtualNode, patches, eventNode);
	return _VirtualDom_applyPatchesHelp(rootDomNode, patches);
}

function _VirtualDom_applyPatchesHelp(rootDomNode, patches)
{
	for (var i = 0; i < patches.length; i++)
	{
		var patch = patches[i];
		var localDomNode = patch.t
		var newNode = _VirtualDom_applyPatch(localDomNode, patch);
		if (localDomNode === rootDomNode)
		{
			rootDomNode = newNode;
		}
	}
	return rootDomNode;
}

function _VirtualDom_applyPatch(domNode, patch)
{
	switch (patch.$)
	{
		case 0:
			return _VirtualDom_applyPatchRedraw(domNode, patch.s, patch.u);

		case 4:
			_VirtualDom_applyFacts(domNode, patch.u, patch.s);
			return domNode;

		case 3:
			domNode.replaceData(0, domNode.length, patch.s);
			return domNode;

		case 1:
			return _VirtualDom_applyPatchesHelp(domNode, patch.s);

		case 2:
			if (domNode.elm_event_node_ref)
			{
				domNode.elm_event_node_ref.j = patch.s;
			}
			else
			{
				domNode.elm_event_node_ref = { j: patch.s, p: patch.u };
			}
			return domNode;

		case 6:
			var data = patch.s;
			for (var i = 0; i < data.i; i++)
			{
				domNode.removeChild(domNode.childNodes[data.v]);
			}
			return domNode;

		case 7:
			var data = patch.s;
			var kids = data.e;
			var i = data.v;
			var theEnd = domNode.childNodes[i];
			for (; i < kids.length; i++)
			{
				domNode.insertBefore(_VirtualDom_render(kids[i], patch.u), theEnd);
			}
			return domNode;

		case 9:
			var data = patch.s;
			if (!data)
			{
				domNode.parentNode.removeChild(domNode);
				return domNode;
			}
			var entry = data.A;
			if (typeof entry.r !== 'undefined')
			{
				domNode.parentNode.removeChild(domNode);
			}
			entry.s = _VirtualDom_applyPatchesHelp(domNode, data.w);
			return domNode;

		case 8:
			return _VirtualDom_applyPatchReorder(domNode, patch);

		case 5:
			return patch.s(domNode);

		default:
			_Debug_crash(10); // 'Ran into an unknown patch!'
	}
}


function _VirtualDom_applyPatchRedraw(domNode, vNode, eventNode)
{
	var parentNode = domNode.parentNode;
	var newNode = _VirtualDom_render(vNode, eventNode);

	if (!newNode.elm_event_node_ref)
	{
		newNode.elm_event_node_ref = domNode.elm_event_node_ref;
	}

	if (parentNode && newNode !== domNode)
	{
		parentNode.replaceChild(newNode, domNode);
	}
	return newNode;
}


function _VirtualDom_applyPatchReorder(domNode, patch)
{
	var data = patch.s;

	// remove end inserts
	var frag = _VirtualDom_applyPatchReorderEndInsertsHelp(data.y, patch);

	// removals
	domNode = _VirtualDom_applyPatchesHelp(domNode, data.w);

	// inserts
	var inserts = data.x;
	for (var i = 0; i < inserts.length; i++)
	{
		var insert = inserts[i];
		var entry = insert.A;
		var node = entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u);
		domNode.insertBefore(node, domNode.childNodes[insert.r]);
	}

	// add end inserts
	if (frag)
	{
		_VirtualDom_appendChild(domNode, frag);
	}

	return domNode;
}


function _VirtualDom_applyPatchReorderEndInsertsHelp(endInserts, patch)
{
	if (!endInserts)
	{
		return;
	}

	var frag = _VirtualDom_doc.createDocumentFragment();
	for (var i = 0; i < endInserts.length; i++)
	{
		var insert = endInserts[i];
		var entry = insert.A;
		_VirtualDom_appendChild(frag, entry.c === 2
			? entry.s
			: _VirtualDom_render(entry.z, patch.u)
		);
	}
	return frag;
}


function _VirtualDom_virtualize(node)
{
	// TEXT NODES

	if (node.nodeType === 3)
	{
		return _VirtualDom_text(node.textContent);
	}


	// WEIRD NODES

	if (node.nodeType !== 1)
	{
		return _VirtualDom_text('');
	}


	// ELEMENT NODES

	var attrList = _List_Nil;
	var attrs = node.attributes;
	for (var i = attrs.length; i--; )
	{
		var attr = attrs[i];
		var name = attr.name;
		var value = attr.value;
		attrList = _List_Cons( A2(_VirtualDom_attribute, name, value), attrList );
	}

	var tag = node.tagName.toLowerCase();
	var kidList = _List_Nil;
	var kids = node.childNodes;

	for (var i = kids.length; i--; )
	{
		kidList = _List_Cons(_VirtualDom_virtualize(kids[i]), kidList);
	}
	return A3(_VirtualDom_node, tag, attrList, kidList);
}

function _VirtualDom_dekey(keyedNode)
{
	var keyedKids = keyedNode.e;
	var len = keyedKids.length;
	var kids = new Array(len);
	for (var i = 0; i < len; i++)
	{
		kids[i] = keyedKids[i].b;
	}

	return {
		$: 1,
		c: keyedNode.c,
		d: keyedNode.d,
		e: kids,
		f: keyedNode.f,
		b: keyedNode.b
	};
}




// ELEMENT


var _Debugger_element;

var _Browser_element = _Debugger_element || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var view = impl.view;
			/**_UNUSED/
			var domNode = args['node'];
			//*/
			/**/
			var domNode = args && args['node'] ? args['node'] : _Debug_crash(0);
			//*/
			var currNode = _VirtualDom_virtualize(domNode);

			return _Browser_makeAnimator(initialModel, function(model)
			{
				var nextNode = view(model);
				var patches = _VirtualDom_diff(currNode, nextNode);
				domNode = _VirtualDom_applyPatches(domNode, currNode, patches, sendToApp);
				currNode = nextNode;
			});
		}
	);
});



// DOCUMENT


var _Debugger_document;

var _Browser_document = _Debugger_document || F4(function(impl, flagDecoder, debugMetadata, args)
{
	return _Platform_initialize(
		flagDecoder,
		args,
		impl.init,
		impl.update,
		impl.subscriptions,
		function(sendToApp, initialModel) {
			var divertHrefToApp = impl.setup && impl.setup(sendToApp)
			var view = impl.view;
			var title = _VirtualDom_doc.title;
			var bodyNode = _VirtualDom_doc.body;
			var currNode = _VirtualDom_virtualize(bodyNode);
			return _Browser_makeAnimator(initialModel, function(model)
			{
				_VirtualDom_divertHrefToApp = divertHrefToApp;
				var doc = view(model);
				var nextNode = _VirtualDom_node('body')(_List_Nil)(doc.body);
				var patches = _VirtualDom_diff(currNode, nextNode);
				bodyNode = _VirtualDom_applyPatches(bodyNode, currNode, patches, sendToApp);
				currNode = nextNode;
				_VirtualDom_divertHrefToApp = 0;
				(title !== doc.title) && (_VirtualDom_doc.title = title = doc.title);
			});
		}
	);
});



// ANIMATION


var _Browser_cancelAnimationFrame =
	typeof cancelAnimationFrame !== 'undefined'
		? cancelAnimationFrame
		: function(id) { clearTimeout(id); };

var _Browser_requestAnimationFrame =
	typeof requestAnimationFrame !== 'undefined'
		? requestAnimationFrame
		: function(callback) { return setTimeout(callback, 1000 / 60); };


function _Browser_makeAnimator(model, draw)
{
	draw(model);

	var state = 0;

	function updateIfNeeded()
	{
		state = state === 1
			? 0
			: ( _Browser_requestAnimationFrame(updateIfNeeded), draw(model), 1 );
	}

	return function(nextModel, isSync)
	{
		model = nextModel;

		isSync
			? ( draw(model),
				state === 2 && (state = 1)
				)
			: ( state === 0 && _Browser_requestAnimationFrame(updateIfNeeded),
				state = 2
				);
	};
}



// APPLICATION


function _Browser_application(impl)
{
	var onUrlChange = impl.onUrlChange;
	var onUrlRequest = impl.onUrlRequest;
	var key = function() { key.a(onUrlChange(_Browser_getUrl())); };

	return _Browser_document({
		setup: function(sendToApp)
		{
			key.a = sendToApp;
			_Browser_window.addEventListener('popstate', key);
			_Browser_window.navigator.userAgent.indexOf('Trident') < 0 || _Browser_window.addEventListener('hashchange', key);

			return F2(function(domNode, event)
			{
				if (!event.ctrlKey && !event.metaKey && !event.shiftKey && event.button < 1 && !domNode.target && !domNode.hasAttribute('download'))
				{
					event.preventDefault();
					var href = domNode.href;
					var curr = _Browser_getUrl();
					var next = $elm$url$Url$fromString(href).a;
					sendToApp(onUrlRequest(
						(next
							&& curr.protocol === next.protocol
							&& curr.host === next.host
							&& curr.port_.a === next.port_.a
						)
							? $elm$browser$Browser$Internal(next)
							: $elm$browser$Browser$External(href)
					));
				}
			});
		},
		init: function(flags)
		{
			return A3(impl.init, flags, _Browser_getUrl(), key);
		},
		view: impl.view,
		update: impl.update,
		subscriptions: impl.subscriptions
	});
}

function _Browser_getUrl()
{
	return $elm$url$Url$fromString(_VirtualDom_doc.location.href).a || _Debug_crash(1);
}

var _Browser_go = F2(function(key, n)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		n && history.go(n);
		key();
	}));
});

var _Browser_pushUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.pushState({}, '', url);
		key();
	}));
});

var _Browser_replaceUrl = F2(function(key, url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function() {
		history.replaceState({}, '', url);
		key();
	}));
});



// GLOBAL EVENTS


var _Browser_fakeNode = { addEventListener: function() {}, removeEventListener: function() {} };
var _Browser_doc = typeof document !== 'undefined' ? document : _Browser_fakeNode;
var _Browser_window = typeof window !== 'undefined' ? window : _Browser_fakeNode;

var _Browser_on = F3(function(node, eventName, sendToSelf)
{
	return _Scheduler_spawn(_Scheduler_binding(function(callback)
	{
		function handler(event)	{ _Scheduler_rawSpawn(sendToSelf(event)); }
		node.addEventListener(eventName, handler, _VirtualDom_passiveSupported && { passive: true });
		return function() { node.removeEventListener(eventName, handler); };
	}));
});

var _Browser_decodeEvent = F2(function(decoder, event)
{
	var result = _Json_runHelp(decoder, event);
	return $elm$core$Result$isOk(result) ? $elm$core$Maybe$Just(result.a) : $elm$core$Maybe$Nothing;
});



// PAGE VISIBILITY


function _Browser_visibilityInfo()
{
	return (typeof _VirtualDom_doc.hidden !== 'undefined')
		? { hidden: 'hidden', change: 'visibilitychange' }
		:
	(typeof _VirtualDom_doc.mozHidden !== 'undefined')
		? { hidden: 'mozHidden', change: 'mozvisibilitychange' }
		:
	(typeof _VirtualDom_doc.msHidden !== 'undefined')
		? { hidden: 'msHidden', change: 'msvisibilitychange' }
		:
	(typeof _VirtualDom_doc.webkitHidden !== 'undefined')
		? { hidden: 'webkitHidden', change: 'webkitvisibilitychange' }
		: { hidden: 'hidden', change: 'visibilitychange' };
}



// ANIMATION FRAMES


function _Browser_rAF()
{
	return _Scheduler_binding(function(callback)
	{
		var id = _Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(Date.now()));
		});

		return function() {
			_Browser_cancelAnimationFrame(id);
		};
	});
}


function _Browser_now()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(Date.now()));
	});
}



// DOM STUFF


function _Browser_withNode(id, doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			var node = document.getElementById(id);
			callback(node
				? _Scheduler_succeed(doStuff(node))
				: _Scheduler_fail($elm$browser$Browser$Dom$NotFound(id))
			);
		});
	});
}


function _Browser_withWindow(doStuff)
{
	return _Scheduler_binding(function(callback)
	{
		_Browser_requestAnimationFrame(function() {
			callback(_Scheduler_succeed(doStuff()));
		});
	});
}


// FOCUS and BLUR


var _Browser_call = F2(function(functionName, id)
{
	return _Browser_withNode(id, function(node) {
		node[functionName]();
		return _Utils_Tuple0;
	});
});



// WINDOW VIEWPORT


function _Browser_getViewport()
{
	return {
		scene: _Browser_getScene(),
		viewport: {
			x: _Browser_window.pageXOffset,
			y: _Browser_window.pageYOffset,
			width: _Browser_doc.documentElement.clientWidth,
			height: _Browser_doc.documentElement.clientHeight
		}
	};
}

function _Browser_getScene()
{
	var body = _Browser_doc.body;
	var elem = _Browser_doc.documentElement;
	return {
		width: Math.max(body.scrollWidth, body.offsetWidth, elem.scrollWidth, elem.offsetWidth, elem.clientWidth),
		height: Math.max(body.scrollHeight, body.offsetHeight, elem.scrollHeight, elem.offsetHeight, elem.clientHeight)
	};
}

var _Browser_setViewport = F2(function(x, y)
{
	return _Browser_withWindow(function()
	{
		_Browser_window.scroll(x, y);
		return _Utils_Tuple0;
	});
});



// ELEMENT VIEWPORT


function _Browser_getViewportOf(id)
{
	return _Browser_withNode(id, function(node)
	{
		return {
			scene: {
				width: node.scrollWidth,
				height: node.scrollHeight
			},
			viewport: {
				x: node.scrollLeft,
				y: node.scrollTop,
				width: node.clientWidth,
				height: node.clientHeight
			}
		};
	});
}


var _Browser_setViewportOf = F3(function(id, x, y)
{
	return _Browser_withNode(id, function(node)
	{
		node.scrollLeft = x;
		node.scrollTop = y;
		return _Utils_Tuple0;
	});
});



// ELEMENT


function _Browser_getElement(id)
{
	return _Browser_withNode(id, function(node)
	{
		var rect = node.getBoundingClientRect();
		var x = _Browser_window.pageXOffset;
		var y = _Browser_window.pageYOffset;
		return {
			scene: _Browser_getScene(),
			viewport: {
				x: x,
				y: y,
				width: _Browser_doc.documentElement.clientWidth,
				height: _Browser_doc.documentElement.clientHeight
			},
			element: {
				x: x + rect.left,
				y: y + rect.top,
				width: rect.width,
				height: rect.height
			}
		};
	});
}



// LOAD and RELOAD


function _Browser_reload(skipCache)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		_VirtualDom_doc.location.reload(skipCache);
	}));
}

function _Browser_load(url)
{
	return A2($elm$core$Task$perform, $elm$core$Basics$never, _Scheduler_binding(function(callback)
	{
		try
		{
			_Browser_window.location = url;
		}
		catch(err)
		{
			// Only Firefox can throw a NS_ERROR_MALFORMED_URI exception here.
			// Other browsers reload the page, so let's be consistent about that.
			_VirtualDom_doc.location.reload(false);
		}
	}));
}



// DECODER

var _File_decoder = _Json_decodePrim(function(value) {
	// NOTE: checks if `File` exists in case this is run on node
	return (typeof File !== 'undefined' && value instanceof File)
		? $elm$core$Result$Ok(value)
		: _Json_expecting('a FILE', value);
});


// METADATA

function _File_name(file) { return file.name; }
function _File_mime(file) { return file.type; }
function _File_size(file) { return file.size; }

function _File_lastModified(file)
{
	return $elm$time$Time$millisToPosix(file.lastModified);
}


// DOWNLOAD

var _File_downloadNode;

function _File_getDownloadNode()
{
	return _File_downloadNode || (_File_downloadNode = document.createElement('a'));
}

var _File_download = F3(function(name, mime, content)
{
	return _Scheduler_binding(function(callback)
	{
		var blob = new Blob([content], {type: mime});

		// for IE10+
		if (navigator.msSaveOrOpenBlob)
		{
			navigator.msSaveOrOpenBlob(blob, name);
			return;
		}

		// for HTML5
		var node = _File_getDownloadNode();
		var objectUrl = URL.createObjectURL(blob);
		node.href = objectUrl;
		node.download = name;
		_File_click(node);
		URL.revokeObjectURL(objectUrl);
	});
});

function _File_downloadUrl(href)
{
	return _Scheduler_binding(function(callback)
	{
		var node = _File_getDownloadNode();
		node.href = href;
		node.download = '';
		node.origin === location.origin || (node.target = '_blank');
		_File_click(node);
	});
}


// IE COMPATIBILITY

function _File_makeBytesSafeForInternetExplorer(bytes)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/10
	// all other browsers can just run `new Blob([bytes])` directly with no problem
	//
	return new Uint8Array(bytes.buffer, bytes.byteOffset, bytes.byteLength);
}

function _File_click(node)
{
	// only needed by IE10 and IE11 to fix https://github.com/elm/file/issues/11
	// all other browsers have MouseEvent and do not need this conditional stuff
	//
	if (typeof MouseEvent === 'function')
	{
		node.dispatchEvent(new MouseEvent('click'));
	}
	else
	{
		var event = document.createEvent('MouseEvents');
		event.initMouseEvent('click', true, true, window, 0, 0, 0, 0, 0, false, false, false, false, 0, null);
		document.body.appendChild(node);
		node.dispatchEvent(event);
		document.body.removeChild(node);
	}
}


// UPLOAD

var _File_node;

function _File_uploadOne(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			callback(_Scheduler_succeed(event.target.files[0]));
		});
		_File_click(_File_node);
	});
}

function _File_uploadOneOrMore(mimes)
{
	return _Scheduler_binding(function(callback)
	{
		_File_node = document.createElement('input');
		_File_node.type = 'file';
		_File_node.multiple = true;
		_File_node.accept = A2($elm$core$String$join, ',', mimes);
		_File_node.addEventListener('change', function(event)
		{
			var elmFiles = _List_fromArray(event.target.files);
			callback(_Scheduler_succeed(_Utils_Tuple2(elmFiles.a, elmFiles.b)));
		});
		_File_click(_File_node);
	});
}


// CONTENT

function _File_toString(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsText(blob);
		return function() { reader.abort(); };
	});
}

function _File_toBytes(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(new DataView(reader.result)));
		});
		reader.readAsArrayBuffer(blob);
		return function() { reader.abort(); };
	});
}

function _File_toUrl(blob)
{
	return _Scheduler_binding(function(callback)
	{
		var reader = new FileReader();
		reader.addEventListener('loadend', function() {
			callback(_Scheduler_succeed(reader.result));
		});
		reader.readAsDataURL(blob);
		return function() { reader.abort(); };
	});
}




var _Bitwise_and = F2(function(a, b)
{
	return a & b;
});

var _Bitwise_or = F2(function(a, b)
{
	return a | b;
});

var _Bitwise_xor = F2(function(a, b)
{
	return a ^ b;
});

function _Bitwise_complement(a)
{
	return ~a;
};

var _Bitwise_shiftLeftBy = F2(function(offset, a)
{
	return a << offset;
});

var _Bitwise_shiftRightBy = F2(function(offset, a)
{
	return a >> offset;
});

var _Bitwise_shiftRightZfBy = F2(function(offset, a)
{
	return a >>> offset;
});



function _Time_now(millisToPosix)
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(millisToPosix(Date.now())));
	});
}

var _Time_setInterval = F2(function(interval, task)
{
	return _Scheduler_binding(function(callback)
	{
		var id = setInterval(function() { _Scheduler_rawSpawn(task); }, interval);
		return function() { clearInterval(id); };
	});
});

function _Time_here()
{
	return _Scheduler_binding(function(callback)
	{
		callback(_Scheduler_succeed(
			A2($elm$time$Time$customZone, -(new Date().getTimezoneOffset()), _List_Nil)
		));
	});
}


function _Time_getZoneName()
{
	return _Scheduler_binding(function(callback)
	{
		try
		{
			var name = $elm$time$Time$Name(Intl.DateTimeFormat().resolvedOptions().timeZone);
		}
		catch (e)
		{
			var name = $elm$time$Time$Offset(new Date().getTimezoneOffset());
		}
		callback(_Scheduler_succeed(name));
	});
}
var $elm$core$Basics$EQ = {$: 'EQ'};
var $elm$core$Basics$GT = {$: 'GT'};
var $elm$core$Basics$LT = {$: 'LT'};
var $elm$core$List$cons = _List_cons;
var $elm$core$Dict$foldr = F3(
	function (func, acc, t) {
		foldr:
		while (true) {
			if (t.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = t.b;
				var value = t.c;
				var left = t.d;
				var right = t.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldr, func, acc, right)),
					$temp$t = left;
				func = $temp$func;
				acc = $temp$acc;
				t = $temp$t;
				continue foldr;
			}
		}
	});
var $elm$core$Dict$toList = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, list) {
				return A2(
					$elm$core$List$cons,
					_Utils_Tuple2(key, value),
					list);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Dict$keys = function (dict) {
	return A3(
		$elm$core$Dict$foldr,
		F3(
			function (key, value, keyList) {
				return A2($elm$core$List$cons, key, keyList);
			}),
		_List_Nil,
		dict);
};
var $elm$core$Set$toList = function (_v0) {
	var dict = _v0.a;
	return $elm$core$Dict$keys(dict);
};
var $elm$core$Elm$JsArray$foldr = _JsArray_foldr;
var $elm$core$Array$foldr = F3(
	function (func, baseCase, _v0) {
		var tree = _v0.c;
		var tail = _v0.d;
		var helper = F2(
			function (node, acc) {
				if (node.$ === 'SubTree') {
					var subTree = node.a;
					return A3($elm$core$Elm$JsArray$foldr, helper, acc, subTree);
				} else {
					var values = node.a;
					return A3($elm$core$Elm$JsArray$foldr, func, acc, values);
				}
			});
		return A3(
			$elm$core$Elm$JsArray$foldr,
			helper,
			A3($elm$core$Elm$JsArray$foldr, func, baseCase, tail),
			tree);
	});
var $elm$core$Array$toList = function (array) {
	return A3($elm$core$Array$foldr, $elm$core$List$cons, _List_Nil, array);
};
var $elm$core$Result$Err = function (a) {
	return {$: 'Err', a: a};
};
var $elm$json$Json$Decode$Failure = F2(
	function (a, b) {
		return {$: 'Failure', a: a, b: b};
	});
var $elm$json$Json$Decode$Field = F2(
	function (a, b) {
		return {$: 'Field', a: a, b: b};
	});
var $elm$json$Json$Decode$Index = F2(
	function (a, b) {
		return {$: 'Index', a: a, b: b};
	});
var $elm$core$Result$Ok = function (a) {
	return {$: 'Ok', a: a};
};
var $elm$json$Json$Decode$OneOf = function (a) {
	return {$: 'OneOf', a: a};
};
var $elm$core$Basics$False = {$: 'False'};
var $elm$core$Basics$add = _Basics_add;
var $elm$core$Maybe$Just = function (a) {
	return {$: 'Just', a: a};
};
var $elm$core$Maybe$Nothing = {$: 'Nothing'};
var $elm$core$String$all = _String_all;
var $elm$core$Basics$and = _Basics_and;
var $elm$core$Basics$append = _Utils_append;
var $elm$json$Json$Encode$encode = _Json_encode;
var $elm$core$String$fromInt = _String_fromNumber;
var $elm$core$String$join = F2(
	function (sep, chunks) {
		return A2(
			_String_join,
			sep,
			_List_toArray(chunks));
	});
var $elm$core$String$split = F2(
	function (sep, string) {
		return _List_fromArray(
			A2(_String_split, sep, string));
	});
var $elm$json$Json$Decode$indent = function (str) {
	return A2(
		$elm$core$String$join,
		'\n    ',
		A2($elm$core$String$split, '\n', str));
};
var $elm$core$List$foldl = F3(
	function (func, acc, list) {
		foldl:
		while (true) {
			if (!list.b) {
				return acc;
			} else {
				var x = list.a;
				var xs = list.b;
				var $temp$func = func,
					$temp$acc = A2(func, x, acc),
					$temp$list = xs;
				func = $temp$func;
				acc = $temp$acc;
				list = $temp$list;
				continue foldl;
			}
		}
	});
var $elm$core$List$length = function (xs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, i) {
				return i + 1;
			}),
		0,
		xs);
};
var $elm$core$List$map2 = _List_map2;
var $elm$core$Basics$le = _Utils_le;
var $elm$core$Basics$sub = _Basics_sub;
var $elm$core$List$rangeHelp = F3(
	function (lo, hi, list) {
		rangeHelp:
		while (true) {
			if (_Utils_cmp(lo, hi) < 1) {
				var $temp$lo = lo,
					$temp$hi = hi - 1,
					$temp$list = A2($elm$core$List$cons, hi, list);
				lo = $temp$lo;
				hi = $temp$hi;
				list = $temp$list;
				continue rangeHelp;
			} else {
				return list;
			}
		}
	});
var $elm$core$List$range = F2(
	function (lo, hi) {
		return A3($elm$core$List$rangeHelp, lo, hi, _List_Nil);
	});
var $elm$core$List$indexedMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$map2,
			f,
			A2(
				$elm$core$List$range,
				0,
				$elm$core$List$length(xs) - 1),
			xs);
	});
var $elm$core$Char$toCode = _Char_toCode;
var $elm$core$Char$isLower = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (97 <= code) && (code <= 122);
};
var $elm$core$Char$isUpper = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 90) && (65 <= code);
};
var $elm$core$Basics$or = _Basics_or;
var $elm$core$Char$isAlpha = function (_char) {
	return $elm$core$Char$isLower(_char) || $elm$core$Char$isUpper(_char);
};
var $elm$core$Char$isDigit = function (_char) {
	var code = $elm$core$Char$toCode(_char);
	return (code <= 57) && (48 <= code);
};
var $elm$core$Char$isAlphaNum = function (_char) {
	return $elm$core$Char$isLower(_char) || ($elm$core$Char$isUpper(_char) || $elm$core$Char$isDigit(_char));
};
var $elm$core$List$reverse = function (list) {
	return A3($elm$core$List$foldl, $elm$core$List$cons, _List_Nil, list);
};
var $elm$core$String$uncons = _String_uncons;
var $elm$json$Json$Decode$errorOneOf = F2(
	function (i, error) {
		return '\n\n(' + ($elm$core$String$fromInt(i + 1) + (') ' + $elm$json$Json$Decode$indent(
			$elm$json$Json$Decode$errorToString(error))));
	});
var $elm$json$Json$Decode$errorToString = function (error) {
	return A2($elm$json$Json$Decode$errorToStringHelp, error, _List_Nil);
};
var $elm$json$Json$Decode$errorToStringHelp = F2(
	function (error, context) {
		errorToStringHelp:
		while (true) {
			switch (error.$) {
				case 'Field':
					var f = error.a;
					var err = error.b;
					var isSimple = function () {
						var _v1 = $elm$core$String$uncons(f);
						if (_v1.$ === 'Nothing') {
							return false;
						} else {
							var _v2 = _v1.a;
							var _char = _v2.a;
							var rest = _v2.b;
							return $elm$core$Char$isAlpha(_char) && A2($elm$core$String$all, $elm$core$Char$isAlphaNum, rest);
						}
					}();
					var fieldName = isSimple ? ('.' + f) : ('[\'' + (f + '\']'));
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, fieldName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'Index':
					var i = error.a;
					var err = error.b;
					var indexName = '[' + ($elm$core$String$fromInt(i) + ']');
					var $temp$error = err,
						$temp$context = A2($elm$core$List$cons, indexName, context);
					error = $temp$error;
					context = $temp$context;
					continue errorToStringHelp;
				case 'OneOf':
					var errors = error.a;
					if (!errors.b) {
						return 'Ran into a Json.Decode.oneOf with no possibilities' + function () {
							if (!context.b) {
								return '!';
							} else {
								return ' at json' + A2(
									$elm$core$String$join,
									'',
									$elm$core$List$reverse(context));
							}
						}();
					} else {
						if (!errors.b.b) {
							var err = errors.a;
							var $temp$error = err,
								$temp$context = context;
							error = $temp$error;
							context = $temp$context;
							continue errorToStringHelp;
						} else {
							var starter = function () {
								if (!context.b) {
									return 'Json.Decode.oneOf';
								} else {
									return 'The Json.Decode.oneOf at json' + A2(
										$elm$core$String$join,
										'',
										$elm$core$List$reverse(context));
								}
							}();
							var introduction = starter + (' failed in the following ' + ($elm$core$String$fromInt(
								$elm$core$List$length(errors)) + ' ways:'));
							return A2(
								$elm$core$String$join,
								'\n\n',
								A2(
									$elm$core$List$cons,
									introduction,
									A2($elm$core$List$indexedMap, $elm$json$Json$Decode$errorOneOf, errors)));
						}
					}
				default:
					var msg = error.a;
					var json = error.b;
					var introduction = function () {
						if (!context.b) {
							return 'Problem with the given value:\n\n';
						} else {
							return 'Problem with the value at json' + (A2(
								$elm$core$String$join,
								'',
								$elm$core$List$reverse(context)) + ':\n\n    ');
						}
					}();
					return introduction + ($elm$json$Json$Decode$indent(
						A2($elm$json$Json$Encode$encode, 4, json)) + ('\n\n' + msg));
			}
		}
	});
var $elm$core$Array$branchFactor = 32;
var $elm$core$Array$Array_elm_builtin = F4(
	function (a, b, c, d) {
		return {$: 'Array_elm_builtin', a: a, b: b, c: c, d: d};
	});
var $elm$core$Elm$JsArray$empty = _JsArray_empty;
var $elm$core$Basics$ceiling = _Basics_ceiling;
var $elm$core$Basics$fdiv = _Basics_fdiv;
var $elm$core$Basics$logBase = F2(
	function (base, number) {
		return _Basics_log(number) / _Basics_log(base);
	});
var $elm$core$Basics$toFloat = _Basics_toFloat;
var $elm$core$Array$shiftStep = $elm$core$Basics$ceiling(
	A2($elm$core$Basics$logBase, 2, $elm$core$Array$branchFactor));
var $elm$core$Array$empty = A4($elm$core$Array$Array_elm_builtin, 0, $elm$core$Array$shiftStep, $elm$core$Elm$JsArray$empty, $elm$core$Elm$JsArray$empty);
var $elm$core$Elm$JsArray$initialize = _JsArray_initialize;
var $elm$core$Array$Leaf = function (a) {
	return {$: 'Leaf', a: a};
};
var $elm$core$Basics$apL = F2(
	function (f, x) {
		return f(x);
	});
var $elm$core$Basics$apR = F2(
	function (x, f) {
		return f(x);
	});
var $elm$core$Basics$eq = _Utils_equal;
var $elm$core$Basics$floor = _Basics_floor;
var $elm$core$Elm$JsArray$length = _JsArray_length;
var $elm$core$Basics$gt = _Utils_gt;
var $elm$core$Basics$max = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) > 0) ? x : y;
	});
var $elm$core$Basics$mul = _Basics_mul;
var $elm$core$Array$SubTree = function (a) {
	return {$: 'SubTree', a: a};
};
var $elm$core$Elm$JsArray$initializeFromList = _JsArray_initializeFromList;
var $elm$core$Array$compressNodes = F2(
	function (nodes, acc) {
		compressNodes:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodes);
			var node = _v0.a;
			var remainingNodes = _v0.b;
			var newAcc = A2(
				$elm$core$List$cons,
				$elm$core$Array$SubTree(node),
				acc);
			if (!remainingNodes.b) {
				return $elm$core$List$reverse(newAcc);
			} else {
				var $temp$nodes = remainingNodes,
					$temp$acc = newAcc;
				nodes = $temp$nodes;
				acc = $temp$acc;
				continue compressNodes;
			}
		}
	});
var $elm$core$Tuple$first = function (_v0) {
	var x = _v0.a;
	return x;
};
var $elm$core$Array$treeFromBuilder = F2(
	function (nodeList, nodeListSize) {
		treeFromBuilder:
		while (true) {
			var newNodeSize = $elm$core$Basics$ceiling(nodeListSize / $elm$core$Array$branchFactor);
			if (newNodeSize === 1) {
				return A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, nodeList).a;
			} else {
				var $temp$nodeList = A2($elm$core$Array$compressNodes, nodeList, _List_Nil),
					$temp$nodeListSize = newNodeSize;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue treeFromBuilder;
			}
		}
	});
var $elm$core$Array$builderToArray = F2(
	function (reverseNodeList, builder) {
		if (!builder.nodeListSize) {
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail),
				$elm$core$Array$shiftStep,
				$elm$core$Elm$JsArray$empty,
				builder.tail);
		} else {
			var treeLen = builder.nodeListSize * $elm$core$Array$branchFactor;
			var depth = $elm$core$Basics$floor(
				A2($elm$core$Basics$logBase, $elm$core$Array$branchFactor, treeLen - 1));
			var correctNodeList = reverseNodeList ? $elm$core$List$reverse(builder.nodeList) : builder.nodeList;
			var tree = A2($elm$core$Array$treeFromBuilder, correctNodeList, builder.nodeListSize);
			return A4(
				$elm$core$Array$Array_elm_builtin,
				$elm$core$Elm$JsArray$length(builder.tail) + treeLen,
				A2($elm$core$Basics$max, 5, depth * $elm$core$Array$shiftStep),
				tree,
				builder.tail);
		}
	});
var $elm$core$Basics$idiv = _Basics_idiv;
var $elm$core$Basics$lt = _Utils_lt;
var $elm$core$Array$initializeHelp = F5(
	function (fn, fromIndex, len, nodeList, tail) {
		initializeHelp:
		while (true) {
			if (fromIndex < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					false,
					{nodeList: nodeList, nodeListSize: (len / $elm$core$Array$branchFactor) | 0, tail: tail});
			} else {
				var leaf = $elm$core$Array$Leaf(
					A3($elm$core$Elm$JsArray$initialize, $elm$core$Array$branchFactor, fromIndex, fn));
				var $temp$fn = fn,
					$temp$fromIndex = fromIndex - $elm$core$Array$branchFactor,
					$temp$len = len,
					$temp$nodeList = A2($elm$core$List$cons, leaf, nodeList),
					$temp$tail = tail;
				fn = $temp$fn;
				fromIndex = $temp$fromIndex;
				len = $temp$len;
				nodeList = $temp$nodeList;
				tail = $temp$tail;
				continue initializeHelp;
			}
		}
	});
var $elm$core$Basics$remainderBy = _Basics_remainderBy;
var $elm$core$Array$initialize = F2(
	function (len, fn) {
		if (len <= 0) {
			return $elm$core$Array$empty;
		} else {
			var tailLen = len % $elm$core$Array$branchFactor;
			var tail = A3($elm$core$Elm$JsArray$initialize, tailLen, len - tailLen, fn);
			var initialFromIndex = (len - tailLen) - $elm$core$Array$branchFactor;
			return A5($elm$core$Array$initializeHelp, fn, initialFromIndex, len, _List_Nil, tail);
		}
	});
var $elm$core$Basics$True = {$: 'True'};
var $elm$core$Result$isOk = function (result) {
	if (result.$ === 'Ok') {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$map = _Json_map1;
var $elm$json$Json$Decode$map2 = _Json_map2;
var $elm$json$Json$Decode$succeed = _Json_succeed;
var $elm$virtual_dom$VirtualDom$toHandlerInt = function (handler) {
	switch (handler.$) {
		case 'Normal':
			return 0;
		case 'MayStopPropagation':
			return 1;
		case 'MayPreventDefault':
			return 2;
		default:
			return 3;
	}
};
var $elm$browser$Browser$External = function (a) {
	return {$: 'External', a: a};
};
var $elm$browser$Browser$Internal = function (a) {
	return {$: 'Internal', a: a};
};
var $elm$core$Basics$identity = function (x) {
	return x;
};
var $elm$browser$Browser$Dom$NotFound = function (a) {
	return {$: 'NotFound', a: a};
};
var $elm$url$Url$Http = {$: 'Http'};
var $elm$url$Url$Https = {$: 'Https'};
var $elm$url$Url$Url = F6(
	function (protocol, host, port_, path, query, fragment) {
		return {fragment: fragment, host: host, path: path, port_: port_, protocol: protocol, query: query};
	});
var $elm$core$String$contains = _String_contains;
var $elm$core$String$length = _String_length;
var $elm$core$String$slice = _String_slice;
var $elm$core$String$dropLeft = F2(
	function (n, string) {
		return (n < 1) ? string : A3(
			$elm$core$String$slice,
			n,
			$elm$core$String$length(string),
			string);
	});
var $elm$core$String$indexes = _String_indexes;
var $elm$core$String$isEmpty = function (string) {
	return string === '';
};
var $elm$core$String$left = F2(
	function (n, string) {
		return (n < 1) ? '' : A3($elm$core$String$slice, 0, n, string);
	});
var $elm$core$String$toInt = _String_toInt;
var $elm$url$Url$chompBeforePath = F5(
	function (protocol, path, params, frag, str) {
		if ($elm$core$String$isEmpty(str) || A2($elm$core$String$contains, '@', str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, ':', str);
			if (!_v0.b) {
				return $elm$core$Maybe$Just(
					A6($elm$url$Url$Url, protocol, str, $elm$core$Maybe$Nothing, path, params, frag));
			} else {
				if (!_v0.b.b) {
					var i = _v0.a;
					var _v1 = $elm$core$String$toInt(
						A2($elm$core$String$dropLeft, i + 1, str));
					if (_v1.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var port_ = _v1;
						return $elm$core$Maybe$Just(
							A6(
								$elm$url$Url$Url,
								protocol,
								A2($elm$core$String$left, i, str),
								port_,
								path,
								params,
								frag));
					}
				} else {
					return $elm$core$Maybe$Nothing;
				}
			}
		}
	});
var $elm$url$Url$chompBeforeQuery = F4(
	function (protocol, params, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '/', str);
			if (!_v0.b) {
				return A5($elm$url$Url$chompBeforePath, protocol, '/', params, frag, str);
			} else {
				var i = _v0.a;
				return A5(
					$elm$url$Url$chompBeforePath,
					protocol,
					A2($elm$core$String$dropLeft, i, str),
					params,
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompBeforeFragment = F3(
	function (protocol, frag, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '?', str);
			if (!_v0.b) {
				return A4($elm$url$Url$chompBeforeQuery, protocol, $elm$core$Maybe$Nothing, frag, str);
			} else {
				var i = _v0.a;
				return A4(
					$elm$url$Url$chompBeforeQuery,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					frag,
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$url$Url$chompAfterProtocol = F2(
	function (protocol, str) {
		if ($elm$core$String$isEmpty(str)) {
			return $elm$core$Maybe$Nothing;
		} else {
			var _v0 = A2($elm$core$String$indexes, '#', str);
			if (!_v0.b) {
				return A3($elm$url$Url$chompBeforeFragment, protocol, $elm$core$Maybe$Nothing, str);
			} else {
				var i = _v0.a;
				return A3(
					$elm$url$Url$chompBeforeFragment,
					protocol,
					$elm$core$Maybe$Just(
						A2($elm$core$String$dropLeft, i + 1, str)),
					A2($elm$core$String$left, i, str));
			}
		}
	});
var $elm$core$String$startsWith = _String_startsWith;
var $elm$url$Url$fromString = function (str) {
	return A2($elm$core$String$startsWith, 'http://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Http,
		A2($elm$core$String$dropLeft, 7, str)) : (A2($elm$core$String$startsWith, 'https://', str) ? A2(
		$elm$url$Url$chompAfterProtocol,
		$elm$url$Url$Https,
		A2($elm$core$String$dropLeft, 8, str)) : $elm$core$Maybe$Nothing);
};
var $elm$core$Basics$never = function (_v0) {
	never:
	while (true) {
		var nvr = _v0.a;
		var $temp$_v0 = nvr;
		_v0 = $temp$_v0;
		continue never;
	}
};
var $elm$core$Task$Perform = function (a) {
	return {$: 'Perform', a: a};
};
var $elm$core$Task$succeed = _Scheduler_succeed;
var $elm$core$Task$init = $elm$core$Task$succeed(_Utils_Tuple0);
var $elm$core$List$foldrHelper = F4(
	function (fn, acc, ctr, ls) {
		if (!ls.b) {
			return acc;
		} else {
			var a = ls.a;
			var r1 = ls.b;
			if (!r1.b) {
				return A2(fn, a, acc);
			} else {
				var b = r1.a;
				var r2 = r1.b;
				if (!r2.b) {
					return A2(
						fn,
						a,
						A2(fn, b, acc));
				} else {
					var c = r2.a;
					var r3 = r2.b;
					if (!r3.b) {
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(fn, c, acc)));
					} else {
						var d = r3.a;
						var r4 = r3.b;
						var res = (ctr > 500) ? A3(
							$elm$core$List$foldl,
							fn,
							acc,
							$elm$core$List$reverse(r4)) : A4($elm$core$List$foldrHelper, fn, acc, ctr + 1, r4);
						return A2(
							fn,
							a,
							A2(
								fn,
								b,
								A2(
									fn,
									c,
									A2(fn, d, res))));
					}
				}
			}
		}
	});
var $elm$core$List$foldr = F3(
	function (fn, acc, ls) {
		return A4($elm$core$List$foldrHelper, fn, acc, 0, ls);
	});
var $elm$core$List$map = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					return A2(
						$elm$core$List$cons,
						f(x),
						acc);
				}),
			_List_Nil,
			xs);
	});
var $elm$core$Task$andThen = _Scheduler_andThen;
var $elm$core$Task$map = F2(
	function (func, taskA) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return $elm$core$Task$succeed(
					func(a));
			},
			taskA);
	});
var $elm$core$Task$map2 = F3(
	function (func, taskA, taskB) {
		return A2(
			$elm$core$Task$andThen,
			function (a) {
				return A2(
					$elm$core$Task$andThen,
					function (b) {
						return $elm$core$Task$succeed(
							A2(func, a, b));
					},
					taskB);
			},
			taskA);
	});
var $elm$core$Task$sequence = function (tasks) {
	return A3(
		$elm$core$List$foldr,
		$elm$core$Task$map2($elm$core$List$cons),
		$elm$core$Task$succeed(_List_Nil),
		tasks);
};
var $elm$core$Platform$sendToApp = _Platform_sendToApp;
var $elm$core$Task$spawnCmd = F2(
	function (router, _v0) {
		var task = _v0.a;
		return _Scheduler_spawn(
			A2(
				$elm$core$Task$andThen,
				$elm$core$Platform$sendToApp(router),
				task));
	});
var $elm$core$Task$onEffects = F3(
	function (router, commands, state) {
		return A2(
			$elm$core$Task$map,
			function (_v0) {
				return _Utils_Tuple0;
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Task$spawnCmd(router),
					commands)));
	});
var $elm$core$Task$onSelfMsg = F3(
	function (_v0, _v1, _v2) {
		return $elm$core$Task$succeed(_Utils_Tuple0);
	});
var $elm$core$Task$cmdMap = F2(
	function (tagger, _v0) {
		var task = _v0.a;
		return $elm$core$Task$Perform(
			A2($elm$core$Task$map, tagger, task));
	});
_Platform_effectManagers['Task'] = _Platform_createManager($elm$core$Task$init, $elm$core$Task$onEffects, $elm$core$Task$onSelfMsg, $elm$core$Task$cmdMap);
var $elm$core$Task$command = _Platform_leaf('Task');
var $elm$core$Task$perform = F2(
	function (toMessage, task) {
		return $elm$core$Task$command(
			$elm$core$Task$Perform(
				A2($elm$core$Task$map, toMessage, task)));
	});
var $elm$browser$Browser$element = _Browser_element;
var $author$project$Limits$NoBounds = {$: 'NoBounds'};
var $author$project$Main$NoType = {$: 'NoType'};
var $author$project$Limits$None = {$: 'None'};
var $author$project$DataSet$LabelFreq = F2(
	function (label, count) {
		return {count: count, label: label};
	});
var $author$project$DataSet$makeLabelFreq = function (pair) {
	var _v0 = pair;
	var lbl = _v0.a;
	var cnt = _v0.b;
	return A2($author$project$DataSet$LabelFreq, lbl, cnt);
};
var $elm$core$Tuple$second = function (_v0) {
	var y = _v0.b;
	return y;
};
var $author$project$DataSet$somaliFreq = _List_fromArray(
	[
		_Utils_Tuple2('O', 574),
		_Utils_Tuple2('A1', 130),
		_Utils_Tuple2('B', 166),
		_Utils_Tuple2('A2 or AB', 130)
	]);
var $author$project$DataSet$somaliBloodType = {
	counts: A2($elm$core$List$map, $elm$core$Tuple$second, $author$project$DataSet$somaliFreq),
	frequencies: A2($elm$core$List$map, $author$project$DataSet$makeLabelFreq, $author$project$DataSet$somaliFreq),
	labels: A2($elm$core$List$map, $elm$core$Tuple$first, $author$project$DataSet$somaliFreq),
	name: 'Somali Blood Types'
};
var $author$project$DataSet$tumaalFreq = _List_fromArray(
	[
		_Utils_Tuple2('A', 16),
		_Utils_Tuple2('AB', 1),
		_Utils_Tuple2('B', 11),
		_Utils_Tuple2('O', 26)
	]);
var $author$project$DataSet$tumaalBloodType = {
	counts: A2($elm$core$List$map, $elm$core$Tuple$second, $author$project$DataSet$tumaalFreq),
	frequencies: A2($elm$core$List$map, $author$project$DataSet$makeLabelFreq, $author$project$DataSet$tumaalFreq),
	labels: A2($elm$core$List$map, $elm$core$Tuple$first, $author$project$DataSet$tumaalFreq),
	name: 'Tumaal/Midgaan Blood Types'
};
var $author$project$DataSet$datasets = _List_fromArray(
	[$author$project$DataSet$somaliBloodType, $author$project$DataSet$tumaalBloodType]);
var $elm$core$Dict$RBEmpty_elm_builtin = {$: 'RBEmpty_elm_builtin'};
var $elm$core$Dict$empty = $elm$core$Dict$RBEmpty_elm_builtin;
var $rundis$elm_bootstrap$Bootstrap$Modal$Hide = {$: 'Hide'};
var $rundis$elm_bootstrap$Bootstrap$Modal$hidden = $rundis$elm_bootstrap$Bootstrap$Modal$Hide;
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area = F4(
	function (top, left, width, height) {
		return {height: height, left: left, top: top, width: width};
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed = {$: 'Closed'};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$State = function (a) {
	return {$: 'State', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$initialState = $rundis$elm_bootstrap$Bootstrap$Dropdown$State(
	{
		menuSize: A4($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area, 0, 0, 0, 0),
		status: $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed,
		toggleSize: A4($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$Area, 0, 0, 0, 0)
	});
var $author$project$Main$initModel = {binomGen: $elm$core$Maybe$Nothing, bootstrapSample: $elm$core$Maybe$Nothing, counts: $elm$core$Maybe$Nothing, csv: $elm$core$Maybe$Nothing, dataDropState: $rundis$elm_bootstrap$Bootstrap$Dropdown$initialState, datasets: $author$project$DataSet$datasets, distPlotConfig: $elm$core$Maybe$Nothing, fileName: 'None', fileType: $author$project$Main$NoType, isProportion: true, level: $elm$core$Maybe$Nothing, modalVisibility: $rundis$elm_bootstrap$Bootstrap$Modal$hidden, originalSample: $elm$core$Maybe$Nothing, perspectiveData: $elm$core$Maybe$Nothing, selected: $elm$core$Maybe$Nothing, selectedData: $elm$core$Maybe$Nothing, selectedVariable: $elm$core$Maybe$Nothing, successDropState: $rundis$elm_bootstrap$Bootstrap$Dropdown$initialState, tail: $author$project$Limits$None, tailLimit: $author$project$Limits$NoBounds, trials: 0, variables: $elm$core$Maybe$Nothing, ys: $elm$core$Dict$empty};
var $elm$core$Platform$Cmd$batch = _Platform_batch;
var $elm$core$Platform$Cmd$none = $elm$core$Platform$Cmd$batch(_List_Nil);
var $author$project$Main$init = function (_v0) {
	return _Utils_Tuple2($author$project$Main$initModel, $elm$core$Platform$Cmd$none);
};
var $author$project$Main$DataDropMsg = function (a) {
	return {$: 'DataDropMsg', a: a};
};
var $author$project$Main$SuccessDropMsg = function (a) {
	return {$: 'SuccessDropMsg', a: a};
};
var $elm$core$Platform$Sub$batch = _Platform_batch;
var $rundis$elm_bootstrap$Bootstrap$Dropdown$ListenClicks = {$: 'ListenClicks'};
var $elm$core$Platform$Sub$none = $elm$core$Platform$Sub$batch(_List_Nil);
var $elm$browser$Browser$AnimationManager$Time = function (a) {
	return {$: 'Time', a: a};
};
var $elm$browser$Browser$AnimationManager$State = F3(
	function (subs, request, oldTime) {
		return {oldTime: oldTime, request: request, subs: subs};
	});
var $elm$browser$Browser$AnimationManager$init = $elm$core$Task$succeed(
	A3($elm$browser$Browser$AnimationManager$State, _List_Nil, $elm$core$Maybe$Nothing, 0));
var $elm$core$Process$kill = _Scheduler_kill;
var $elm$browser$Browser$AnimationManager$now = _Browser_now(_Utils_Tuple0);
var $elm$browser$Browser$AnimationManager$rAF = _Browser_rAF(_Utils_Tuple0);
var $elm$core$Platform$sendToSelf = _Platform_sendToSelf;
var $elm$core$Process$spawn = _Scheduler_spawn;
var $elm$browser$Browser$AnimationManager$onEffects = F3(
	function (router, subs, _v0) {
		var request = _v0.request;
		var oldTime = _v0.oldTime;
		var _v1 = _Utils_Tuple2(request, subs);
		if (_v1.a.$ === 'Nothing') {
			if (!_v1.b.b) {
				var _v2 = _v1.a;
				return $elm$browser$Browser$AnimationManager$init;
			} else {
				var _v4 = _v1.a;
				return A2(
					$elm$core$Task$andThen,
					function (pid) {
						return A2(
							$elm$core$Task$andThen,
							function (time) {
								return $elm$core$Task$succeed(
									A3(
										$elm$browser$Browser$AnimationManager$State,
										subs,
										$elm$core$Maybe$Just(pid),
										time));
							},
							$elm$browser$Browser$AnimationManager$now);
					},
					$elm$core$Process$spawn(
						A2(
							$elm$core$Task$andThen,
							$elm$core$Platform$sendToSelf(router),
							$elm$browser$Browser$AnimationManager$rAF)));
			}
		} else {
			if (!_v1.b.b) {
				var pid = _v1.a.a;
				return A2(
					$elm$core$Task$andThen,
					function (_v3) {
						return $elm$browser$Browser$AnimationManager$init;
					},
					$elm$core$Process$kill(pid));
			} else {
				return $elm$core$Task$succeed(
					A3($elm$browser$Browser$AnimationManager$State, subs, request, oldTime));
			}
		}
	});
var $elm$time$Time$Posix = function (a) {
	return {$: 'Posix', a: a};
};
var $elm$time$Time$millisToPosix = $elm$time$Time$Posix;
var $elm$browser$Browser$AnimationManager$onSelfMsg = F3(
	function (router, newTime, _v0) {
		var subs = _v0.subs;
		var oldTime = _v0.oldTime;
		var send = function (sub) {
			if (sub.$ === 'Time') {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(
						$elm$time$Time$millisToPosix(newTime)));
			} else {
				var tagger = sub.a;
				return A2(
					$elm$core$Platform$sendToApp,
					router,
					tagger(newTime - oldTime));
			}
		};
		return A2(
			$elm$core$Task$andThen,
			function (pid) {
				return A2(
					$elm$core$Task$andThen,
					function (_v1) {
						return $elm$core$Task$succeed(
							A3(
								$elm$browser$Browser$AnimationManager$State,
								subs,
								$elm$core$Maybe$Just(pid),
								newTime));
					},
					$elm$core$Task$sequence(
						A2($elm$core$List$map, send, subs)));
			},
			$elm$core$Process$spawn(
				A2(
					$elm$core$Task$andThen,
					$elm$core$Platform$sendToSelf(router),
					$elm$browser$Browser$AnimationManager$rAF)));
	});
var $elm$browser$Browser$AnimationManager$Delta = function (a) {
	return {$: 'Delta', a: a};
};
var $elm$core$Basics$composeL = F3(
	function (g, f, x) {
		return g(
			f(x));
	});
var $elm$browser$Browser$AnimationManager$subMap = F2(
	function (func, sub) {
		if (sub.$ === 'Time') {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Time(
				A2($elm$core$Basics$composeL, func, tagger));
		} else {
			var tagger = sub.a;
			return $elm$browser$Browser$AnimationManager$Delta(
				A2($elm$core$Basics$composeL, func, tagger));
		}
	});
_Platform_effectManagers['Browser.AnimationManager'] = _Platform_createManager($elm$browser$Browser$AnimationManager$init, $elm$browser$Browser$AnimationManager$onEffects, $elm$browser$Browser$AnimationManager$onSelfMsg, 0, $elm$browser$Browser$AnimationManager$subMap);
var $elm$browser$Browser$AnimationManager$subscription = _Platform_leaf('Browser.AnimationManager');
var $elm$browser$Browser$AnimationManager$onAnimationFrame = function (tagger) {
	return $elm$browser$Browser$AnimationManager$subscription(
		$elm$browser$Browser$AnimationManager$Time(tagger));
};
var $elm$browser$Browser$Events$onAnimationFrame = $elm$browser$Browser$AnimationManager$onAnimationFrame;
var $elm$browser$Browser$Events$Document = {$: 'Document'};
var $elm$browser$Browser$Events$MySub = F3(
	function (a, b, c) {
		return {$: 'MySub', a: a, b: b, c: c};
	});
var $elm$browser$Browser$Events$State = F2(
	function (subs, pids) {
		return {pids: pids, subs: subs};
	});
var $elm$browser$Browser$Events$init = $elm$core$Task$succeed(
	A2($elm$browser$Browser$Events$State, _List_Nil, $elm$core$Dict$empty));
var $elm$browser$Browser$Events$nodeToKey = function (node) {
	if (node.$ === 'Document') {
		return 'd_';
	} else {
		return 'w_';
	}
};
var $elm$browser$Browser$Events$addKey = function (sub) {
	var node = sub.a;
	var name = sub.b;
	return _Utils_Tuple2(
		_Utils_ap(
			$elm$browser$Browser$Events$nodeToKey(node),
			name),
		sub);
};
var $elm$core$Dict$Black = {$: 'Black'};
var $elm$core$Dict$RBNode_elm_builtin = F5(
	function (a, b, c, d, e) {
		return {$: 'RBNode_elm_builtin', a: a, b: b, c: c, d: d, e: e};
	});
var $elm$core$Dict$Red = {$: 'Red'};
var $elm$core$Dict$balance = F5(
	function (color, key, value, left, right) {
		if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Red')) {
			var _v1 = right.a;
			var rK = right.b;
			var rV = right.c;
			var rLeft = right.d;
			var rRight = right.e;
			if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
				var _v3 = left.a;
				var lK = left.b;
				var lV = left.c;
				var lLeft = left.d;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					key,
					value,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					rK,
					rV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, left, rLeft),
					rRight);
			}
		} else {
			if ((((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) && (left.d.$ === 'RBNode_elm_builtin')) && (left.d.a.$ === 'Red')) {
				var _v5 = left.a;
				var lK = left.b;
				var lV = left.c;
				var _v6 = left.d;
				var _v7 = _v6.a;
				var llK = _v6.b;
				var llV = _v6.c;
				var llLeft = _v6.d;
				var llRight = _v6.e;
				var lRight = left.e;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Red,
					lK,
					lV,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, key, value, lRight, right));
			} else {
				return A5($elm$core$Dict$RBNode_elm_builtin, color, key, value, left, right);
			}
		}
	});
var $elm$core$Basics$compare = _Utils_compare;
var $elm$core$Dict$insertHelp = F3(
	function (key, value, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, $elm$core$Dict$RBEmpty_elm_builtin, $elm$core$Dict$RBEmpty_elm_builtin);
		} else {
			var nColor = dict.a;
			var nKey = dict.b;
			var nValue = dict.c;
			var nLeft = dict.d;
			var nRight = dict.e;
			var _v1 = A2($elm$core$Basics$compare, key, nKey);
			switch (_v1.$) {
				case 'LT':
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						A3($elm$core$Dict$insertHelp, key, value, nLeft),
						nRight);
				case 'EQ':
					return A5($elm$core$Dict$RBNode_elm_builtin, nColor, nKey, value, nLeft, nRight);
				default:
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						nLeft,
						A3($elm$core$Dict$insertHelp, key, value, nRight));
			}
		}
	});
var $elm$core$Dict$insert = F3(
	function (key, value, dict) {
		var _v0 = A3($elm$core$Dict$insertHelp, key, value, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$fromList = function (assocs) {
	return A3(
		$elm$core$List$foldl,
		F2(
			function (_v0, dict) {
				var key = _v0.a;
				var value = _v0.b;
				return A3($elm$core$Dict$insert, key, value, dict);
			}),
		$elm$core$Dict$empty,
		assocs);
};
var $elm$core$Dict$foldl = F3(
	function (func, acc, dict) {
		foldl:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return acc;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var $temp$func = func,
					$temp$acc = A3(
					func,
					key,
					value,
					A3($elm$core$Dict$foldl, func, acc, left)),
					$temp$dict = right;
				func = $temp$func;
				acc = $temp$acc;
				dict = $temp$dict;
				continue foldl;
			}
		}
	});
var $elm$core$Dict$merge = F6(
	function (leftStep, bothStep, rightStep, leftDict, rightDict, initialResult) {
		var stepState = F3(
			function (rKey, rValue, _v0) {
				stepState:
				while (true) {
					var list = _v0.a;
					var result = _v0.b;
					if (!list.b) {
						return _Utils_Tuple2(
							list,
							A3(rightStep, rKey, rValue, result));
					} else {
						var _v2 = list.a;
						var lKey = _v2.a;
						var lValue = _v2.b;
						var rest = list.b;
						if (_Utils_cmp(lKey, rKey) < 0) {
							var $temp$rKey = rKey,
								$temp$rValue = rValue,
								$temp$_v0 = _Utils_Tuple2(
								rest,
								A3(leftStep, lKey, lValue, result));
							rKey = $temp$rKey;
							rValue = $temp$rValue;
							_v0 = $temp$_v0;
							continue stepState;
						} else {
							if (_Utils_cmp(lKey, rKey) > 0) {
								return _Utils_Tuple2(
									list,
									A3(rightStep, rKey, rValue, result));
							} else {
								return _Utils_Tuple2(
									rest,
									A4(bothStep, lKey, lValue, rValue, result));
							}
						}
					}
				}
			});
		var _v3 = A3(
			$elm$core$Dict$foldl,
			stepState,
			_Utils_Tuple2(
				$elm$core$Dict$toList(leftDict),
				initialResult),
			rightDict);
		var leftovers = _v3.a;
		var intermediateResult = _v3.b;
		return A3(
			$elm$core$List$foldl,
			F2(
				function (_v4, result) {
					var k = _v4.a;
					var v = _v4.b;
					return A3(leftStep, k, v, result);
				}),
			intermediateResult,
			leftovers);
	});
var $elm$browser$Browser$Events$Event = F2(
	function (key, event) {
		return {event: event, key: key};
	});
var $elm$browser$Browser$Events$spawn = F3(
	function (router, key, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var actualNode = function () {
			if (node.$ === 'Document') {
				return _Browser_doc;
			} else {
				return _Browser_window;
			}
		}();
		return A2(
			$elm$core$Task$map,
			function (value) {
				return _Utils_Tuple2(key, value);
			},
			A3(
				_Browser_on,
				actualNode,
				name,
				function (event) {
					return A2(
						$elm$core$Platform$sendToSelf,
						router,
						A2($elm$browser$Browser$Events$Event, key, event));
				}));
	});
var $elm$core$Dict$union = F2(
	function (t1, t2) {
		return A3($elm$core$Dict$foldl, $elm$core$Dict$insert, t2, t1);
	});
var $elm$browser$Browser$Events$onEffects = F3(
	function (router, subs, state) {
		var stepRight = F3(
			function (key, sub, _v6) {
				var deads = _v6.a;
				var lives = _v6.b;
				var news = _v6.c;
				return _Utils_Tuple3(
					deads,
					lives,
					A2(
						$elm$core$List$cons,
						A3($elm$browser$Browser$Events$spawn, router, key, sub),
						news));
			});
		var stepLeft = F3(
			function (_v4, pid, _v5) {
				var deads = _v5.a;
				var lives = _v5.b;
				var news = _v5.c;
				return _Utils_Tuple3(
					A2($elm$core$List$cons, pid, deads),
					lives,
					news);
			});
		var stepBoth = F4(
			function (key, pid, _v2, _v3) {
				var deads = _v3.a;
				var lives = _v3.b;
				var news = _v3.c;
				return _Utils_Tuple3(
					deads,
					A3($elm$core$Dict$insert, key, pid, lives),
					news);
			});
		var newSubs = A2($elm$core$List$map, $elm$browser$Browser$Events$addKey, subs);
		var _v0 = A6(
			$elm$core$Dict$merge,
			stepLeft,
			stepBoth,
			stepRight,
			state.pids,
			$elm$core$Dict$fromList(newSubs),
			_Utils_Tuple3(_List_Nil, $elm$core$Dict$empty, _List_Nil));
		var deadPids = _v0.a;
		var livePids = _v0.b;
		var makeNewPids = _v0.c;
		return A2(
			$elm$core$Task$andThen,
			function (pids) {
				return $elm$core$Task$succeed(
					A2(
						$elm$browser$Browser$Events$State,
						newSubs,
						A2(
							$elm$core$Dict$union,
							livePids,
							$elm$core$Dict$fromList(pids))));
			},
			A2(
				$elm$core$Task$andThen,
				function (_v1) {
					return $elm$core$Task$sequence(makeNewPids);
				},
				$elm$core$Task$sequence(
					A2($elm$core$List$map, $elm$core$Process$kill, deadPids))));
	});
var $elm$core$List$maybeCons = F3(
	function (f, mx, xs) {
		var _v0 = f(mx);
		if (_v0.$ === 'Just') {
			var x = _v0.a;
			return A2($elm$core$List$cons, x, xs);
		} else {
			return xs;
		}
	});
var $elm$core$List$filterMap = F2(
	function (f, xs) {
		return A3(
			$elm$core$List$foldr,
			$elm$core$List$maybeCons(f),
			_List_Nil,
			xs);
	});
var $elm$browser$Browser$Events$onSelfMsg = F3(
	function (router, _v0, state) {
		var key = _v0.key;
		var event = _v0.event;
		var toMessage = function (_v2) {
			var subKey = _v2.a;
			var _v3 = _v2.b;
			var node = _v3.a;
			var name = _v3.b;
			var decoder = _v3.c;
			return _Utils_eq(subKey, key) ? A2(_Browser_decodeEvent, decoder, event) : $elm$core$Maybe$Nothing;
		};
		var messages = A2($elm$core$List$filterMap, toMessage, state.subs);
		return A2(
			$elm$core$Task$andThen,
			function (_v1) {
				return $elm$core$Task$succeed(state);
			},
			$elm$core$Task$sequence(
				A2(
					$elm$core$List$map,
					$elm$core$Platform$sendToApp(router),
					messages)));
	});
var $elm$browser$Browser$Events$subMap = F2(
	function (func, _v0) {
		var node = _v0.a;
		var name = _v0.b;
		var decoder = _v0.c;
		return A3(
			$elm$browser$Browser$Events$MySub,
			node,
			name,
			A2($elm$json$Json$Decode$map, func, decoder));
	});
_Platform_effectManagers['Browser.Events'] = _Platform_createManager($elm$browser$Browser$Events$init, $elm$browser$Browser$Events$onEffects, $elm$browser$Browser$Events$onSelfMsg, 0, $elm$browser$Browser$Events$subMap);
var $elm$browser$Browser$Events$subscription = _Platform_leaf('Browser.Events');
var $elm$browser$Browser$Events$on = F3(
	function (node, name, decoder) {
		return $elm$browser$Browser$Events$subscription(
			A3($elm$browser$Browser$Events$MySub, node, name, decoder));
	});
var $elm$browser$Browser$Events$onClick = A2($elm$browser$Browser$Events$on, $elm$browser$Browser$Events$Document, 'click');
var $rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus = F2(
	function (status, _v0) {
		var stateRec = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Dropdown$State(
			_Utils_update(
				stateRec,
				{status: status}));
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions = F2(
	function (state, toMsg) {
		var status = state.a.status;
		switch (status.$) {
			case 'Open':
				return $elm$browser$Browser$Events$onAnimationFrame(
					function (_v1) {
						return toMsg(
							A2($rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus, $rundis$elm_bootstrap$Bootstrap$Dropdown$ListenClicks, state));
					});
			case 'ListenClicks':
				return $elm$browser$Browser$Events$onClick(
					$elm$json$Json$Decode$succeed(
						toMsg(
							A2($rundis$elm_bootstrap$Bootstrap$Dropdown$updateStatus, $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed, state))));
			default:
				return $elm$core$Platform$Sub$none;
		}
	});
var $author$project$Main$subscriptions = function (model) {
	return $elm$core$Platform$Sub$batch(
		_List_fromArray(
			[
				A2($rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions, model.dataDropState, $author$project$Main$DataDropMsg),
				A2($rundis$elm_bootstrap$Bootstrap$Dropdown$subscriptions, model.successDropState, $author$project$Main$SuccessDropMsg)
			]));
};
var $author$project$Main$CsvLoaded = function (a) {
	return {$: 'CsvLoaded', a: a};
};
var $author$project$Main$CsvSelected = function (a) {
	return {$: 'CsvSelected', a: a};
};
var $author$project$Main$Frequency = {$: 'Frequency'};
var $author$project$Main$NewStatistics = function (a) {
	return {$: 'NewStatistics', a: a};
};
var $author$project$Main$Regular = {$: 'Regular'};
var $elm$core$Maybe$andThen = F2(
	function (callback, maybeValue) {
		if (maybeValue.$ === 'Just') {
			var value = maybeValue.a;
			return callback(value);
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$Main$bootstrapPlotToJS = _Platform_outgoingPort('bootstrapPlotToJS', $elm$core$Basics$identity);
var $gicentre$elm_vegalite$VegaLite$Nominal = {$: 'Nominal'};
var $gicentre$elm_vegalite$VegaLite$Quantitative = {$: 'Quantitative'};
var $gicentre$elm_vegalite$VegaLite$X = {$: 'X'};
var $gicentre$elm_vegalite$VegaLite$Y = {$: 'Y'};
var $gicentre$elm_vegalite$VegaLite$GridOpacity = function (a) {
	return {$: 'GridOpacity', a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoGridOpacity = $gicentre$elm_vegalite$VegaLite$GridOpacity;
var $gicentre$elm_vegalite$VegaLite$LabelAngle = function (a) {
	return {$: 'LabelAngle', a: a};
};
var $gicentre$elm_vegalite$VegaLite$axcoLabelAngle = $gicentre$elm_vegalite$VegaLite$LabelAngle;
var $gicentre$elm_vegalite$VegaLite$Bar = {$: 'Bar'};
var $gicentre$elm_vegalite$VegaLite$VLMark = {$: 'VLMark'};
var $gicentre$elm_vegalite$VegaLite$markLabel = function (m) {
	switch (m.$) {
		case 'Area':
			return 'area';
		case 'Bar':
			return 'bar';
		case 'Boxplot':
			return 'boxplot';
		case 'Circle':
			return 'circle';
		case 'Errorband':
			return 'errorband';
		case 'Errorbar':
			return 'errorbar';
		case 'Line':
			return 'line';
		case 'Geoshape':
			return 'geoshape';
		case 'Point':
			return 'point';
		case 'Rect':
			return 'rect';
		case 'Rule':
			return 'rule';
		case 'Square':
			return 'square';
		case 'Text':
			return 'text';
		case 'Tick':
			return 'tick';
		default:
			return 'trail';
	}
};
var $gicentre$elm_vegalite$VegaLite$TTNone = {$: 'TTNone'};
var $elm$json$Json$Encode$bool = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$cursorLabel = function (cur) {
	switch (cur.$) {
		case 'CAuto':
			return 'auto';
		case 'CDefault':
			return 'default';
		case 'CNone':
			return 'none';
		case 'CContextMenu':
			return 'context-menu';
		case 'CHelp':
			return 'help';
		case 'CPointer':
			return 'pointer';
		case 'CProgress':
			return 'progress';
		case 'CWait':
			return 'wait';
		case 'CCell':
			return 'cell';
		case 'CCrosshair':
			return 'crosshair';
		case 'CText':
			return 'text';
		case 'CVerticalText':
			return 'vertical-text';
		case 'CAlias':
			return 'alias';
		case 'CCopy':
			return 'copy';
		case 'CMove':
			return 'move';
		case 'CNoDrop':
			return 'no-drop';
		case 'CNotAllowed':
			return 'not-allowed';
		case 'CAllScroll':
			return 'all-scroll';
		case 'CColResize':
			return 'col-resize';
		case 'CRowResize':
			return 'row-resize';
		case 'CNResize':
			return 'n-resize';
		case 'CEResize':
			return 'e-resize';
		case 'CSResize':
			return 's-resize';
		case 'CWResize':
			return 'w-resize';
		case 'CNEResize':
			return 'ne-resize';
		case 'CNWResize':
			return 'nw-resize';
		case 'CSEResize':
			return 'se-resize';
		case 'CSWResize':
			return 'sw-resize';
		case 'CEWResize':
			return 'ew-resize';
		case 'CNSResize':
			return 'ns-resize';
		case 'CNESWResize':
			return 'nesw-resize';
		case 'CNWSEResize':
			return 'nwse-resize';
		case 'CZoomIn':
			return 'zoom-in';
		case 'CZoomOut':
			return 'zoom-out';
		case 'CGrab':
			return 'grab';
		default:
			return 'grabbing';
	}
};
var $elm$json$Json$Encode$float = _Json_wrap;
var $elm$json$Json$Encode$string = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$extentSpec = function (ext) {
	switch (ext.$) {
		case 'ExCI':
			return $elm$json$Json$Encode$string('ci');
		case 'ExStderr':
			return $elm$json$Json$Encode$string('stderr');
		case 'ExStdev':
			return $elm$json$Json$Encode$string('stdev');
		case 'ExIqr':
			return $elm$json$Json$Encode$string('iqr');
		case 'ExRange':
			return $elm$json$Json$Encode$string('min-max');
		default:
			var sc = ext.a;
			return $elm$json$Json$Encode$float(sc);
	}
};
var $gicentre$elm_vegalite$VegaLite$fontWeightSpec = function (w) {
	switch (w.$) {
		case 'Normal':
			return $elm$json$Json$Encode$string('normal');
		case 'Bold':
			return $elm$json$Json$Encode$string('bold');
		case 'Bolder':
			return $elm$json$Json$Encode$string('bolder');
		case 'Lighter':
			return $elm$json$Json$Encode$string('lighter');
		case 'W100':
			return $elm$json$Json$Encode$float(100);
		case 'W200':
			return $elm$json$Json$Encode$float(200);
		case 'W300':
			return $elm$json$Json$Encode$float(300);
		case 'W400':
			return $elm$json$Json$Encode$float(400);
		case 'W500':
			return $elm$json$Json$Encode$float(500);
		case 'W600':
			return $elm$json$Json$Encode$float(600);
		case 'W700':
			return $elm$json$Json$Encode$float(700);
		case 'W800':
			return $elm$json$Json$Encode$float(800);
		default:
			return $elm$json$Json$Encode$float(900);
	}
};
var $gicentre$elm_vegalite$VegaLite$hAlignLabel = function (al) {
	switch (al.$) {
		case 'AlignLeft':
			return 'left';
		case 'AlignCenter':
			return 'center';
		default:
			return 'right';
	}
};
var $elm$json$Json$Encode$list = F2(
	function (func, entries) {
		return _Json_wrap(
			A3(
				$elm$core$List$foldl,
				_Json_addEntry(func),
				_Json_emptyArray(_Utils_Tuple0),
				entries));
	});
var $gicentre$elm_vegalite$VegaLite$markInterpolationLabel = function (interp) {
	switch (interp.$) {
		case 'Linear':
			return 'linear';
		case 'LinearClosed':
			return 'linear-closed';
		case 'Stepwise':
			return 'step';
		case 'StepBefore':
			return 'step-before';
		case 'StepAfter':
			return 'step-after';
		case 'Basis':
			return 'basis';
		case 'BasisOpen':
			return 'basis-open';
		case 'BasisClosed':
			return 'basis-closed';
		case 'Cardinal':
			return 'cardinal';
		case 'CardinalOpen':
			return 'cardinal-open';
		case 'CardinalClosed':
			return 'cardinal-closed';
		case 'Bundle':
			return 'bundle';
		default:
			return 'monotone';
	}
};
var $gicentre$elm_vegalite$VegaLite$markOrientationLabel = function (orient) {
	if (orient.$ === 'MOHorizontal') {
		return 'horizontal';
	} else {
		return 'vertical';
	}
};
var $elm$json$Json$Encode$null = _Json_encodeNull;
var $elm$json$Json$Encode$object = function (pairs) {
	return _Json_wrap(
		A3(
			$elm$core$List$foldl,
			F2(
				function (_v0, obj) {
					var k = _v0.a;
					var v = _v0.b;
					return A3(_Json_addField, k, v, obj);
				}),
			_Json_emptyObject(_Utils_Tuple0),
			pairs));
};
var $gicentre$elm_vegalite$VegaLite$strokeCapLabel = function (cap) {
	switch (cap.$) {
		case 'CButt':
			return 'butt';
		case 'CRound':
			return 'round';
		default:
			return 'square';
	}
};
var $gicentre$elm_vegalite$VegaLite$strokeJoinLabel = function (jn) {
	switch (jn.$) {
		case 'JMiter':
			return 'miter';
		case 'JRound':
			return 'round';
		default:
			return 'bevel';
	}
};
var $gicentre$elm_vegalite$VegaLite$symbolLabel = function (sym) {
	switch (sym.$) {
		case 'SymCircle':
			return 'circle';
		case 'SymSquare':
			return 'square';
		case 'SymCross':
			return 'cross';
		case 'SymDiamond':
			return 'diamond';
		case 'SymTriangleUp':
			return 'triangle-up';
		case 'SymTriangleDown':
			return 'triangle-down';
		default:
			var svgPath = sym.a;
			return svgPath;
	}
};
var $gicentre$elm_vegalite$VegaLite$ttContentLabel = function (ttContent) {
	switch (ttContent.$) {
		case 'TTEncoding':
			return 'encoding';
		case 'TTData':
			return 'data';
		default:
			return 'null';
	}
};
var $gicentre$elm_vegalite$VegaLite$vAlignLabel = function (al) {
	switch (al.$) {
		case 'AlignTop':
			return 'top';
		case 'AlignMiddle':
			return 'middle';
		default:
			return 'bottom';
	}
};
var $gicentre$elm_vegalite$VegaLite$lineMarkerSpec = function (pm) {
	if (pm.$ === 'LMNone') {
		return $elm$json$Json$Encode$bool(false);
	} else {
		var mps = pm.a;
		return $elm$json$Json$Encode$object(
			A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps));
	}
};
var $gicentre$elm_vegalite$VegaLite$markProperty = function (mProp) {
	switch (mProp.$) {
		case 'MFilled':
			var b = mProp.a;
			return _Utils_Tuple2(
				'filled',
				$elm$json$Json$Encode$bool(b));
		case 'MClip':
			var b = mProp.a;
			return _Utils_Tuple2(
				'clip',
				$elm$json$Json$Encode$bool(b));
		case 'MColor':
			var col = mProp.a;
			return _Utils_Tuple2(
				'color',
				$elm$json$Json$Encode$string(col));
		case 'MCursor':
			var cur = mProp.a;
			return _Utils_Tuple2(
				'cursor',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$cursorLabel(cur)));
		case 'MExtent':
			var ext = mProp.a;
			return _Utils_Tuple2(
				'extent',
				$gicentre$elm_vegalite$VegaLite$extentSpec(ext));
		case 'MHRef':
			var s = mProp.a;
			return _Utils_Tuple2(
				'href',
				$elm$json$Json$Encode$string(s));
		case 'MFill':
			var col = mProp.a;
			return _Utils_Tuple2(
				'fill',
				$elm$json$Json$Encode$string(col));
		case 'MStroke':
			var col = mProp.a;
			return _Utils_Tuple2(
				'stroke',
				$elm$json$Json$Encode$string(col));
		case 'MStrokeCap':
			var sc = mProp.a;
			return _Utils_Tuple2(
				'strokeCap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$strokeCapLabel(sc)));
		case 'MStrokeJoin':
			var sj = mProp.a;
			return _Utils_Tuple2(
				'strokeJoin',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$strokeJoinLabel(sj)));
		case 'MStrokeMiterLimit':
			var ml = mProp.a;
			return _Utils_Tuple2(
				'strokeMiterLimit',
				$elm$json$Json$Encode$float(ml));
		case 'MOpacity':
			var x = mProp.a;
			return _Utils_Tuple2(
				'opacity',
				$elm$json$Json$Encode$float(x));
		case 'MFillOpacity':
			var x = mProp.a;
			return _Utils_Tuple2(
				'fillOpacity',
				$elm$json$Json$Encode$float(x));
		case 'MStrokeOpacity':
			var x = mProp.a;
			return _Utils_Tuple2(
				'strokeOpacity',
				$elm$json$Json$Encode$float(x));
		case 'MStrokeWidth':
			var x = mProp.a;
			return _Utils_Tuple2(
				'strokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'MStrokeDash':
			var xs = mProp.a;
			return _Utils_Tuple2(
				'strokeDash',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		case 'MStrokeDashOffset':
			var x = mProp.a;
			return _Utils_Tuple2(
				'strokeDashOffset',
				$elm$json$Json$Encode$float(x));
		case 'MStyle':
			var styles = mProp.a;
			return _Utils_Tuple2(
				'style',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, styles));
		case 'MInterpolate':
			var interp = mProp.a;
			return _Utils_Tuple2(
				'interpolate',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markInterpolationLabel(interp)));
		case 'MTension':
			var x = mProp.a;
			return _Utils_Tuple2(
				'tension',
				$elm$json$Json$Encode$float(x));
		case 'MOrient':
			var orient = mProp.a;
			return _Utils_Tuple2(
				'orient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markOrientationLabel(orient)));
		case 'MShape':
			var sym = mProp.a;
			return _Utils_Tuple2(
				'shape',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$symbolLabel(sym)));
		case 'MSize':
			var x = mProp.a;
			return _Utils_Tuple2(
				'size',
				$elm$json$Json$Encode$float(x));
		case 'MAngle':
			var x = mProp.a;
			return _Utils_Tuple2(
				'angle',
				$elm$json$Json$Encode$float(x));
		case 'MAlign':
			var al = mProp.a;
			return _Utils_Tuple2(
				'align',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(al)));
		case 'MBaseline':
			var va = mProp.a;
			return _Utils_Tuple2(
				'baseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'MdX':
			var dx = mProp.a;
			return _Utils_Tuple2(
				'dx',
				$elm$json$Json$Encode$float(dx));
		case 'MdY':
			var dy = mProp.a;
			return _Utils_Tuple2(
				'dy',
				$elm$json$Json$Encode$float(dy));
		case 'MFont':
			var fnt = mProp.a;
			return _Utils_Tuple2(
				'font',
				$elm$json$Json$Encode$string(fnt));
		case 'MFontSize':
			var x = mProp.a;
			return _Utils_Tuple2(
				'fontSize',
				$elm$json$Json$Encode$float(x));
		case 'MFontStyle':
			var fSty = mProp.a;
			return _Utils_Tuple2(
				'fontStyle',
				$elm$json$Json$Encode$string(fSty));
		case 'MFontWeight':
			var w = mProp.a;
			return _Utils_Tuple2(
				'fontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w));
		case 'MRadius':
			var x = mProp.a;
			return _Utils_Tuple2(
				'radius',
				$elm$json$Json$Encode$float(x));
		case 'MText':
			var txt = mProp.a;
			return _Utils_Tuple2(
				'text',
				$elm$json$Json$Encode$string(txt));
		case 'MTheta':
			var x = mProp.a;
			return _Utils_Tuple2(
				'theta',
				$elm$json$Json$Encode$float(x));
		case 'MBinSpacing':
			var x = mProp.a;
			return _Utils_Tuple2(
				'binSpacing',
				$elm$json$Json$Encode$float(x));
		case 'MContinuousBandSize':
			var x = mProp.a;
			return _Utils_Tuple2(
				'continuousBandSize',
				$elm$json$Json$Encode$float(x));
		case 'MDiscreteBandSize':
			var x = mProp.a;
			return _Utils_Tuple2(
				'discreteBandSize',
				$elm$json$Json$Encode$float(x));
		case 'MShortTimeLabels':
			var b = mProp.a;
			return _Utils_Tuple2(
				'shortTimeLabels',
				$elm$json$Json$Encode$bool(b));
		case 'MBandSize':
			var x = mProp.a;
			return _Utils_Tuple2(
				'bandSize',
				$elm$json$Json$Encode$float(x));
		case 'MThickness':
			var x = mProp.a;
			return _Utils_Tuple2(
				'thickness',
				$elm$json$Json$Encode$float(x));
		case 'MRule':
			var props = mProp.a;
			return _Utils_Tuple2(
				'rule',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MBorders':
			var props = mProp.a;
			return _Utils_Tuple2(
				'borders',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MMedian':
			var props = mProp.a;
			return _Utils_Tuple2(
				'median',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MBox':
			var props = mProp.a;
			return _Utils_Tuple2(
				'box',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MOutliers':
			var props = mProp.a;
			return _Utils_Tuple2(
				'outliers',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MTicks':
			var props = mProp.a;
			return _Utils_Tuple2(
				'ticks',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, props)));
		case 'MTooltip':
			var ttContent = mProp.a;
			return _Utils_eq(ttContent, $gicentre$elm_vegalite$VegaLite$TTNone) ? _Utils_Tuple2('tooltip', $elm$json$Json$Encode$null) : _Utils_Tuple2(
				'tooltip',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'content',
							$elm$json$Json$Encode$string(
								$gicentre$elm_vegalite$VegaLite$ttContentLabel(ttContent)))
						])));
		case 'MPoint':
			var pm = mProp.a;
			return _Utils_Tuple2(
				'point',
				$gicentre$elm_vegalite$VegaLite$pointMarkerSpec(pm));
		case 'MLine':
			var lm = mProp.a;
			return _Utils_Tuple2(
				'line',
				$gicentre$elm_vegalite$VegaLite$lineMarkerSpec(lm));
		case 'MX':
			var x = mProp.a;
			return _Utils_Tuple2(
				'x',
				$elm$json$Json$Encode$float(x));
		case 'MY':
			var y = mProp.a;
			return _Utils_Tuple2(
				'y',
				$elm$json$Json$Encode$float(y));
		case 'MX2':
			var x = mProp.a;
			return _Utils_Tuple2(
				'x2',
				$elm$json$Json$Encode$float(x));
		case 'MY2':
			var y = mProp.a;
			return _Utils_Tuple2(
				'y2',
				$elm$json$Json$Encode$float(y));
		case 'MOrder':
			var b = mProp.a;
			return _Utils_Tuple2(
				'order',
				$elm$json$Json$Encode$bool(b));
		case 'MXOffset':
			var o = mProp.a;
			return _Utils_Tuple2(
				'xOffset',
				$elm$json$Json$Encode$float(o));
		case 'MX2Offset':
			var o = mProp.a;
			return _Utils_Tuple2(
				'x2Offset',
				$elm$json$Json$Encode$float(o));
		case 'MYOffset':
			var o = mProp.a;
			return _Utils_Tuple2(
				'yOffset',
				$elm$json$Json$Encode$float(o));
		default:
			var o = mProp.a;
			return _Utils_Tuple2(
				'y2Offset',
				$elm$json$Json$Encode$float(o));
	}
};
var $gicentre$elm_vegalite$VegaLite$pointMarkerSpec = function (pm) {
	switch (pm.$) {
		case 'PMTransparent':
			return $elm$json$Json$Encode$string('transparent');
		case 'PMNone':
			return $elm$json$Json$Encode$bool(false);
		default:
			var mps = pm.a;
			return $elm$json$Json$Encode$object(
				A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps));
	}
};
var $gicentre$elm_vegalite$VegaLite$mark = F2(
	function (m, mProps) {
		if (!mProps.b) {
			return _Utils_Tuple2(
				$gicentre$elm_vegalite$VegaLite$VLMark,
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markLabel(m)));
		} else {
			return _Utils_Tuple2(
				$gicentre$elm_vegalite$VegaLite$VLMark,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'type',
							$elm$json$Json$Encode$string(
								$gicentre$elm_vegalite$VegaLite$markLabel(m))),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mProps))));
		}
	});
var $gicentre$elm_vegalite$VegaLite$bar = $gicentre$elm_vegalite$VegaLite$mark($gicentre$elm_vegalite$VegaLite$Bar);
var $gicentre$elm_vegalite$VegaLite$Axis = function (a) {
	return {$: 'Axis', a: a};
};
var $gicentre$elm_vegalite$VegaLite$coAxis = $gicentre$elm_vegalite$VegaLite$Axis;
var $elm$core$List$append = F2(
	function (xs, ys) {
		if (!ys.b) {
			return xs;
		} else {
			return A3($elm$core$List$foldr, $elm$core$List$cons, ys, xs);
		}
	});
var $elm$core$List$concat = function (lists) {
	return A3($elm$core$List$foldr, $elm$core$List$append, _List_Nil, lists);
};
var $elm$core$List$concatMap = F2(
	function (f, list) {
		return $elm$core$List$concat(
			A2($elm$core$List$map, f, list));
	});
var $gicentre$elm_vegalite$VegaLite$arrangementLabel = function (arrng) {
	switch (arrng.$) {
		case 'Row':
			return 'row';
		case 'Column':
			return 'column';
		default:
			return 'repeat';
	}
};
var $elm$json$Json$Encode$int = _Json_wrap;
var $gicentre$elm_vegalite$VegaLite$binProperty = function (binProp) {
	switch (binProp.$) {
		case 'MaxBins':
			var n = binProp.a;
			return _Utils_Tuple2(
				'maxbins',
				$elm$json$Json$Encode$int(n));
		case 'Base':
			var x = binProp.a;
			return _Utils_Tuple2(
				'base',
				$elm$json$Json$Encode$float(x));
		case 'Step':
			var x = binProp.a;
			return _Utils_Tuple2(
				'step',
				$elm$json$Json$Encode$float(x));
		case 'Steps':
			var xs = binProp.a;
			return _Utils_Tuple2(
				'steps',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		case 'MinStep':
			var x = binProp.a;
			return _Utils_Tuple2(
				'minstep',
				$elm$json$Json$Encode$float(x));
		case 'Divides':
			var xs = binProp.a;
			return _Utils_Tuple2(
				'divide',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		case 'Extent':
			var mn = binProp.a;
			var mx = binProp.b;
			return _Utils_Tuple2(
				'extent',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$float,
					_List_fromArray(
						[mn, mx])));
		default:
			var b = binProp.a;
			return _Utils_Tuple2(
				'nice',
				$elm$json$Json$Encode$bool(b));
	}
};
var $gicentre$elm_vegalite$VegaLite$bin = function (bProps) {
	return _Utils_eq(bProps, _List_Nil) ? _Utils_Tuple2(
		'bin',
		$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
		'bin',
		$elm$json$Json$Encode$object(
			A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$binProperty, bProps)));
};
var $gicentre$elm_vegalite$VegaLite$booleanOpSpec = function (bo) {
	switch (bo.$) {
		case 'Expr':
			var ex = bo.a;
			return $elm$json$Json$Encode$string(ex);
		case 'SelectionName':
			var selName = bo.a;
			return $elm$json$Json$Encode$string(selName);
		case 'Selection':
			var sel = bo.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'selection',
						$elm$json$Json$Encode$string(sel))
					]));
		case 'And':
			var operand1 = bo.a;
			var operand2 = bo.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'and',
						A2(
							$elm$json$Json$Encode$list,
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec,
							_List_fromArray(
								[operand1, operand2])))
					]));
		case 'Or':
			var operand1 = bo.a;
			var operand2 = bo.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'or',
						A2(
							$elm$json$Json$Encode$list,
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec,
							_List_fromArray(
								[operand1, operand2])))
					]));
		default:
			var operand = bo.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'not',
						$gicentre$elm_vegalite$VegaLite$booleanOpSpec(operand))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$dayLabel = function (dayName) {
	switch (dayName.$) {
		case 'Mon':
			return 'Mon';
		case 'Tue':
			return 'Tue';
		case 'Wed':
			return 'Wed';
		case 'Thu':
			return 'Thu';
		case 'Fri':
			return 'Fri';
		case 'Sat':
			return 'Sat';
		default:
			return 'Sun';
	}
};
var $gicentre$elm_vegalite$VegaLite$monthNameLabel = function (mon) {
	switch (mon.$) {
		case 'Jan':
			return 'Jan';
		case 'Feb':
			return 'Feb';
		case 'Mar':
			return 'Mar';
		case 'Apr':
			return 'Apr';
		case 'May':
			return 'May';
		case 'Jun':
			return 'Jun';
		case 'Jul':
			return 'Jul';
		case 'Aug':
			return 'Aug';
		case 'Sep':
			return 'Sep';
		case 'Oct':
			return 'Oct';
		case 'Nov':
			return 'Nov';
		default:
			return 'Dec';
	}
};
var $gicentre$elm_vegalite$VegaLite$dateTimeProperty = function (dtp) {
	switch (dtp.$) {
		case 'DTYear':
			var y = dtp.a;
			return _Utils_Tuple2(
				'year',
				$elm$json$Json$Encode$int(y));
		case 'DTQuarter':
			var q = dtp.a;
			return _Utils_Tuple2(
				'quarter',
				$elm$json$Json$Encode$int(q));
		case 'DTMonth':
			var mon = dtp.a;
			return _Utils_Tuple2(
				'month',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$monthNameLabel(mon)));
		case 'DTDate':
			var d = dtp.a;
			return _Utils_Tuple2(
				'date',
				$elm$json$Json$Encode$int(d));
		case 'DTDay':
			var d = dtp.a;
			return _Utils_Tuple2(
				'day',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$dayLabel(d)));
		case 'DTHours':
			var h = dtp.a;
			return _Utils_Tuple2(
				'hours',
				$elm$json$Json$Encode$int(h));
		case 'DTMinutes':
			var m = dtp.a;
			return _Utils_Tuple2(
				'minutes',
				$elm$json$Json$Encode$int(m));
		case 'DTSeconds':
			var s = dtp.a;
			return _Utils_Tuple2(
				'seconds',
				$elm$json$Json$Encode$int(s));
		default:
			var ms = dtp.a;
			return _Utils_Tuple2(
				'milliseconds',
				$elm$json$Json$Encode$int(ms));
	}
};
var $gicentre$elm_vegalite$VegaLite$dataValueSpec = function (val) {
	switch (val.$) {
		case 'Number':
			var x = val.a;
			return $elm$json$Json$Encode$float(x);
		case 'Str':
			var s = val.a;
			return $elm$json$Json$Encode$string(s);
		case 'Boolean':
			var b = val.a;
			return $elm$json$Json$Encode$bool(b);
		default:
			var d = val.a;
			return $elm$json$Json$Encode$object(
				A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
	}
};
var $gicentre$elm_vegalite$VegaLite$dataValuesSpecs = function (dvs) {
	switch (dvs.$) {
		case 'Numbers':
			var xs = dvs.a;
			return A2($elm$core$List$map, $elm$json$Json$Encode$float, xs);
		case 'Strings':
			var ss = dvs.a;
			return A2($elm$core$List$map, $elm$json$Json$Encode$string, ss);
		case 'DateTimes':
			var dtss = dvs.a;
			return A2(
				$elm$core$List$map,
				function (ds) {
					return $elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, ds));
				},
				dtss);
		default:
			var bs = dvs.a;
			return A2($elm$core$List$map, $elm$json$Json$Encode$bool, bs);
	}
};
var $gicentre$elm_vegalite$VegaLite$imMethodLabel = function (method) {
	switch (method.$) {
		case 'ImValue':
			return 'value';
		case 'ImMean':
			return 'mean';
		case 'ImMedian':
			return 'median';
		case 'ImMax':
			return 'max';
		default:
			return 'min';
	}
};
var $gicentre$elm_vegalite$VegaLite$toList = $elm$json$Json$Encode$list($elm$core$Basics$identity);
var $gicentre$elm_vegalite$VegaLite$imputeProperty = function (ip) {
	switch (ip.$) {
		case 'ImFrame':
			if (ip.a.$ === 'Just') {
				if (ip.b.$ === 'Just') {
					var n1 = ip.a.a;
					var n2 = ip.b.a;
					return _Utils_Tuple2(
						'frame',
						A2(
							$elm$json$Json$Encode$list,
							$elm$json$Json$Encode$int,
							_List_fromArray(
								[n1, n2])));
				} else {
					var n1 = ip.a.a;
					var _v2 = ip.b;
					return _Utils_Tuple2(
						'frame',
						$gicentre$elm_vegalite$VegaLite$toList(
							_List_fromArray(
								[
									$elm$json$Json$Encode$int(n1),
									$elm$json$Json$Encode$null
								])));
				}
			} else {
				if (ip.b.$ === 'Just') {
					var _v1 = ip.a;
					var n2 = ip.b.a;
					return _Utils_Tuple2(
						'frame',
						$gicentre$elm_vegalite$VegaLite$toList(
							_List_fromArray(
								[
									$elm$json$Json$Encode$null,
									$elm$json$Json$Encode$int(n2)
								])));
				} else {
					var _v3 = ip.a;
					var _v4 = ip.b;
					return _Utils_Tuple2(
						'frame',
						$gicentre$elm_vegalite$VegaLite$toList(
							_List_fromArray(
								[$elm$json$Json$Encode$null, $elm$json$Json$Encode$null])));
				}
			}
		case 'ImKeyVals':
			var dVals = ip.a;
			return _Utils_Tuple2(
				'keyvals',
				$gicentre$elm_vegalite$VegaLite$toList(
					$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(dVals)));
		case 'ImKeyValSequence':
			var start = ip.a;
			var stop = ip.b;
			var step = ip.c;
			return _Utils_Tuple2(
				'keyvals',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'start',
							$elm$json$Json$Encode$float(start)),
							_Utils_Tuple2(
							'stop',
							$elm$json$Json$Encode$float(stop)),
							_Utils_Tuple2(
							'step',
							$elm$json$Json$Encode$float(step))
						])));
		case 'ImMethod':
			var method = ip.a;
			return _Utils_Tuple2(
				'method',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$imMethodLabel(method)));
		case 'ImNewValue':
			var dVal = ip.a;
			return _Utils_Tuple2(
				'value',
				$gicentre$elm_vegalite$VegaLite$dataValueSpec(dVal));
		default:
			return _Utils_Tuple2('groupby', $elm$json$Json$Encode$null);
	}
};
var $gicentre$elm_vegalite$VegaLite$compositionAlignmentSpec = function (ca) {
	switch (ca.$) {
		case 'CANone':
			return $elm$json$Json$Encode$string('none');
		case 'CAEach':
			return $elm$json$Json$Encode$string('each');
		default:
			return $elm$json$Json$Encode$string('all');
	}
};
var $gicentre$elm_vegalite$VegaLite$legendOrientLabel = function (orient) {
	switch (orient.$) {
		case 'Left':
			return 'left';
		case 'TopLeft':
			return 'top-left';
		case 'Top':
			return 'top';
		case 'TopRight':
			return 'top-right';
		case 'Right':
			return 'right';
		case 'BottomRight':
			return 'bottom-right';
		case 'Bottom':
			return 'bottom';
		case 'BottomLeft':
			return 'bottom-left';
		default:
			return 'none';
	}
};
var $gicentre$elm_vegalite$VegaLite$overlapStrategyLabel = function (strat) {
	switch (strat.$) {
		case 'ONone':
			return 'false';
		case 'OParity':
			return 'parity';
		default:
			return 'greedy';
	}
};
var $gicentre$elm_vegalite$VegaLite$legendProperty = function (legendProp) {
	switch (legendProp.$) {
		case 'LClipHeight':
			var h = legendProp.a;
			return _Utils_Tuple2(
				'clipHeight',
				$elm$json$Json$Encode$float(h));
		case 'LColumnPadding':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'columnPadding',
				$elm$json$Json$Encode$float(n));
		case 'LRowPadding':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'rowPadding',
				$elm$json$Json$Encode$float(n));
		case 'LColumns':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'columns',
				$elm$json$Json$Encode$float(n));
		case 'LCornerRadius':
			var r = legendProp.a;
			return _Utils_Tuple2(
				'cornerRadius',
				$elm$json$Json$Encode$float(r));
		case 'LFillColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'fillColor',
				$elm$json$Json$Encode$string(s));
		case 'LDirection':
			var d = legendProp.a;
			return _Utils_Tuple2(
				'direction',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)));
		case 'LType':
			var lType = legendProp.a;
			if (lType.$ === 'Gradient') {
				return _Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('gradient'));
			} else {
				return _Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('symbol'));
			}
		case 'LFormat':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'format',
				$elm$json$Json$Encode$string(s));
		case 'LFormatAsNum':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('number'));
		case 'LFormatAsTemporal':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('time'));
		case 'LGradientLength':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'gradientLength',
				$elm$json$Json$Encode$float(n));
		case 'LGradientThickness':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'gradientThickness',
				$elm$json$Json$Encode$float(n));
		case 'LGradientStrokeColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'gradientStrokeColor',
				$elm$json$Json$Encode$string(s));
		case 'LGradientStrokeWidth':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'gradientStrokeWidth',
				$elm$json$Json$Encode$float(n));
		case 'LGridAlign':
			var ga = legendProp.a;
			return _Utils_Tuple2(
				'gridAlign',
				$gicentre$elm_vegalite$VegaLite$compositionAlignmentSpec(ga));
		case 'LLabelAlign':
			var ha = legendProp.a;
			return _Utils_Tuple2(
				'labelAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'LLabelBaseline':
			var va = legendProp.a;
			return _Utils_Tuple2(
				'labelBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'LLabelColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'labelColor',
				$elm$json$Json$Encode$string(s));
		case 'LLabelFont':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'labelFont',
				$elm$json$Json$Encode$string(s));
		case 'LLabelFontSize':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'labelFontSize',
				$elm$json$Json$Encode$float(x));
		case 'LLabelLimit':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'labelLimit',
				$elm$json$Json$Encode$float(x));
		case 'LLabelOffset':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'labelOffset',
				$elm$json$Json$Encode$float(x));
		case 'LLabelOverlap':
			var lo = legendProp.a;
			return _Utils_Tuple2(
				'labelOverlap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$overlapStrategyLabel(lo)));
		case 'LOffset':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$float(x));
		case 'LOrient':
			var orient = legendProp.a;
			return _Utils_Tuple2(
				'orient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$legendOrientLabel(orient)));
		case 'LPadding':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'padding',
				$elm$json$Json$Encode$float(x));
		case 'LStrokeColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'strokeColor',
				$elm$json$Json$Encode$string(s));
		case 'LStrokeWidth':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'strokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'LSymbolFillColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'symbolFillColor',
				$elm$json$Json$Encode$string(s));
		case 'LSymbolStrokeColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'symbolStrokeColor',
				$elm$json$Json$Encode$string(s));
		case 'LSymbolType':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'symbolType',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$symbolLabel(s)));
		case 'LSymbolSize':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'symbolSize',
				$elm$json$Json$Encode$float(x));
		case 'LSymbolStrokeWidth':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'symbolStrokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'LTickCount':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'tickCount',
				$elm$json$Json$Encode$float(x));
		case 'LTitle':
			var s = legendProp.a;
			return (s === '') ? _Utils_Tuple2('title', $elm$json$Json$Encode$null) : _Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(s));
		case 'LTitleAlign':
			var ha = legendProp.a;
			return _Utils_Tuple2(
				'titleAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'LTitleBaseline':
			var va = legendProp.a;
			return _Utils_Tuple2(
				'titleBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'LTitleColor':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'titleColor',
				$elm$json$Json$Encode$string(s));
		case 'LTitleFont':
			var s = legendProp.a;
			return _Utils_Tuple2(
				'titleFont',
				$elm$json$Json$Encode$string(s));
		case 'LTitleFontSize':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'titleFontSize',
				$elm$json$Json$Encode$float(x));
		case 'LTitleFontWeight':
			var fw = legendProp.a;
			return _Utils_Tuple2(
				'titleFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw));
		case 'LTitleLimit':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'titleLimit',
				$elm$json$Json$Encode$float(x));
		case 'LTitlePadding':
			var x = legendProp.a;
			return _Utils_Tuple2(
				'titlePadding',
				$elm$json$Json$Encode$float(x));
		case 'LValues':
			var vals = legendProp.a;
			var list = function () {
				switch (vals.$) {
					case 'LNumbers':
						var xs = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs);
					case 'LDateTimes':
						var ds = vals.a;
						return A2(
							$elm$json$Json$Encode$list,
							function (d) {
								return $elm$json$Json$Encode$object(
									A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
							},
							ds);
					default:
						var ss = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
				}
			}();
			return _Utils_Tuple2('values', list);
		case 'LeX':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'legendX',
				$elm$json$Json$Encode$float(n));
		case 'LeY':
			var n = legendProp.a;
			return _Utils_Tuple2(
				'legendY',
				$elm$json$Json$Encode$float(n));
		default:
			var n = legendProp.a;
			return _Utils_Tuple2(
				'zindex',
				$elm$json$Json$Encode$int(n));
	}
};
var $gicentre$elm_vegalite$VegaLite$measurementLabel = function (mType) {
	switch (mType.$) {
		case 'Nominal':
			return 'nominal';
		case 'Ordinal':
			return 'ordinal';
		case 'Quantitative':
			return 'quantitative';
		case 'Temporal':
			return 'temporal';
		default:
			return 'geojson';
	}
};
var $gicentre$elm_vegalite$VegaLite$operationLabel = function (op) {
	switch (op.$) {
		case 'ArgMax':
			return 'argmax';
		case 'ArgMin':
			return 'argmin';
		case 'Count':
			return 'count';
		case 'CI0':
			return 'ci0';
		case 'CI1':
			return 'ci1';
		case 'Distinct':
			return 'distinct';
		case 'Max':
			return 'max';
		case 'Mean':
			return 'mean';
		case 'Median':
			return 'median';
		case 'Min':
			return 'min';
		case 'Missing':
			return 'missing';
		case 'Q1':
			return 'q1';
		case 'Q3':
			return 'q3';
		case 'Stdev':
			return 'stdev';
		case 'StdevP':
			return 'stdevp';
		case 'Sum':
			return 'sum';
		case 'Stderr':
			return 'stderr';
		case 'Valid':
			return 'valid';
		case 'Variance':
			return 'variance';
		default:
			return 'variancep';
	}
};
var $gicentre$elm_vegalite$VegaLite$cInterpolateSpec = function (iType) {
	switch (iType.$) {
		case 'Rgb':
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('rgb')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
		case 'Hsl':
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hsl'))
					]));
		case 'HslLong':
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hsl-long'))
					]));
		case 'Lab':
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('lab'))
					]));
		case 'Hcl':
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hcl'))
					]));
		case 'HclLong':
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('hcl-long'))
					]));
		case 'CubeHelix':
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('cubehelix')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
		default:
			var gamma = iType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'type',
						$elm$json$Json$Encode$string('cubehelix-long')),
						_Utils_Tuple2(
						'gamma',
						$elm$json$Json$Encode$float(gamma))
					]));
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleDomainSpec = function (sdType) {
	switch (sdType.$) {
		case 'DNumbers':
			var ns = sdType.a;
			return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, ns);
		case 'DDateTimes':
			var ds = sdType.a;
			return A2(
				$elm$json$Json$Encode$list,
				function (d) {
					return $elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
				},
				ds);
		case 'DStrings':
			var cats = sdType.a;
			return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, cats);
		case 'DSelection':
			var selName = sdType.a;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'selection',
						$elm$json$Json$Encode$string(selName))
					]));
		default:
			return $elm$json$Json$Encode$string('unaggregated');
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleLabel = function (sc) {
	switch (sc.$) {
		case 'ScLinear':
			return 'linear';
		case 'ScPow':
			return 'pow';
		case 'ScSymLog':
			return 'symlog';
		case 'ScSqrt':
			return 'sqrt';
		case 'ScLog':
			return 'log';
		case 'ScTime':
			return 'time';
		case 'ScUtc':
			return 'utc';
		case 'ScOrdinal':
			return 'ordinal';
		case 'ScBand':
			return 'band';
		case 'ScPoint':
			return 'point';
		case 'ScBinLinear':
			return 'bin-linear';
		case 'ScBinOrdinal':
			return 'bin-ordinal';
		case 'ScQuantile':
			return 'quantile';
		case 'ScQuantize':
			return 'quantize';
		default:
			return 'threshold';
	}
};
var $gicentre$elm_vegalite$VegaLite$timeUnitLabel = function (tu) {
	switch (tu.$) {
		case 'Year':
			return 'year';
		case 'YearQuarter':
			return 'yearquarter';
		case 'YearQuarterMonth':
			return 'yearquartermonth';
		case 'YearMonth':
			return 'yearmonth';
		case 'YearMonthDate':
			return 'yearmonthdate';
		case 'YearMonthDateHours':
			return 'yearmonthdatehours';
		case 'YearMonthDateHoursMinutes':
			return 'yearmonthdatehoursminutes';
		case 'YearMonthDateHoursMinutesSeconds':
			return 'yearmonthdatehoursminutesseconds';
		case 'Quarter':
			return 'quarter';
		case 'QuarterMonth':
			return 'quartermonth';
		case 'Month':
			return 'month';
		case 'MonthDate':
			return 'monthdate';
		case 'MonthDateHours':
			return 'monthdatehours';
		case 'Date':
			return 'date';
		case 'Day':
			return 'day';
		case 'Hours':
			return 'hours';
		case 'HoursMinutes':
			return 'hoursminutes';
		case 'HoursMinutesSeconds':
			return 'hoursminutesseconds';
		case 'Minutes':
			return 'minutes';
		case 'MinutesSeconds':
			return 'minutesseconds';
		case 'Seconds':
			return 'seconds';
		case 'SecondsMilliseconds':
			return 'secondsmilliseconds';
		case 'Milliseconds':
			return 'milliseconds';
		default:
			var timeUnit = tu.a;
			return 'utc' + $gicentre$elm_vegalite$VegaLite$timeUnitLabel(timeUnit);
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleNiceSpec = function (ni) {
	switch (ni.$) {
		case 'NMillisecond':
			return $elm$json$Json$Encode$string('millisecond');
		case 'NSecond':
			return $elm$json$Json$Encode$string('second');
		case 'NMinute':
			return $elm$json$Json$Encode$string('minute');
		case 'NHour':
			return $elm$json$Json$Encode$string('hour');
		case 'NDay':
			return $elm$json$Json$Encode$string('day');
		case 'NWeek':
			return $elm$json$Json$Encode$string('week');
		case 'NMonth':
			return $elm$json$Json$Encode$string('month');
		case 'NYear':
			return $elm$json$Json$Encode$string('year');
		case 'NInterval':
			var tu = ni.a;
			var step = ni.b;
			return $elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2(
						'interval',
						$elm$json$Json$Encode$string(
							$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu))),
						_Utils_Tuple2(
						'step',
						$elm$json$Json$Encode$int(step))
					]));
		case 'NTrue':
			return $elm$json$Json$Encode$bool(true);
		case 'NFalse':
			return $elm$json$Json$Encode$bool(false);
		default:
			var n = ni.a;
			return $elm$json$Json$Encode$int(n);
	}
};
var $gicentre$elm_vegalite$VegaLite$schemeProperty = F2(
	function (schName, extent) {
		if (!extent.b) {
			return _Utils_Tuple2(
				'scheme',
				$elm$json$Json$Encode$string(schName));
		} else {
			if (!extent.b.b) {
				var n = extent.a;
				return _Utils_Tuple2(
					'scheme',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'name',
								$elm$json$Json$Encode$string(schName)),
								_Utils_Tuple2(
								'count',
								$elm$json$Json$Encode$float(n))
							])));
			} else {
				if (!extent.b.b.b) {
					var mn = extent.a;
					var _v1 = extent.b;
					var mx = _v1.a;
					return _Utils_Tuple2(
						'scheme',
						$elm$json$Json$Encode$object(
							_List_fromArray(
								[
									_Utils_Tuple2(
									'name',
									$elm$json$Json$Encode$string(schName)),
									_Utils_Tuple2(
									'extent',
									A2(
										$elm$json$Json$Encode$list,
										$elm$json$Json$Encode$float,
										_List_fromArray(
											[mn, mx])))
								])));
				} else {
					return _Utils_Tuple2(
						'scheme',
						$elm$json$Json$Encode$string(schName));
				}
			}
		}
	});
var $gicentre$elm_vegalite$VegaLite$scaleProperty = function (scaleProp) {
	switch (scaleProp.$) {
		case 'SType':
			var sType = scaleProp.a;
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$scaleLabel(sType)));
		case 'SDomain':
			var sdType = scaleProp.a;
			return _Utils_Tuple2(
				'domain',
				$gicentre$elm_vegalite$VegaLite$scaleDomainSpec(sdType));
		case 'SRange':
			var range = scaleProp.a;
			switch (range.$) {
				case 'RNumbers':
					var xs = range.a;
					return _Utils_Tuple2(
						'range',
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
				case 'RStrings':
					var ss = range.a;
					return _Utils_Tuple2(
						'range',
						A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss));
				default:
					var s = range.a;
					return _Utils_Tuple2(
						'range',
						$elm$json$Json$Encode$string(s));
			}
		case 'SScheme':
			var schName = scaleProp.a;
			var extent = scaleProp.b;
			return A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schName, extent);
		case 'SPadding':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'padding',
				$elm$json$Json$Encode$float(x));
		case 'SBase':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'base',
				$elm$json$Json$Encode$float(x));
		case 'SExponent':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'exponent',
				$elm$json$Json$Encode$float(x));
		case 'SConstant':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'constant',
				$elm$json$Json$Encode$float(x));
		case 'SPaddingInner':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'paddingInner',
				$elm$json$Json$Encode$float(x));
		case 'SPaddingOuter':
			var x = scaleProp.a;
			return _Utils_Tuple2(
				'paddingOuter',
				$elm$json$Json$Encode$float(x));
		case 'SRangeStep':
			var numOrNull = scaleProp.a;
			if (numOrNull.$ === 'Just') {
				var x = numOrNull.a;
				return _Utils_Tuple2(
					'rangeStep',
					$elm$json$Json$Encode$float(x));
			} else {
				return _Utils_Tuple2('rangeStep', $elm$json$Json$Encode$null);
			}
		case 'SRound':
			var b = scaleProp.a;
			return _Utils_Tuple2(
				'round',
				$elm$json$Json$Encode$bool(b));
		case 'SClamp':
			var b = scaleProp.a;
			return _Utils_Tuple2(
				'clamp',
				$elm$json$Json$Encode$bool(b));
		case 'SInterpolate':
			var interp = scaleProp.a;
			return _Utils_Tuple2(
				'interpolate',
				$gicentre$elm_vegalite$VegaLite$cInterpolateSpec(interp));
		case 'SNice':
			var ni = scaleProp.a;
			return _Utils_Tuple2(
				'nice',
				$gicentre$elm_vegalite$VegaLite$scaleNiceSpec(ni));
		case 'SZero':
			var b = scaleProp.a;
			return _Utils_Tuple2(
				'zero',
				$elm$json$Json$Encode$bool(b));
		default:
			var b = scaleProp.a;
			return _Utils_Tuple2(
				'reverse',
				$elm$json$Json$Encode$bool(b));
	}
};
var $gicentre$elm_vegalite$VegaLite$markChannelProperty = function (field) {
	switch (field.$) {
		case 'MName':
			var s = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(s))
				]);
		case 'MRepeat':
			var arr = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 'MmType':
			var t = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$measurementLabel(t)))
				]);
		case 'MScale':
			var sps = field.a;
			return _Utils_eq(sps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('scale', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'scale',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$scaleProperty, sps)))
				]);
		case 'MLegend':
			var lps = field.a;
			return _Utils_eq(lps, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('legend', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'legend',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$legendProperty, lps)))
				]);
		case 'MBin':
			var bps = field.a;
			return _List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$bin(bps)
				]);
		case 'MBinned':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'bin',
					$elm$json$Json$Encode$string('binned'))
				]);
		case 'MImpute':
			var ips = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'impute',
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$imputeProperty, ips)))
				]);
		case 'MSelectionCondition':
			var selName = field.a;
			var ifClause = field.b;
			var elseClause = field.c;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'selection',
								$gicentre$elm_vegalite$VegaLite$booleanOpSpec(selName)),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperty, ifClause)))),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperty, elseClause));
		case 'MDataCondition':
			var tests = field.a;
			var elseClause = field.b;
			var testClause = function (_v1) {
				var predicate = _v1.a;
				var ifClause = _v1.b;
				return $elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'test',
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec(predicate)),
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperty, ifClause)));
			};
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					A2($elm$json$Json$Encode$list, testClause, tests)),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperty, elseClause));
		case 'MTimeUnit':
			var tu = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'timeUnit',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu)))
				]);
		case 'MTitle':
			var t = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'title',
					$elm$json$Json$Encode$string(t))
				]);
		case 'MAggregate':
			var op = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'aggregate',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$operationLabel(op)))
				]);
		case 'MPath':
			var s = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$string(s))
				]);
		case 'MNumber':
			var x = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$float(x))
				]);
		case 'MString':
			var s = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$string(s))
				]);
		default:
			var b = field.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'value',
					$elm$json$Json$Encode$bool(b))
				]);
	}
};
var $gicentre$elm_vegalite$VegaLite$color = function (markProps) {
	return $elm$core$List$cons(
		_Utils_Tuple2(
			'color',
			$elm$json$Json$Encode$object(
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$markChannelProperty, markProps))));
};
var $gicentre$elm_vegalite$VegaLite$autosizeProperty = function (asCfg) {
	switch (asCfg.$) {
		case 'APad':
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('pad'));
		case 'AFit':
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('fit'));
		case 'ANone':
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string('none'));
		case 'AResize':
			return _Utils_Tuple2(
				'resize',
				$elm$json$Json$Encode$bool(true));
		case 'AContent':
			return _Utils_Tuple2(
				'contains',
				$elm$json$Json$Encode$string('content'));
		default:
			return _Utils_Tuple2(
				'contains',
				$elm$json$Json$Encode$string('padding'));
	}
};
var $gicentre$elm_vegalite$VegaLite$axisConfigProperty = function (axisCfg) {
	switch (axisCfg.$) {
		case 'BandPosition':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'bandPosition',
				$elm$json$Json$Encode$float(x));
		case 'Domain':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'domain',
				$elm$json$Json$Encode$bool(b));
		case 'DomainColor':
			var c = axisCfg.a;
			return _Utils_Tuple2(
				'domainColor',
				$elm$json$Json$Encode$string(c));
		case 'DomainOpacity':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'domainOpacity',
				$elm$json$Json$Encode$float(n));
		case 'DomainWidth':
			var w = axisCfg.a;
			return _Utils_Tuple2(
				'domainWidth',
				$elm$json$Json$Encode$float(w));
		case 'MaxExtent':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'maxExtent',
				$elm$json$Json$Encode$float(n));
		case 'MinExtent':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'minExtent',
				$elm$json$Json$Encode$float(n));
		case 'Grid':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'grid',
				$elm$json$Json$Encode$bool(b));
		case 'GridColor':
			var c = axisCfg.a;
			return _Utils_Tuple2(
				'gridColor',
				$elm$json$Json$Encode$string(c));
		case 'GridDash':
			var ds = axisCfg.a;
			return _Utils_Tuple2(
				'gridDash',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, ds));
		case 'GridOpacity':
			var o = axisCfg.a;
			return _Utils_Tuple2(
				'gridOpacity',
				$elm$json$Json$Encode$float(o));
		case 'GridWidth':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'gridWidth',
				$elm$json$Json$Encode$float(x));
		case 'Labels':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'labels',
				$elm$json$Json$Encode$bool(b));
		case 'LabelAlign':
			var ha = axisCfg.a;
			return _Utils_Tuple2(
				'labelAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'LabelAngle':
			var angle = axisCfg.a;
			return _Utils_Tuple2(
				'labelAngle',
				$elm$json$Json$Encode$float(angle));
		case 'LabelBaseline':
			var va = axisCfg.a;
			return _Utils_Tuple2(
				'labelBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'LabelBound':
			var mn = axisCfg.a;
			if (mn.$ === 'Just') {
				var n = mn.a;
				return (n === 1) ? _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$float(n));
			} else {
				return _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$bool(false));
			}
		case 'LabelColor':
			var c = axisCfg.a;
			return _Utils_Tuple2(
				'labelColor',
				$elm$json$Json$Encode$string(c));
		case 'LabelFlush':
			var mn = axisCfg.a;
			if (mn.$ === 'Just') {
				var n = mn.a;
				return (!n) ? _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$float(n));
			} else {
				return _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$bool(false));
			}
		case 'LabelFlushOffset':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'labelFlushOffset',
				$elm$json$Json$Encode$float(n));
		case 'LabelFont':
			var f = axisCfg.a;
			return _Utils_Tuple2(
				'labelFont',
				$elm$json$Json$Encode$string(f));
		case 'LabelFontSize':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'labelFontSize',
				$elm$json$Json$Encode$float(x));
		case 'LabelFontWeight':
			var fw = axisCfg.a;
			return _Utils_Tuple2(
				'labelFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw));
		case 'LabelLimit':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'labelLimit',
				$elm$json$Json$Encode$float(x));
		case 'LabelOpacity':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'labelOpacity',
				$elm$json$Json$Encode$float(n));
		case 'LabelOverlap':
			var strat = axisCfg.a;
			return _Utils_Tuple2(
				'labelOverlap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$overlapStrategyLabel(strat)));
		case 'LabelPadding':
			var pad = axisCfg.a;
			return _Utils_Tuple2(
				'labelPadding',
				$elm$json$Json$Encode$float(pad));
		case 'ShortTimeLabels':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'shortTimeLabels',
				$elm$json$Json$Encode$bool(b));
		case 'Ticks':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'ticks',
				$elm$json$Json$Encode$bool(b));
		case 'TickColor':
			var c = axisCfg.a;
			return _Utils_Tuple2(
				'tickColor',
				$elm$json$Json$Encode$string(c));
		case 'TickExtra':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'tickExtra',
				$elm$json$Json$Encode$bool(b));
		case 'TickOffset':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'tickOffset',
				$elm$json$Json$Encode$float(n));
		case 'TickOpacity':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'tickOpacity',
				$elm$json$Json$Encode$float(n));
		case 'TickMinStep':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'tickMinStep',
				$elm$json$Json$Encode$float(n));
		case 'TickRound':
			var b = axisCfg.a;
			return _Utils_Tuple2(
				'tickRound',
				$elm$json$Json$Encode$bool(b));
		case 'TickSize':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'tickSize',
				$elm$json$Json$Encode$float(x));
		case 'TickWidth':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'tickWidth',
				$elm$json$Json$Encode$float(x));
		case 'TitleAlign':
			var al = axisCfg.a;
			return _Utils_Tuple2(
				'titleAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(al)));
		case 'TitleAngle':
			var angle = axisCfg.a;
			return _Utils_Tuple2(
				'titleAngle',
				$elm$json$Json$Encode$float(angle));
		case 'TitleBaseline':
			var va = axisCfg.a;
			return _Utils_Tuple2(
				'titleBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'TitleColor':
			var c = axisCfg.a;
			return _Utils_Tuple2(
				'titleColor',
				$elm$json$Json$Encode$string(c));
		case 'TitleFont':
			var f = axisCfg.a;
			return _Utils_Tuple2(
				'titleFont',
				$elm$json$Json$Encode$string(f));
		case 'TitleFontWeight':
			var w = axisCfg.a;
			return _Utils_Tuple2(
				'titleFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w));
		case 'TitleFontSize':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'titleFontSize',
				$elm$json$Json$Encode$float(x));
		case 'TitleLimit':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'titleLimit',
				$elm$json$Json$Encode$float(x));
		case 'TitleOpacity':
			var n = axisCfg.a;
			return _Utils_Tuple2(
				'titleOpacity',
				$elm$json$Json$Encode$float(n));
		case 'TitlePadding':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'titlePadding',
				$elm$json$Json$Encode$float(x));
		case 'TitleX':
			var x = axisCfg.a;
			return _Utils_Tuple2(
				'titleX',
				$elm$json$Json$Encode$float(x));
		default:
			var y = axisCfg.a;
			return _Utils_Tuple2(
				'titleY',
				$elm$json$Json$Encode$float(y));
	}
};
var $gicentre$elm_vegalite$VegaLite$facetConfigProperty = function (fcp) {
	if (fcp.$ === 'FColumns') {
		var n = fcp.a;
		return _Utils_Tuple2(
			'columns',
			$elm$json$Json$Encode$int(n));
	} else {
		var x = fcp.a;
		return _Utils_Tuple2(
			'spacing',
			$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$fieldTitleLabel = function (ftp) {
	switch (ftp.$) {
		case 'FTVerbal':
			return 'verbal';
		case 'FTFunction':
			return 'function';
		default:
			return 'plain';
	}
};
var $gicentre$elm_vegalite$VegaLite$anchorLabel = function (an) {
	switch (an.$) {
		case 'AnStart':
			return 'start';
		case 'AnMiddle':
			return 'middle';
		default:
			return 'end';
	}
};
var $gicentre$elm_vegalite$VegaLite$sideLabel = function (side) {
	switch (side.$) {
		case 'STop':
			return 'top';
		case 'SBottom':
			return 'bottom';
		case 'SLeft':
			return 'left';
		default:
			return 'right';
	}
};
var $gicentre$elm_vegalite$VegaLite$headerProperty = function (hProp) {
	switch (hProp.$) {
		case 'HFormat':
			var fmt = hProp.a;
			return _Utils_Tuple2(
				'format',
				$elm$json$Json$Encode$string(fmt));
		case 'HFormatAsNum':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('number'));
		case 'HFormatAsTemporal':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('time'));
		case 'HLabelAlign':
			var ha = hProp.a;
			return _Utils_Tuple2(
				'labelAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'HLabelAnchor':
			var a = hProp.a;
			return _Utils_Tuple2(
				'labelAnchor',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$anchorLabel(a)));
		case 'HLabelAngle':
			var x = hProp.a;
			return _Utils_Tuple2(
				'labelAngle',
				$elm$json$Json$Encode$float(x));
		case 'HLabelColor':
			var s = hProp.a;
			return _Utils_Tuple2(
				'labelColor',
				$elm$json$Json$Encode$string(s));
		case 'HLabelFont':
			var s = hProp.a;
			return _Utils_Tuple2(
				'labelFont',
				$elm$json$Json$Encode$string(s));
		case 'HLabelFontSize':
			var x = hProp.a;
			return _Utils_Tuple2(
				'labelFontSize',
				$elm$json$Json$Encode$float(x));
		case 'HLabelLimit':
			var x = hProp.a;
			return _Utils_Tuple2(
				'labelLimit',
				$elm$json$Json$Encode$float(x));
		case 'HLabelOrient':
			var orient = hProp.a;
			return _Utils_Tuple2(
				'labelOrient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$sideLabel(orient)));
		case 'HLabelPadding':
			var x = hProp.a;
			return _Utils_Tuple2(
				'labelPadding',
				$elm$json$Json$Encode$float(x));
		case 'HTitle':
			var s = hProp.a;
			return _Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(s));
		case 'HTitleAnchor':
			var a = hProp.a;
			return _Utils_Tuple2(
				'titleAnchor',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$anchorLabel(a)));
		case 'HTitleAlign':
			var ha = hProp.a;
			return _Utils_Tuple2(
				'titleAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'HTitleAngle':
			var x = hProp.a;
			return _Utils_Tuple2(
				'titleAngle',
				$elm$json$Json$Encode$float(x));
		case 'HTitleBaseline':
			var va = hProp.a;
			return _Utils_Tuple2(
				'titleBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'HTitleColor':
			var s = hProp.a;
			return _Utils_Tuple2(
				'titleColor',
				$elm$json$Json$Encode$string(s));
		case 'HTitleFont':
			var s = hProp.a;
			return _Utils_Tuple2(
				'titleFont',
				$elm$json$Json$Encode$string(s));
		case 'HTitleFontWeight':
			var s = hProp.a;
			return _Utils_Tuple2(
				'titleFontWeight',
				$elm$json$Json$Encode$string(s));
		case 'HTitleFontSize':
			var x = hProp.a;
			return _Utils_Tuple2(
				'titleFontSize',
				$elm$json$Json$Encode$float(x));
		case 'HTitleLimit':
			var x = hProp.a;
			return _Utils_Tuple2(
				'titleLimit',
				$elm$json$Json$Encode$float(x));
		case 'HTitleOrient':
			var orient = hProp.a;
			return _Utils_Tuple2(
				'titleOrient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$sideLabel(orient)));
		default:
			var x = hProp.a;
			return _Utils_Tuple2(
				'titlePadding',
				$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$legendConfigProperty = function (legendConfig) {
	switch (legendConfig.$) {
		case 'LeClipHeight':
			var h = legendConfig.a;
			return _Utils_Tuple2(
				'clipHeight',
				$elm$json$Json$Encode$float(h));
		case 'LeColumnPadding':
			var n = legendConfig.a;
			return _Utils_Tuple2(
				'columnPadding',
				$elm$json$Json$Encode$float(n));
		case 'LeRowPadding':
			var n = legendConfig.a;
			return _Utils_Tuple2(
				'rowPadding',
				$elm$json$Json$Encode$float(n));
		case 'LeColumns':
			var n = legendConfig.a;
			return _Utils_Tuple2(
				'columns',
				$elm$json$Json$Encode$float(n));
		case 'CornerRadius':
			var r = legendConfig.a;
			return _Utils_Tuple2(
				'cornerRadius',
				$elm$json$Json$Encode$float(r));
		case 'FillColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'fillColor',
				$elm$json$Json$Encode$string(s));
		case 'Orient':
			var orient = legendConfig.a;
			return _Utils_Tuple2(
				'orient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$legendOrientLabel(orient)));
		case 'Offset':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$float(x));
		case 'StrokeColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'strokeColor',
				$elm$json$Json$Encode$string(s));
		case 'LeStrokeDash':
			var xs = legendConfig.a;
			return _Utils_Tuple2(
				'strokeDash',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		case 'LeStrokeWidth':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'strokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'LePadding':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'padding',
				$elm$json$Json$Encode$float(x));
		case 'GradientDirection':
			var d = legendConfig.a;
			return _Utils_Tuple2(
				'gradientDirection',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)));
		case 'GradientLabelBaseline':
			var va = legendConfig.a;
			return _Utils_Tuple2(
				'gradientLabelBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'GradientLabelLimit':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'gradientLabelLimit',
				$elm$json$Json$Encode$float(x));
		case 'GradientLabelOffset':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'gradientLabelOffset',
				$elm$json$Json$Encode$float(x));
		case 'GradientStrokeColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'gradientStrokeColor',
				$elm$json$Json$Encode$string(s));
		case 'GradientStrokeWidth':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'gradientStrokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'GradientHeight':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'gradientHeight',
				$elm$json$Json$Encode$float(x));
		case 'GradientWidth':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'gradientWidth',
				$elm$json$Json$Encode$float(x));
		case 'LeGridAlign':
			var ga = legendConfig.a;
			return _Utils_Tuple2(
				'gridAlign',
				$gicentre$elm_vegalite$VegaLite$compositionAlignmentSpec(ga));
		case 'LeLabelAlign':
			var ha = legendConfig.a;
			return _Utils_Tuple2(
				'labelAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'LeLabelBaseline':
			var va = legendConfig.a;
			return _Utils_Tuple2(
				'labelBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'LeLabelColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'labelColor',
				$elm$json$Json$Encode$string(s));
		case 'LeLabelFont':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'labelFont',
				$elm$json$Json$Encode$string(s));
		case 'LeLabelFontSize':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'labelFontSize',
				$elm$json$Json$Encode$float(x));
		case 'LeLabelLimit':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'labelLimit',
				$elm$json$Json$Encode$float(x));
		case 'LeLabelOffset':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'labelOffset',
				$elm$json$Json$Encode$float(x));
		case 'LeLabelOverlap':
			var lo = legendConfig.a;
			return _Utils_Tuple2(
				'labelOverlap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$overlapStrategyLabel(lo)));
		case 'LeShortTimeLabels':
			var b = legendConfig.a;
			return _Utils_Tuple2(
				'shortTimeLabels',
				$elm$json$Json$Encode$bool(b));
		case 'EntryPadding':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'entryPadding',
				$elm$json$Json$Encode$float(x));
		case 'SymbolDirection':
			var d = legendConfig.a;
			return _Utils_Tuple2(
				'symbolDirection',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$markOrientationLabel(d)));
		case 'SymbolFillColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'symbolFillColor',
				$elm$json$Json$Encode$string(s));
		case 'SymbolBaseFillColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'symbolBaseFillColor',
				$elm$json$Json$Encode$string(s));
		case 'SymbolStrokeColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'symbolStrokeColor',
				$elm$json$Json$Encode$string(s));
		case 'SymbolBaseStrokeColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'symbolBaseStrokeColor',
				$elm$json$Json$Encode$string(s));
		case 'SymbolOffset':
			var o = legendConfig.a;
			return _Utils_Tuple2(
				'symbolOffset',
				$elm$json$Json$Encode$float(o));
		case 'SymbolType':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'symbolType',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$symbolLabel(s)));
		case 'SymbolSize':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'symbolSize',
				$elm$json$Json$Encode$float(x));
		case 'SymbolStrokeWidth':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'symbolStrokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'LeTitleAlign':
			var ha = legendConfig.a;
			return _Utils_Tuple2(
				'titleAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'LeTitleBaseline':
			var va = legendConfig.a;
			return _Utils_Tuple2(
				'titleBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'LeTitleColor':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'titleColor',
				$elm$json$Json$Encode$string(s));
		case 'LeTitleFont':
			var s = legendConfig.a;
			return _Utils_Tuple2(
				'titleFont',
				$elm$json$Json$Encode$string(s));
		case 'LeTitleFontSize':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'titleFontSize',
				$elm$json$Json$Encode$float(x));
		case 'LeTitleFontWeight':
			var fw = legendConfig.a;
			return _Utils_Tuple2(
				'titleFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw));
		case 'LeTitleLimit':
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'titleLimit',
				$elm$json$Json$Encode$float(x));
		default:
			var x = legendConfig.a;
			return _Utils_Tuple2(
				'titlePadding',
				$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$paddingSpec = function (pad) {
	if (pad.$ === 'PSize') {
		var p = pad.a;
		return $elm$json$Json$Encode$float(p);
	} else {
		var l = pad.a;
		var t = pad.b;
		var r = pad.c;
		var b = pad.d;
		return $elm$json$Json$Encode$object(
			_List_fromArray(
				[
					_Utils_Tuple2(
					'left',
					$elm$json$Json$Encode$float(l)),
					_Utils_Tuple2(
					'top',
					$elm$json$Json$Encode$float(t)),
					_Utils_Tuple2(
					'right',
					$elm$json$Json$Encode$float(r)),
					_Utils_Tuple2(
					'bottom',
					$elm$json$Json$Encode$float(b))
				]));
	}
};
var $gicentre$elm_vegalite$VegaLite$projectionLabel = function (proj) {
	switch (proj.$) {
		case 'Albers':
			return 'albers';
		case 'AlbersUsa':
			return 'albersUsa';
		case 'AzimuthalEqualArea':
			return 'azimuthalEqualArea';
		case 'AzimuthalEquidistant':
			return 'azimuthalEquidistant';
		case 'ConicConformal':
			return 'conicConformal';
		case 'ConicEqualArea':
			return 'conicEqualarea';
		case 'ConicEquidistant':
			return 'conicEquidistant';
		case 'Custom':
			var projName = proj.a;
			return projName;
		case 'Equirectangular':
			return 'equirectangular';
		case 'Gnomonic':
			return 'gnomonic';
		case 'Identity':
			return 'identity';
		case 'Mercator':
			return 'mercator';
		case 'Orthographic':
			return 'orthographic';
		case 'Stereographic':
			return 'stereographic';
		default:
			return 'transverseMercator';
	}
};
var $gicentre$elm_vegalite$VegaLite$projectionProperty = function (pp) {
	switch (pp.$) {
		case 'PType':
			var proj = pp.a;
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$projectionLabel(proj)));
		case 'PClipAngle':
			var numOrNull = pp.a;
			if (numOrNull.$ === 'Just') {
				var x = numOrNull.a;
				return _Utils_Tuple2(
					'clipAngle',
					$elm$json$Json$Encode$float(x));
			} else {
				return _Utils_Tuple2('clipAngle', $elm$json$Json$Encode$null);
			}
		case 'PClipExtent':
			var rClip = pp.a;
			if (rClip.$ === 'NoClip') {
				return _Utils_Tuple2('clipExtent', $elm$json$Json$Encode$null);
			} else {
				var l = rClip.a;
				var t = rClip.b;
				var r = rClip.c;
				var b = rClip.d;
				return _Utils_Tuple2(
					'clipExtent',
					A2(
						$elm$json$Json$Encode$list,
						$elm$json$Json$Encode$float,
						_List_fromArray(
							[l, t, r, b])));
			}
		case 'PReflectX':
			var b = pp.a;
			return _Utils_Tuple2(
				'reflectX',
				$elm$json$Json$Encode$bool(b));
		case 'PReflectY':
			var b = pp.a;
			return _Utils_Tuple2(
				'reflectY',
				$elm$json$Json$Encode$bool(b));
		case 'PCenter':
			var lon = pp.a;
			var lat = pp.b;
			return _Utils_Tuple2(
				'center',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$float,
					_List_fromArray(
						[lon, lat])));
		case 'PrScale':
			var sc = pp.a;
			return _Utils_Tuple2(
				'scale',
				$elm$json$Json$Encode$float(sc));
		case 'PrTranslate':
			var tx = pp.a;
			var ty = pp.b;
			return _Utils_Tuple2(
				'translate',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$float,
					_List_fromArray(
						[tx, ty])));
		case 'PrRotate':
			var lambda = pp.a;
			var phi = pp.b;
			var gamma = pp.c;
			return _Utils_Tuple2(
				'rotate',
				A2(
					$elm$json$Json$Encode$list,
					$elm$json$Json$Encode$float,
					_List_fromArray(
						[lambda, phi, gamma])));
		case 'PPrecision':
			var pr = pp.a;
			return _Utils_Tuple2(
				'precision',
				$elm$json$Json$Encode$float(pr));
		case 'PCoefficient':
			var x = pp.a;
			return _Utils_Tuple2(
				'coefficient',
				$elm$json$Json$Encode$float(x));
		case 'PDistance':
			var x = pp.a;
			return _Utils_Tuple2(
				'distance',
				$elm$json$Json$Encode$float(x));
		case 'PFraction':
			var x = pp.a;
			return _Utils_Tuple2(
				'fraction',
				$elm$json$Json$Encode$float(x));
		case 'PLobes':
			var n = pp.a;
			return _Utils_Tuple2(
				'lobes',
				$elm$json$Json$Encode$int(n));
		case 'PParallel':
			var x = pp.a;
			return _Utils_Tuple2(
				'parallel',
				$elm$json$Json$Encode$float(x));
		case 'PRadius':
			var x = pp.a;
			return _Utils_Tuple2(
				'radius',
				$elm$json$Json$Encode$float(x));
		case 'PRatio':
			var x = pp.a;
			return _Utils_Tuple2(
				'ratio',
				$elm$json$Json$Encode$float(x));
		case 'PSpacing':
			var x = pp.a;
			return _Utils_Tuple2(
				'spacing',
				$elm$json$Json$Encode$float(x));
		default:
			var x = pp.a;
			return _Utils_Tuple2(
				'tilt',
				$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$rangeConfigProperty = function (rangeCfg) {
	switch (rangeCfg.$) {
		case 'RCategory':
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'category',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
		case 'RDiverging':
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'diverging',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
		case 'RHeatmap':
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'heatmap',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
		case 'ROrdinal':
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'ordinal',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
		case 'RRamp':
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'ramp',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
		default:
			var schemeName = rangeCfg.a;
			return _Utils_Tuple2(
				'symbol',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							A2($gicentre$elm_vegalite$VegaLite$schemeProperty, schemeName, _List_Nil)
						])));
	}
};
var $gicentre$elm_vegalite$VegaLite$scaleConfigProperty = function (scaleCfg) {
	switch (scaleCfg.$) {
		case 'SCBandPaddingInner':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'bandPaddingInner',
				$elm$json$Json$Encode$float(x));
		case 'SCBandPaddingOuter':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'bandPaddingOuter',
				$elm$json$Json$Encode$float(x));
		case 'SCBarBandPaddingInner':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'barBandPaddingInner',
				$elm$json$Json$Encode$float(x));
		case 'SCBarBandPaddingOuter':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'barBandPaddingOuter',
				$elm$json$Json$Encode$float(x));
		case 'SCRectBandPaddingInner':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'rectBandPaddingInner',
				$elm$json$Json$Encode$float(x));
		case 'SCRectBandPaddingOuter':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'rectBandPaddingOuter',
				$elm$json$Json$Encode$float(x));
		case 'SCClamp':
			var b = scaleCfg.a;
			return _Utils_Tuple2(
				'clamp',
				$elm$json$Json$Encode$bool(b));
		case 'SCMaxBandSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'maxBandSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMinBandSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'minBandSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMaxFontSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'maxFontSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMinFontSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'minFontSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMaxOpacity':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'maxOpacity',
				$elm$json$Json$Encode$float(x));
		case 'SCMinOpacity':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'minOpacity',
				$elm$json$Json$Encode$float(x));
		case 'SCMaxSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'maxSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMinSize':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'minSize',
				$elm$json$Json$Encode$float(x));
		case 'SCMaxStrokeWidth':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'maxStrokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'SCMinStrokeWidth':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'minStrokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'SCPointPadding':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'pointPadding',
				$elm$json$Json$Encode$float(x));
		case 'SCRangeStep':
			var numOrNull = scaleCfg.a;
			if (numOrNull.$ === 'Just') {
				var x = numOrNull.a;
				return _Utils_Tuple2(
					'rangeStep',
					$elm$json$Json$Encode$float(x));
			} else {
				return _Utils_Tuple2('rangeStep', $elm$json$Json$Encode$null);
			}
		case 'SCRound':
			var b = scaleCfg.a;
			return _Utils_Tuple2(
				'round',
				$elm$json$Json$Encode$bool(b));
		case 'SCTextXRangeStep':
			var x = scaleCfg.a;
			return _Utils_Tuple2(
				'textXRangeStep',
				$elm$json$Json$Encode$float(x));
		default:
			var b = scaleCfg.a;
			return _Utils_Tuple2(
				'useUnaggregatedDomain',
				$elm$json$Json$Encode$bool(b));
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionLabel = function (seType) {
	switch (seType.$) {
		case 'SeSingle':
			return 'single';
		case 'SeMulti':
			return 'multi';
		default:
			return 'interval';
	}
};
var $gicentre$elm_vegalite$VegaLite$inputProperty = function (prop) {
	switch (prop.$) {
		case 'InMin':
			var x = prop.a;
			return _Utils_Tuple2(
				'min',
				$elm$json$Json$Encode$float(x));
		case 'InMax':
			var x = prop.a;
			return _Utils_Tuple2(
				'max',
				$elm$json$Json$Encode$float(x));
		case 'InStep':
			var x = prop.a;
			return _Utils_Tuple2(
				'step',
				$elm$json$Json$Encode$float(x));
		case 'Debounce':
			var x = prop.a;
			return _Utils_Tuple2(
				'debounce',
				$elm$json$Json$Encode$float(x));
		case 'InName':
			var s = prop.a;
			return _Utils_Tuple2(
				'name',
				$elm$json$Json$Encode$string(s));
		case 'InOptions':
			var opts = prop.a;
			return _Utils_Tuple2(
				'options',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, opts));
		case 'InPlaceholder':
			var el = prop.a;
			return _Utils_Tuple2(
				'placeholder',
				$elm$json$Json$Encode$string(el));
		default:
			var el = prop.a;
			return _Utils_Tuple2(
				'element',
				$elm$json$Json$Encode$string(el));
	}
};
var $gicentre$elm_vegalite$VegaLite$bindingSpec = function (bnd) {
	switch (bnd.$) {
		case 'IRange':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('range')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'ICheckbox':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('checkbox')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IRadio':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('radio')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'ISelect':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('select')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IText':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('text')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'INumber':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('number')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IDate':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('date')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'ITime':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('time')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IMonth':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('month')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IWeek':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('week')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'IDateTimeLocal':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('datetimelocal')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		case 'ITel':
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('tel')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
		default:
			var label = bnd.a;
			var props = bnd.b;
			return _Utils_Tuple2(
				label,
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'input',
							$elm$json$Json$Encode$string('color')),
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$inputProperty, props))));
	}
};
var $gicentre$elm_vegalite$VegaLite$channelLabel = function (ch) {
	switch (ch.$) {
		case 'ChX':
			return 'x';
		case 'ChY':
			return 'y';
		case 'ChX2':
			return 'x2';
		case 'ChY2':
			return 'y2';
		case 'ChColor':
			return 'color';
		case 'ChOpacity':
			return 'opacity';
		case 'ChShape':
			return 'shape';
		default:
			return 'size';
	}
};
var $elm$core$Tuple$mapSecond = F2(
	function (func, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			x,
			func(y));
	});
var $gicentre$elm_vegalite$VegaLite$selectionMarkProperty = function (markProp) {
	switch (markProp.$) {
		case 'SMFill':
			var colour = markProp.a;
			return _Utils_Tuple2(
				'fill',
				$elm$json$Json$Encode$string(colour));
		case 'SMFillOpacity':
			var x = markProp.a;
			return _Utils_Tuple2(
				'fillOpacity',
				$elm$json$Json$Encode$float(x));
		case 'SMStroke':
			var colour = markProp.a;
			return _Utils_Tuple2(
				'stroke',
				$elm$json$Json$Encode$string(colour));
		case 'SMStrokeOpacity':
			var x = markProp.a;
			return _Utils_Tuple2(
				'strokeOpacity',
				$elm$json$Json$Encode$float(x));
		case 'SMStrokeWidth':
			var x = markProp.a;
			return _Utils_Tuple2(
				'strokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'SMStrokeDash':
			var xs = markProp.a;
			return _Utils_Tuple2(
				'strokeDash',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		default:
			var x = markProp.a;
			return _Utils_Tuple2(
				'strokeDashOffset',
				$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$selectionResolutionLabel = function (res) {
	switch (res.$) {
		case 'SeGlobal':
			return 'global';
		case 'SeUnion':
			return 'union';
		default:
			return 'intersect';
	}
};
var $elm$core$String$trim = _String_trim;
var $gicentre$elm_vegalite$VegaLite$selectionProperty = function (selProp) {
	switch (selProp.$) {
		case 'Fields':
			var fNames = selProp.a;
			return _Utils_Tuple2(
				'fields',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, fNames));
		case 'SInit':
			var iVals = selProp.a;
			return _Utils_Tuple2(
				'init',
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$map,
						$elm$core$Tuple$mapSecond($gicentre$elm_vegalite$VegaLite$dataValueSpec),
						iVals)));
		case 'Encodings':
			var channels = selProp.a;
			return _Utils_Tuple2(
				'encodings',
				A2(
					$elm$json$Json$Encode$list,
					A2($elm$core$Basics$composeL, $elm$json$Json$Encode$string, $gicentre$elm_vegalite$VegaLite$channelLabel),
					channels));
		case 'On':
			var evStr = selProp.a;
			return _Utils_Tuple2(
				'on',
				$elm$json$Json$Encode$string(evStr));
		case 'Clear':
			var evStr = selProp.a;
			var _v1 = $elm$core$String$trim(evStr);
			if (_v1 === '') {
				return _Utils_Tuple2(
					'clear',
					$elm$json$Json$Encode$bool(false));
			} else {
				var evStrTrimmed = _v1;
				return _Utils_Tuple2(
					'clear',
					$elm$json$Json$Encode$string(evStrTrimmed));
			}
		case 'Empty':
			return _Utils_Tuple2(
				'empty',
				$elm$json$Json$Encode$string('none'));
		case 'ResolveSelections':
			var res = selProp.a;
			return _Utils_Tuple2(
				'resolve',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$selectionResolutionLabel(res)));
		case 'SelectionMark':
			var markProps = selProp.a;
			return _Utils_Tuple2(
				'mark',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$selectionMarkProperty, markProps)));
		case 'BindScales':
			return _Utils_Tuple2(
				'bind',
				$elm$json$Json$Encode$string('scales'));
		case 'Bind':
			var binds = selProp.a;
			return _Utils_Tuple2(
				'bind',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$bindingSpec, binds)));
		case 'Nearest':
			var b = selProp.a;
			return _Utils_Tuple2(
				'nearest',
				$elm$json$Json$Encode$bool(b));
		case 'Toggle':
			var ex = selProp.a;
			return _Utils_Tuple2(
				'toggle',
				$elm$json$Json$Encode$string(ex));
		case 'Translate':
			var e = selProp.a;
			return (e === '') ? _Utils_Tuple2(
				'translate',
				$elm$json$Json$Encode$bool(false)) : _Utils_Tuple2(
				'translate',
				$elm$json$Json$Encode$string(e));
		default:
			var e = selProp.a;
			return (e === '') ? _Utils_Tuple2(
				'zoom',
				$elm$json$Json$Encode$bool(false)) : _Utils_Tuple2(
				'zoom',
				$elm$json$Json$Encode$string(e));
	}
};
var $gicentre$elm_vegalite$VegaLite$stackOffsetSpec = function (sp) {
	switch (sp.$) {
		case 'OfZero':
			return $elm$json$Json$Encode$string('zero');
		case 'OfNormalize':
			return $elm$json$Json$Encode$string('normalize');
		case 'OfCenter':
			return $elm$json$Json$Encode$string('center');
		default:
			return $elm$json$Json$Encode$null;
	}
};
var $gicentre$elm_vegalite$VegaLite$stackOffset = function (offset) {
	return _Utils_Tuple2(
		'stack',
		$gicentre$elm_vegalite$VegaLite$stackOffsetSpec(offset));
};
var $gicentre$elm_vegalite$VegaLite$titleConfigSpec = function (titleCfg) {
	switch (titleCfg.$) {
		case 'TAnchor':
			var an = titleCfg.a;
			return _Utils_Tuple2(
				'anchor',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$anchorLabel(an)));
		case 'TAngle':
			var x = titleCfg.a;
			return _Utils_Tuple2(
				'angle',
				$elm$json$Json$Encode$float(x));
		case 'TBaseline':
			var va = titleCfg.a;
			return _Utils_Tuple2(
				'baseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'TColor':
			var clr = titleCfg.a;
			return _Utils_Tuple2(
				'color',
				$elm$json$Json$Encode$string(clr));
		case 'TFont':
			var fnt = titleCfg.a;
			return _Utils_Tuple2(
				'font',
				$elm$json$Json$Encode$string(fnt));
		case 'TFontSize':
			var x = titleCfg.a;
			return _Utils_Tuple2(
				'fontSize',
				$elm$json$Json$Encode$float(x));
		case 'TFontWeight':
			var w = titleCfg.a;
			return _Utils_Tuple2(
				'fontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(w));
		case 'TLimit':
			var x = titleCfg.a;
			return _Utils_Tuple2(
				'limit',
				$elm$json$Json$Encode$float(x));
		case 'TOffset':
			var x = titleCfg.a;
			return _Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$float(x));
		default:
			var sd = titleCfg.a;
			return _Utils_Tuple2(
				'orient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$sideLabel(sd)));
	}
};
var $gicentre$elm_vegalite$VegaLite$viewConfigProperty = function (viewCfg) {
	switch (viewCfg.$) {
		case 'VWidth':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'width',
				$elm$json$Json$Encode$float(x));
		case 'VHeight':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'height',
				$elm$json$Json$Encode$float(x));
		case 'VClip':
			var b = viewCfg.a;
			return _Utils_Tuple2(
				'clip',
				$elm$json$Json$Encode$bool(b));
		case 'VCornerRadius':
			var r = viewCfg.a;
			return _Utils_Tuple2(
				'cornerRadius',
				$elm$json$Json$Encode$float(r));
		case 'VFill':
			var ms = viewCfg.a;
			if (ms.$ === 'Just') {
				var s = ms.a;
				return _Utils_Tuple2(
					'fill',
					$elm$json$Json$Encode$string(s));
			} else {
				return _Utils_Tuple2(
					'fill',
					$elm$json$Json$Encode$string(''));
			}
		case 'VFillOpacity':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'fillOpacity',
				$elm$json$Json$Encode$float(x));
		case 'VOpacity':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'opacity',
				$elm$json$Json$Encode$float(x));
		case 'VStroke':
			var ms = viewCfg.a;
			if (ms.$ === 'Just') {
				var s = ms.a;
				return _Utils_Tuple2(
					'stroke',
					$elm$json$Json$Encode$string(s));
			} else {
				return _Utils_Tuple2(
					'stroke',
					$elm$json$Json$Encode$string(''));
			}
		case 'VStrokeOpacity':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'strokeOpacity',
				$elm$json$Json$Encode$float(x));
		case 'VStrokeCap':
			var cap = viewCfg.a;
			return _Utils_Tuple2(
				'strokeCap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$strokeCapLabel(cap)));
		case 'VStrokeJoin':
			var jn = viewCfg.a;
			return _Utils_Tuple2(
				'strokeJoin',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$strokeJoinLabel(jn)));
		case 'VStrokeWidth':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'strokeWidth',
				$elm$json$Json$Encode$float(x));
		case 'VStrokeDash':
			var xs = viewCfg.a;
			return _Utils_Tuple2(
				'strokeDash',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs));
		case 'VStrokeDashOffset':
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'strokeDashOffset',
				$elm$json$Json$Encode$float(x));
		default:
			var x = viewCfg.a;
			return _Utils_Tuple2(
				'strokeMiterLimit',
				$elm$json$Json$Encode$float(x));
	}
};
var $gicentre$elm_vegalite$VegaLite$configProperty = function (configProp) {
	switch (configProp.$) {
		case 'Autosize':
			var aus = configProp.a;
			return _Utils_Tuple2(
				'autosize',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$autosizeProperty, aus)));
		case 'Background':
			var bg = configProp.a;
			return _Utils_Tuple2(
				'background',
				$elm$json$Json$Encode$string(bg));
		case 'CountTitle':
			var s = configProp.a;
			return _Utils_Tuple2(
				'countTitle',
				$elm$json$Json$Encode$string(s));
		case 'FieldTitle':
			var ftp = configProp.a;
			return _Utils_Tuple2(
				'fieldTitle',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$fieldTitleLabel(ftp)));
		case 'RemoveInvalid':
			var b = configProp.a;
			return b ? _Utils_Tuple2(
				'invalidValues',
				$elm$json$Json$Encode$string('filter')) : _Utils_Tuple2('invalidValues', $elm$json$Json$Encode$null);
		case 'NumberFormat':
			var fmt = configProp.a;
			return _Utils_Tuple2(
				'numberFormat',
				$elm$json$Json$Encode$string(fmt));
		case 'Padding':
			var pad = configProp.a;
			return _Utils_Tuple2(
				'padding',
				$gicentre$elm_vegalite$VegaLite$paddingSpec(pad));
		case 'TimeFormat':
			var fmt = configProp.a;
			return _Utils_Tuple2(
				'timeFormat',
				$elm$json$Json$Encode$string(fmt));
		case 'Axis':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axis',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisX':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisX',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisY':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisY',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisLeft':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisLeft',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisRight':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisRight',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisTop':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisTop',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisBottom':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisBottom',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'AxisBand':
			var acs = configProp.a;
			return _Utils_Tuple2(
				'axisBand',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisConfigProperty, acs)));
		case 'Legend':
			var lcs = configProp.a;
			return _Utils_Tuple2(
				'legend',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$legendConfigProperty, lcs)));
		case 'MarkStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'mark',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'Projection':
			var pps = configProp.a;
			return _Utils_Tuple2(
				'projection',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$projectionProperty, pps)));
		case 'AreaStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'area',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'BarStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'bar',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'CircleStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'circle',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'FacetStyle':
			var fps = configProp.a;
			return _Utils_Tuple2(
				'facet',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$facetConfigProperty, fps)));
		case 'GeoshapeStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'geoshape',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'HeaderStyle':
			var hps = configProp.a;
			return _Utils_Tuple2(
				'header',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$headerProperty, hps)));
		case 'LineStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'line',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'PointStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'point',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'RectStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'rect',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'RuleStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'rule',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'SquareStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'square',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'TextStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'text',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'TickStyle':
			var mps = configProp.a;
			return _Utils_Tuple2(
				'tick',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
		case 'TitleStyle':
			var tcs = configProp.a;
			return _Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$titleConfigSpec, tcs)));
		case 'NamedStyle':
			var styleName = configProp.a;
			var mps = configProp.b;
			return _Utils_Tuple2(
				'style',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							styleName,
							$elm$json$Json$Encode$object(
								A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)))
						])));
		case 'NamedStyles':
			var styles = configProp.a;
			return _Utils_Tuple2(
				'style',
				$elm$json$Json$Encode$object(
					A2(
						$elm$core$List$map,
						function (_v1) {
							var sName = _v1.a;
							var mps = _v1.b;
							return _Utils_Tuple2(
								sName,
								$elm$json$Json$Encode$object(
									A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
						},
						styles)));
		case 'Scale':
			var scs = configProp.a;
			return _Utils_Tuple2(
				'scale',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$scaleConfigProperty, scs)));
		case 'Stack':
			var so = configProp.a;
			return $gicentre$elm_vegalite$VegaLite$stackOffset(so);
		case 'Range':
			var rcs = configProp.a;
			return _Utils_Tuple2(
				'range',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$rangeConfigProperty, rcs)));
		case 'SelectionStyle':
			var selConfig = configProp.a;
			var selProp = function (_v2) {
				var sel = _v2.a;
				var sps = _v2.b;
				return _Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$selectionLabel(sel),
					$elm$json$Json$Encode$object(
						A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$selectionProperty, sps)));
			};
			return _Utils_Tuple2(
				'selection',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, selProp, selConfig)));
		case 'View':
			var vcs = configProp.a;
			return _Utils_Tuple2(
				'view',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$viewConfigProperty, vcs)));
		default:
			var mps = configProp.a;
			return _Utils_Tuple2(
				'trail',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$markProperty, mps)));
	}
};
var $gicentre$elm_vegalite$VegaLite$configuration = function (cfg) {
	return $elm$core$List$cons(
		$gicentre$elm_vegalite$VegaLite$configProperty(cfg));
};
var $gicentre$elm_vegalite$VegaLite$VLConfig = {$: 'VLConfig'};
var $gicentre$elm_vegalite$VegaLite$configure = function (configs) {
	return _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLConfig,
		$elm$json$Json$Encode$object(configs));
};
var $gicentre$elm_vegalite$VegaLite$dataColumn = F2(
	function (colName, data) {
		switch (data.$) {
			case 'Numbers':
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (x) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$float(x));
						},
						col));
			case 'Strings':
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (s) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$string(s));
						},
						col));
			case 'DateTimes':
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (ds) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$object(
									A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, ds)));
						},
						col));
			default:
				var col = data.a;
				return $elm$core$List$cons(
					A2(
						$elm$core$List$map,
						function (b) {
							return _Utils_Tuple2(
								colName,
								$elm$json$Json$Encode$bool(b));
						},
						col));
		}
	});
var $gicentre$elm_vegalite$VegaLite$VLData = {$: 'VLData'};
var $gicentre$elm_vegalite$VegaLite$dataTypeSpec = function (dType) {
	switch (dType.$) {
		case 'FoNum':
			return $elm$json$Json$Encode$string('number');
		case 'FoBoo':
			return $elm$json$Json$Encode$string('boolean');
		case 'FoDate':
			var dateFmt = dType.a;
			return (dateFmt === '') ? $elm$json$Json$Encode$string('date') : $elm$json$Json$Encode$string('date:\'' + (dateFmt + '\''));
		default:
			var dateFmt = dType.a;
			return (dateFmt === '') ? $elm$json$Json$Encode$string('utc') : $elm$json$Json$Encode$string('utc:\'' + (dateFmt + '\''));
	}
};
var $elm$core$String$cons = _String_cons;
var $elm$core$String$fromChar = function (_char) {
	return A2($elm$core$String$cons, _char, '');
};
var $gicentre$elm_vegalite$VegaLite$formatProperty = function (fmt) {
	switch (fmt.$) {
		case 'JSON':
			var propertyName = fmt.a;
			return ($elm$core$String$trim(propertyName) === '') ? _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('json'))
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('json')),
					_Utils_Tuple2(
					'property',
					$elm$json$Json$Encode$string(propertyName))
				]);
		case 'CSV':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('csv'))
				]);
		case 'TSV':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('tsv'))
				]);
		case 'DSV':
			var delim = fmt.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('dsv')),
					_Utils_Tuple2(
					'delimiter',
					$elm$json$Json$Encode$string(
						$elm$core$String$fromChar(delim)))
				]);
		case 'Arrow':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('arrow'))
				]);
		case 'TopojsonFeature':
			var objectSet = fmt.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('topojson')),
					_Utils_Tuple2(
					'feature',
					$elm$json$Json$Encode$string(objectSet))
				]);
		case 'TopojsonMesh':
			var objectSet = fmt.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string('topojson')),
					_Utils_Tuple2(
					'mesh',
					$elm$json$Json$Encode$string(objectSet))
				]);
		default:
			var fmts = fmt.a;
			return _Utils_eq(fmts, _List_Nil) ? _List_fromArray(
				[
					_Utils_Tuple2('parse', $elm$json$Json$Encode$null)
				]) : _List_fromArray(
				[
					_Utils_Tuple2(
					'parse',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$map,
							function (_v1) {
								var field = _v1.a;
								var fFormat = _v1.b;
								return _Utils_Tuple2(
									field,
									$gicentre$elm_vegalite$VegaLite$dataTypeSpec(fFormat));
							},
							fmts)))
				]);
	}
};
var $elm$core$Basics$composeR = F3(
	function (f, g, x) {
		return g(
			f(x));
	});
var $elm$core$List$head = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(x);
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$repeatHelp = F3(
	function (result, n, value) {
		repeatHelp:
		while (true) {
			if (n <= 0) {
				return result;
			} else {
				var $temp$result = A2($elm$core$List$cons, value, result),
					$temp$n = n - 1,
					$temp$value = value;
				result = $temp$result;
				n = $temp$n;
				value = $temp$value;
				continue repeatHelp;
			}
		}
	});
var $elm$core$List$repeat = F2(
	function (n, value) {
		return A3($elm$core$List$repeatHelp, _List_Nil, n, value);
	});
var $elm$core$Maybe$withDefault = F2(
	function (_default, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return value;
		} else {
			return _default;
		}
	});
var $gicentre$elm_vegalite$VegaLite$transpose = function (xss) {
	var numCols = A2(
		$elm$core$Basics$composeR,
		$elm$core$List$head,
		A2(
			$elm$core$Basics$composeR,
			$elm$core$Maybe$withDefault(_List_Nil),
			$elm$core$List$length));
	return A3(
		$elm$core$List$foldr,
		$elm$core$List$map2($elm$core$List$cons),
		A2(
			$elm$core$List$repeat,
			numCols(xss),
			_List_Nil),
		xss);
};
var $gicentre$elm_vegalite$VegaLite$dataFromColumns = F2(
	function (fmts, cols) {
		var dataArray = A2(
			$elm$json$Json$Encode$list,
			$elm$json$Json$Encode$object,
			$gicentre$elm_vegalite$VegaLite$transpose(cols));
		return _Utils_eq(fmts, _List_Nil) ? _Utils_Tuple2(
			$gicentre$elm_vegalite$VegaLite$VLData,
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2('values', dataArray)
					]))) : _Utils_Tuple2(
			$gicentre$elm_vegalite$VegaLite$VLData,
			$elm$json$Json$Encode$object(
				_List_fromArray(
					[
						_Utils_Tuple2('values', dataArray),
						_Utils_Tuple2(
						'format',
						$elm$json$Json$Encode$object(
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$formatProperty, fmts)))
					])));
	});
var $gicentre$elm_vegalite$VegaLite$DNumbers = function (a) {
	return {$: 'DNumbers', a: a};
};
var $gicentre$elm_vegalite$VegaLite$doNums = $gicentre$elm_vegalite$VegaLite$DNumbers;
var $gicentre$elm_vegalite$VegaLite$VLEncoding = {$: 'VLEncoding'};
var $gicentre$elm_vegalite$VegaLite$encoding = function (channels) {
	return _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLEncoding,
		$elm$json$Json$Encode$object(channels));
};
var $gicentre$elm_vegalite$VegaLite$VLHeight = {$: 'VLHeight'};
var $gicentre$elm_vegalite$VegaLite$height = function (h) {
	return _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLHeight,
		$elm$json$Json$Encode$float(h));
};
var $gicentre$elm_vegalite$VegaLite$MLegend = function (a) {
	return {$: 'MLegend', a: a};
};
var $gicentre$elm_vegalite$VegaLite$mLegend = $gicentre$elm_vegalite$VegaLite$MLegend;
var $gicentre$elm_vegalite$VegaLite$MmType = function (a) {
	return {$: 'MmType', a: a};
};
var $gicentre$elm_vegalite$VegaLite$mMType = $gicentre$elm_vegalite$VegaLite$MmType;
var $gicentre$elm_vegalite$VegaLite$MName = function (a) {
	return {$: 'MName', a: a};
};
var $gicentre$elm_vegalite$VegaLite$mName = $gicentre$elm_vegalite$VegaLite$MName;
var $gicentre$elm_vegalite$VegaLite$Numbers = function (a) {
	return {$: 'Numbers', a: a};
};
var $gicentre$elm_vegalite$VegaLite$nums = $gicentre$elm_vegalite$VegaLite$Numbers;
var $gicentre$elm_vegalite$VegaLite$PmType = function (a) {
	return {$: 'PmType', a: a};
};
var $gicentre$elm_vegalite$VegaLite$pMType = $gicentre$elm_vegalite$VegaLite$PmType;
var $gicentre$elm_vegalite$VegaLite$PName = function (a) {
	return {$: 'PName', a: a};
};
var $gicentre$elm_vegalite$VegaLite$pName = $gicentre$elm_vegalite$VegaLite$PName;
var $gicentre$elm_vegalite$VegaLite$PScale = function (a) {
	return {$: 'PScale', a: a};
};
var $gicentre$elm_vegalite$VegaLite$pScale = $gicentre$elm_vegalite$VegaLite$PScale;
var $gicentre$elm_vegalite$VegaLite$Latitude = {$: 'Latitude'};
var $gicentre$elm_vegalite$VegaLite$Latitude2 = {$: 'Latitude2'};
var $gicentre$elm_vegalite$VegaLite$Longitude = {$: 'Longitude'};
var $gicentre$elm_vegalite$VegaLite$Longitude2 = {$: 'Longitude2'};
var $gicentre$elm_vegalite$VegaLite$X2 = {$: 'X2'};
var $gicentre$elm_vegalite$VegaLite$Y2 = {$: 'Y2'};
var $gicentre$elm_vegalite$VegaLite$axisProperty = function (axisProp) {
	switch (axisProp.$) {
		case 'AxBandPosition':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'bandPosition',
				$elm$json$Json$Encode$float(n));
		case 'AxFormat':
			var fmt = axisProp.a;
			return _Utils_Tuple2(
				'format',
				$elm$json$Json$Encode$string(fmt));
		case 'AxFormatAsNum':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('number'));
		case 'AxFormatAsTemporal':
			return _Utils_Tuple2(
				'formatType',
				$elm$json$Json$Encode$string('time'));
		case 'AxLabels':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'labels',
				$elm$json$Json$Encode$bool(b));
		case 'AxLabelAlign':
			var ha = axisProp.a;
			return _Utils_Tuple2(
				'labelAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(ha)));
		case 'AxLabelBaseline':
			var va = axisProp.a;
			return _Utils_Tuple2(
				'labelBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'AxLabelBound':
			var mn = axisProp.a;
			if (mn.$ === 'Just') {
				var n = mn.a;
				return (n === 1) ? _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$float(n));
			} else {
				return _Utils_Tuple2(
					'labelBound',
					$elm$json$Json$Encode$bool(false));
			}
		case 'AxLabelAngle':
			var angle = axisProp.a;
			return _Utils_Tuple2(
				'labelAngle',
				$elm$json$Json$Encode$float(angle));
		case 'AxLabelColor':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'labelColor',
				$elm$json$Json$Encode$string(s));
		case 'AxLabelFlush':
			var mn = axisProp.a;
			if (mn.$ === 'Just') {
				var n = mn.a;
				return (n === 1) ? _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$bool(true)) : _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$float(n));
			} else {
				return _Utils_Tuple2(
					'labelFlush',
					$elm$json$Json$Encode$bool(false));
			}
		case 'AxLabelFlushOffset':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'labelFlushOffset',
				$elm$json$Json$Encode$float(n));
		case 'AxLabelFont':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'labelFont',
				$elm$json$Json$Encode$string(s));
		case 'AxLabelFontSize':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'labelFontSize',
				$elm$json$Json$Encode$float(n));
		case 'AxLabelFontWeight':
			var fw = axisProp.a;
			return _Utils_Tuple2(
				'labelFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw));
		case 'AxLabelLimit':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'labelLimit',
				$elm$json$Json$Encode$float(n));
		case 'AxLabelOpacity':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'labelOpacity',
				$elm$json$Json$Encode$float(n));
		case 'AxLabelOverlap':
			var strat = axisProp.a;
			return _Utils_Tuple2(
				'labelOverlap',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$overlapStrategyLabel(strat)));
		case 'AxLabelPadding':
			var pad = axisProp.a;
			return _Utils_Tuple2(
				'labelPadding',
				$elm$json$Json$Encode$float(pad));
		case 'AxDomain':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'domain',
				$elm$json$Json$Encode$bool(b));
		case 'AxDomainColor':
			var c = axisProp.a;
			return _Utils_Tuple2(
				'domainColor',
				$elm$json$Json$Encode$string(c));
		case 'AxDomainOpacity':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'domainOpacity',
				$elm$json$Json$Encode$float(n));
		case 'AxDomainWidth':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'domainWidth',
				$elm$json$Json$Encode$float(n));
		case 'AxGrid':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'grid',
				$elm$json$Json$Encode$bool(b));
		case 'AxMaxExtent':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'maxExtent',
				$elm$json$Json$Encode$float(n));
		case 'AxMinExtent':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'minExtent',
				$elm$json$Json$Encode$float(n));
		case 'AxOrient':
			var side = axisProp.a;
			return _Utils_Tuple2(
				'orient',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$sideLabel(side)));
		case 'AxOffset':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'offset',
				$elm$json$Json$Encode$float(n));
		case 'AxPosition':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'position',
				$elm$json$Json$Encode$float(n));
		case 'AxZIndex':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'zindex',
				$elm$json$Json$Encode$int(n));
		case 'AxTicks':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'ticks',
				$elm$json$Json$Encode$bool(b));
		case 'AxTickColor':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'tickColor',
				$elm$json$Json$Encode$string(s));
		case 'AxTickCount':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'tickCount',
				$elm$json$Json$Encode$int(n));
		case 'AxTickExtra':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'tickExtra',
				$elm$json$Json$Encode$bool(b));
		case 'AxTickOffset':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'tickOffset',
				$elm$json$Json$Encode$float(n));
		case 'AxTickOpacity':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'tickOpacity',
				$elm$json$Json$Encode$float(n));
		case 'AxTickRound':
			var b = axisProp.a;
			return _Utils_Tuple2(
				'tickRound',
				$elm$json$Json$Encode$bool(b));
		case 'AxTickMinStep':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'tickMinStep',
				$elm$json$Json$Encode$float(n));
		case 'AxTickSize':
			var sz = axisProp.a;
			return _Utils_Tuple2(
				'tickSize',
				$elm$json$Json$Encode$float(sz));
		case 'AxTickWidth':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'tickWidth',
				$elm$json$Json$Encode$float(n));
		case 'AxValues':
			var vals = axisProp.a;
			return _Utils_Tuple2(
				'values',
				A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, vals));
		case 'AxDates':
			var dtss = axisProp.a;
			return _Utils_Tuple2(
				'values',
				A2(
					$elm$json$Json$Encode$list,
					function (ds) {
						return $elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, ds));
					},
					dtss));
		case 'AxTitle':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(s));
		case 'AxTitleAlign':
			var al = axisProp.a;
			return _Utils_Tuple2(
				'titleAlign',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$hAlignLabel(al)));
		case 'AxTitleAngle':
			var angle = axisProp.a;
			return _Utils_Tuple2(
				'titleAngle',
				$elm$json$Json$Encode$float(angle));
		case 'AxTitleBaseline':
			var va = axisProp.a;
			return _Utils_Tuple2(
				'titleBaseline',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$vAlignLabel(va)));
		case 'AxTitleColor':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'titleColor',
				$elm$json$Json$Encode$string(s));
		case 'AxTitleFont':
			var s = axisProp.a;
			return _Utils_Tuple2(
				'titleFont',
				$elm$json$Json$Encode$string(s));
		case 'AxTitleFontSize':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'titleFontSize',
				$elm$json$Json$Encode$float(n));
		case 'AxTitleFontWeight':
			var fw = axisProp.a;
			return _Utils_Tuple2(
				'titleFontWeight',
				$gicentre$elm_vegalite$VegaLite$fontWeightSpec(fw));
		case 'AxTitleLimit':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'titleLimit',
				$elm$json$Json$Encode$float(n));
		case 'AxTitleOpacity':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'titleOpacity',
				$elm$json$Json$Encode$float(n));
		case 'AxTitlePadding':
			var pad = axisProp.a;
			return _Utils_Tuple2(
				'titlePadding',
				$elm$json$Json$Encode$float(pad));
		case 'AxTitleX':
			var n = axisProp.a;
			return _Utils_Tuple2(
				'titleX',
				$elm$json$Json$Encode$float(n));
		default:
			var n = axisProp.a;
			return _Utils_Tuple2(
				'titleY',
				$elm$json$Json$Encode$float(n));
	}
};
var $gicentre$elm_vegalite$VegaLite$sortProperty = function (sp) {
	switch (sp.$) {
		case 'Ascending':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'order',
					$elm$json$Json$Encode$string('ascending'))
				]);
		case 'Descending':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'order',
					$elm$json$Json$Encode$string('descending'))
				]);
		case 'ByChannel':
			var ch = sp.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'encoding',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$channelLabel(ch)))
				]);
		case 'ByFieldOp':
			var field = sp.a;
			var op = sp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(field)),
					_Utils_Tuple2(
					'op',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$operationLabel(op)))
				]);
		case 'ByRepeatOp':
			var arr = sp.a;
			var op = sp.b;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							]))),
					_Utils_Tuple2(
					'op',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$operationLabel(op)))
				]);
		default:
			var dvs = sp.a;
			return _List_Nil;
	}
};
var $gicentre$elm_vegalite$VegaLite$positionChannelProperty = function (pDef) {
	switch (pDef.$) {
		case 'PName':
			var s = pDef.a;
			return _Utils_Tuple2(
				'field',
				$elm$json$Json$Encode$string(s));
		case 'PmType':
			var measure = pDef.a;
			return _Utils_Tuple2(
				'type',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$measurementLabel(measure)));
		case 'PBin':
			var bps = pDef.a;
			return $gicentre$elm_vegalite$VegaLite$bin(bps);
		case 'PBinned':
			return _Utils_Tuple2(
				'bin',
				$elm$json$Json$Encode$string('binned'));
		case 'PAggregate':
			var op = pDef.a;
			return _Utils_Tuple2(
				'aggregate',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$operationLabel(op)));
		case 'PTimeUnit':
			var tu = pDef.a;
			return _Utils_Tuple2(
				'timeUnit',
				$elm$json$Json$Encode$string(
					$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu)));
		case 'PTitle':
			var t = pDef.a;
			return _Utils_Tuple2(
				'title',
				$elm$json$Json$Encode$string(t));
		case 'PSort':
			var sps = pDef.a;
			_v1$4:
			while (true) {
				if (!sps.b) {
					return _Utils_Tuple2('sort', $elm$json$Json$Encode$null);
				} else {
					if (!sps.b.b) {
						switch (sps.a.$) {
							case 'Ascending':
								var _v2 = sps.a;
								return _Utils_Tuple2(
									'sort',
									$elm$json$Json$Encode$string('ascending'));
							case 'Descending':
								var _v3 = sps.a;
								return _Utils_Tuple2(
									'sort',
									$elm$json$Json$Encode$string('descending'));
							case 'CustomSort':
								var dvs = sps.a.a;
								return _Utils_Tuple2(
									'sort',
									$gicentre$elm_vegalite$VegaLite$toList(
										$gicentre$elm_vegalite$VegaLite$dataValuesSpecs(dvs)));
							default:
								break _v1$4;
						}
					} else {
						break _v1$4;
					}
				}
			}
			return _Utils_Tuple2(
				'sort',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$sortProperty, sps)));
		case 'PScale':
			var sps = pDef.a;
			return _Utils_eq(sps, _List_Nil) ? _Utils_Tuple2('scale', $elm$json$Json$Encode$null) : _Utils_Tuple2(
				'scale',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$scaleProperty, sps)));
		case 'PAxis':
			var aps = pDef.a;
			return _Utils_eq(aps, _List_Nil) ? _Utils_Tuple2('axis', $elm$json$Json$Encode$null) : _Utils_Tuple2(
				'axis',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$axisProperty, aps)));
		case 'PStack':
			var so = pDef.a;
			return $gicentre$elm_vegalite$VegaLite$stackOffset(so);
		case 'PRepeat':
			var arr = pDef.a;
			return _Utils_Tuple2(
				'field',
				$elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(
							'repeat',
							$elm$json$Json$Encode$string(
								$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
						])));
		case 'PWidth':
			return _Utils_Tuple2(
				'value',
				$elm$json$Json$Encode$string('width'));
		case 'PHeight':
			return _Utils_Tuple2(
				'value',
				$elm$json$Json$Encode$string('height'));
		case 'PNumber':
			var x = pDef.a;
			return _Utils_Tuple2(
				'value',
				$elm$json$Json$Encode$float(x));
		default:
			var ips = pDef.a;
			return _Utils_Tuple2(
				'impute',
				$elm$json$Json$Encode$object(
					A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$imputeProperty, ips)));
	}
};
var $gicentre$elm_vegalite$VegaLite$positionLabel = function (pChannel) {
	switch (pChannel.$) {
		case 'X':
			return 'x';
		case 'Y':
			return 'y';
		case 'X2':
			return 'x2';
		case 'Y2':
			return 'y2';
		case 'Longitude':
			return 'longitude';
		case 'Latitude':
			return 'latitude';
		case 'Longitude2':
			return 'longitude2';
		default:
			return 'latitude2';
	}
};
var $gicentre$elm_vegalite$VegaLite$position = F2(
	function (pos, pDefs) {
		var isNotPmType = function (pp) {
			if (pp.$ === 'PmType') {
				var t = pp.a;
				return false;
			} else {
				return true;
			}
		};
		switch (pos.$) {
			case 'X':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$X),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'Y':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Y),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'X2':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$X2),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'Y2':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Y2),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'Longitude':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Longitude),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'Latitude':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Latitude),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			case 'Longitude2':
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Longitude2),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
			default:
				return $elm$core$List$cons(
					_Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$positionLabel($gicentre$elm_vegalite$VegaLite$Latitude2),
						$elm$json$Json$Encode$object(
							A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$positionChannelProperty, pDefs))));
		}
	});
var $author$project$SamplePlot$samplePlotConfig = {height: 100, lblAngle: 0, width: 150};
var $gicentre$elm_vegalite$VegaLite$SDomain = function (a) {
	return {$: 'SDomain', a: a};
};
var $gicentre$elm_vegalite$VegaLite$scDomain = $gicentre$elm_vegalite$VegaLite$SDomain;
var $gicentre$elm_vegalite$VegaLite$Strings = function (a) {
	return {$: 'Strings', a: a};
};
var $gicentre$elm_vegalite$VegaLite$strs = $gicentre$elm_vegalite$VegaLite$Strings;
var $gicentre$elm_vegalite$VegaLite$vlPropertyLabel = function (spec) {
	switch (spec.$) {
		case 'VLName':
			return 'name';
		case 'VLDescription':
			return 'description';
		case 'VLTitle':
			return 'title';
		case 'VLWidth':
			return 'width';
		case 'VLHeight':
			return 'height';
		case 'VLPadding':
			return 'padding';
		case 'VLAutosize':
			return 'autosize';
		case 'VLBackground':
			return 'background';
		case 'VLData':
			return 'data';
		case 'VLDatasets':
			return 'datasets';
		case 'VLProjection':
			return 'projection';
		case 'VLMark':
			return 'mark';
		case 'VLTransform':
			return 'transform';
		case 'VLEncoding':
			return 'encoding';
		case 'VLConfig':
			return 'config';
		case 'VLSelection':
			return 'selection';
		case 'VLConcat':
			return 'concat';
		case 'VLColumns':
			return 'columns';
		case 'VLHConcat':
			return 'hconcat';
		case 'VLVConcat':
			return 'vconcat';
		case 'VLLayer':
			return 'layer';
		case 'VLRepeat':
			return 'repeat';
		case 'VLFacet':
			return 'facet';
		case 'VLSpacing':
			return 'spacing';
		case 'VLAlign':
			return 'align';
		case 'VLBounds':
			return 'bounds';
		case 'VLCenter':
			return 'center';
		case 'VLSpec':
			return 'spec';
		case 'VLResolve':
			return 'resolve';
		default:
			return 'view';
	}
};
var $gicentre$elm_vegalite$VegaLite$toVegaLite = function (spec) {
	return $elm$json$Json$Encode$object(
		A2(
			$elm$core$List$cons,
			_Utils_Tuple2(
				'$schema',
				$elm$json$Json$Encode$string('https://vega.github.io/schema/vega-lite/v3.json')),
			A2(
				$elm$core$List$map,
				function (_v0) {
					var s = _v0.a;
					var v = _v0.b;
					return _Utils_Tuple2(
						$gicentre$elm_vegalite$VegaLite$vlPropertyLabel(s),
						v);
				},
				spec)));
};
var $gicentre$elm_vegalite$VegaLite$VLWidth = {$: 'VLWidth'};
var $gicentre$elm_vegalite$VegaLite$width = function (w) {
	return _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLWidth,
		$elm$json$Json$Encode$float(w));
};
var $author$project$SamplePlot$samplePlot = function (sample) {
	var xs = _List_fromArray(
		[sample.successLbl, sample.failureLbl]);
	var n = sample.n;
	var pFailure = sample.numFailures / n;
	var pSuccess = sample.numSuccess / n;
	var ys = _List_fromArray(
		[pSuccess, pFailure]);
	var enc = A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				$gicentre$elm_vegalite$VegaLite$encoding,
				A2(
					$gicentre$elm_vegalite$VegaLite$position,
					$gicentre$elm_vegalite$VegaLite$X,
					_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$pName('Outcome'),
							$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Nominal)
						]))),
			A2(
				$gicentre$elm_vegalite$VegaLite$position,
				$gicentre$elm_vegalite$VegaLite$Y,
				_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$pName('Frequency'),
						$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Quantitative),
						$gicentre$elm_vegalite$VegaLite$pScale(
						_List_fromArray(
							[
								$gicentre$elm_vegalite$VegaLite$scDomain(
								$gicentre$elm_vegalite$VegaLite$doNums(
									_List_fromArray(
										[0, 1])))
							]))
					]))),
		$gicentre$elm_vegalite$VegaLite$color(
			_List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$mName('Outcome'),
					$gicentre$elm_vegalite$VegaLite$mMType($gicentre$elm_vegalite$VegaLite$Nominal),
					$gicentre$elm_vegalite$VegaLite$mLegend(_List_Nil)
				])));
	var data = A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			$gicentre$elm_vegalite$VegaLite$dataFromColumns(_List_Nil),
			A2(
				$gicentre$elm_vegalite$VegaLite$dataColumn,
				'Outcome',
				$gicentre$elm_vegalite$VegaLite$strs(xs))),
		A2(
			$gicentre$elm_vegalite$VegaLite$dataColumn,
			'Frequency',
			$gicentre$elm_vegalite$VegaLite$nums(ys)));
	var cfg = A2(
		$elm$core$Basics$composeL,
		$gicentre$elm_vegalite$VegaLite$configure,
		$gicentre$elm_vegalite$VegaLite$configuration(
			$gicentre$elm_vegalite$VegaLite$coAxis(
				_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$axcoGridOpacity(0.1),
						$gicentre$elm_vegalite$VegaLite$axcoLabelAngle($author$project$SamplePlot$samplePlotConfig.lblAngle)
					]))));
	return $gicentre$elm_vegalite$VegaLite$toVegaLite(
		_List_fromArray(
			[
				data(_List_Nil),
				$gicentre$elm_vegalite$VegaLite$height($author$project$SamplePlot$samplePlotConfig.height),
				$gicentre$elm_vegalite$VegaLite$width($author$project$SamplePlot$samplePlotConfig.width),
				cfg(_List_Nil),
				$gicentre$elm_vegalite$VegaLite$bar(_List_Nil),
				enc(_List_Nil)
			]));
};
var $author$project$Main$samplePlotCmd = F3(
	function (cmd, dotSample, model) {
		var _v0 = dotSample(model);
		if (_v0.$ === 'Nothing') {
			return $elm$core$Platform$Cmd$none;
		} else {
			var sample = _v0.a;
			return cmd(
				$author$project$SamplePlot$samplePlot(sample));
		}
	});
var $author$project$Main$bootstrapPlotCmd = A2(
	$author$project$Main$samplePlotCmd,
	$author$project$Main$bootstrapPlotToJS,
	function ($) {
		return $.bootstrapSample;
	});
var $gicentre$elm_vegalite$VegaLite$asSpec = function (specs) {
	return $elm$json$Json$Encode$object(
		A2(
			$elm$core$List$map,
			function (_v0) {
				var s = _v0.a;
				var v = _v0.b;
				return _Utils_Tuple2(
					$gicentre$elm_vegalite$VegaLite$vlPropertyLabel(s),
					v);
			},
			specs));
};
var $gicentre$elm_vegalite$VegaLite$AxTitle = function (a) {
	return {$: 'AxTitle', a: a};
};
var $gicentre$elm_vegalite$VegaLite$axTitle = $gicentre$elm_vegalite$VegaLite$AxTitle;
var $author$project$Defaults$defaults = {
	collectNs: _List_fromArray(
		[1, 10, 100, 1000, 10000, 100000]),
	distMinHeight: 100.0,
	distPlotHeight: 525,
	distPlotWidth: 700,
	largePlot: 5000,
	levels: _List_fromArray(
		[0.8, 0.9, 0.95, 0.99]),
	levelsTxt: _List_fromArray(
		['80%', '90%', '95%', '99%']),
	minTrialsForPValue: 100,
	n: 200,
	numSD: 4.0,
	p: 0.25,
	pValDigits: 4,
	trimAt: 100
};
var $gicentre$elm_vegalite$VegaLite$FExpr = function (a) {
	return {$: 'FExpr', a: a};
};
var $gicentre$elm_vegalite$VegaLite$fiExpr = $gicentre$elm_vegalite$VegaLite$FExpr;
var $gicentre$elm_vegalite$VegaLite$filter = function (f) {
	switch (f.$) {
		case 'FExpr':
			var ex = f.a;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$string(ex)));
		case 'FCompose':
			var boolExpr = f.a;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$gicentre$elm_vegalite$VegaLite$booleanOpSpec(boolExpr)));
		case 'FEqual':
			var field = f.a;
			var val = f.b;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'equal',
								$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
							]))));
		case 'FLessThan':
			var field = f.a;
			var val = f.b;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'lt',
								$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
							]))));
		case 'FLessThanEq':
			var field = f.a;
			var val = f.b;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'lte',
								$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
							]))));
		case 'FGreaterThan':
			var field = f.a;
			var val = f.b;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'gt',
								$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
							]))));
		case 'FGreaterThanEq':
			var field = f.a;
			var val = f.b;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'gte',
								$gicentre$elm_vegalite$VegaLite$dataValueSpec(val))
							]))));
		case 'FSelection':
			var selName = f.a;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'selection',
								$elm$json$Json$Encode$string(selName))
							]))));
		case 'FRange':
			var field = f.a;
			var vals = f.b;
			var values = function () {
				if (vals.$ === 'NumberRange') {
					var mn = vals.a;
					var mx = vals.b;
					return A2(
						$elm$json$Json$Encode$list,
						$elm$json$Json$Encode$float,
						_List_fromArray(
							[mn, mx]));
				} else {
					if (!vals.a.b) {
						if (!vals.b.b) {
							return $gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[$elm$json$Json$Encode$null, $elm$json$Json$Encode$null]));
						} else {
							var dMax = vals.b;
							return $gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[
										$elm$json$Json$Encode$null,
										$elm$json$Json$Encode$object(
										A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, dMax))
									]));
						}
					} else {
						if (!vals.b.b) {
							var dMin = vals.a;
							return $gicentre$elm_vegalite$VegaLite$toList(
								_List_fromArray(
									[
										$elm$json$Json$Encode$object(
										A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, dMin)),
										$elm$json$Json$Encode$null
									]));
						} else {
							var dMin = vals.a;
							var dMax = vals.b;
							return A2(
								$elm$json$Json$Encode$list,
								$elm$json$Json$Encode$object,
								_List_fromArray(
									[
										A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, dMin),
										A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, dMax)
									]));
						}
					}
				}
			}();
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2('range', values)
							]))));
		case 'FOneOf':
			var field = f.a;
			var vals = f.b;
			var values = function () {
				switch (vals.$) {
					case 'Numbers':
						var xs = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$float, xs);
					case 'DateTimes':
						var ds = vals.a;
						return A2(
							$elm$json$Json$Encode$list,
							function (d) {
								return $elm$json$Json$Encode$object(
									A2($elm$core$List$map, $gicentre$elm_vegalite$VegaLite$dateTimeProperty, d));
							},
							ds);
					case 'Strings':
						var ss = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$string, ss);
					default:
						var bs = vals.a;
						return A2($elm$json$Json$Encode$list, $elm$json$Json$Encode$bool, bs);
				}
			}();
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2('oneOf', values)
							]))));
		default:
			var field = f.a;
			return $elm$core$List$cons(
				_Utils_Tuple2(
					'filter',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'field',
								$elm$json$Json$Encode$string(field)),
								_Utils_Tuple2(
								'valid',
								$elm$json$Json$Encode$bool(true))
							]))));
	}
};
var $gicentre$elm_vegalite$VegaLite$VLLayer = {$: 'VLLayer'};
var $gicentre$elm_vegalite$VegaLite$layer = function (specs) {
	return _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLLayer,
		$gicentre$elm_vegalite$VegaLite$toList(specs));
};
var $gicentre$elm_vegalite$VegaLite$MString = function (a) {
	return {$: 'MString', a: a};
};
var $gicentre$elm_vegalite$VegaLite$mStr = $gicentre$elm_vegalite$VegaLite$MString;
var $gicentre$elm_vegalite$VegaLite$Sum = {$: 'Sum'};
var $gicentre$elm_vegalite$VegaLite$opSum = $gicentre$elm_vegalite$VegaLite$Sum;
var $gicentre$elm_vegalite$VegaLite$PAggregate = function (a) {
	return {$: 'PAggregate', a: a};
};
var $gicentre$elm_vegalite$VegaLite$pAggregate = $gicentre$elm_vegalite$VegaLite$PAggregate;
var $gicentre$elm_vegalite$VegaLite$PAxis = function (a) {
	return {$: 'PAxis', a: a};
};
var $gicentre$elm_vegalite$VegaLite$pAxis = $gicentre$elm_vegalite$VegaLite$PAxis;
var $gicentre$elm_vegalite$VegaLite$TFormat = function (a) {
	return {$: 'TFormat', a: a};
};
var $gicentre$elm_vegalite$VegaLite$tFormat = $gicentre$elm_vegalite$VegaLite$TFormat;
var $gicentre$elm_vegalite$VegaLite$TmType = function (a) {
	return {$: 'TmType', a: a};
};
var $gicentre$elm_vegalite$VegaLite$tMType = $gicentre$elm_vegalite$VegaLite$TmType;
var $gicentre$elm_vegalite$VegaLite$TName = function (a) {
	return {$: 'TName', a: a};
};
var $gicentre$elm_vegalite$VegaLite$tName = $gicentre$elm_vegalite$VegaLite$TName;
var $gicentre$elm_vegalite$VegaLite$textChannelProperty = function (tDef) {
	switch (tDef.$) {
		case 'TName':
			var s = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$string(s))
				]);
		case 'TRepeat':
			var arr = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'field',
					$elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2(
								'repeat',
								$elm$json$Json$Encode$string(
									$gicentre$elm_vegalite$VegaLite$arrangementLabel(arr)))
							])))
				]);
		case 'TmType':
			var measure = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'type',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$measurementLabel(measure)))
				]);
		case 'TBin':
			var bps = tDef.a;
			return _List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$bin(bps)
				]);
		case 'TBinned':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'bin',
					$elm$json$Json$Encode$string('binned'))
				]);
		case 'TAggregate':
			var op = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'aggregate',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$operationLabel(op)))
				]);
		case 'TTimeUnit':
			var tu = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'timeUnit',
					$elm$json$Json$Encode$string(
						$gicentre$elm_vegalite$VegaLite$timeUnitLabel(tu)))
				]);
		case 'TTitle':
			var t = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'title',
					$elm$json$Json$Encode$string(t))
				]);
		case 'TFormat':
			var fmt = tDef.a;
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'format',
					$elm$json$Json$Encode$string(fmt))
				]);
		case 'TFormatAsNum':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('number'))
				]);
		case 'TFormatAsTemporal':
			return _List_fromArray(
				[
					_Utils_Tuple2(
					'formatType',
					$elm$json$Json$Encode$string('time'))
				]);
		case 'TSelectionCondition':
			var selName = tDef.a;
			var ifClause = tDef.b;
			var elseClause = tDef.c;
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					$elm$json$Json$Encode$object(
						A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								'selection',
								$gicentre$elm_vegalite$VegaLite$booleanOpSpec(selName)),
							A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$textChannelProperty, ifClause)))),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$textChannelProperty, elseClause));
		default:
			var tests = tDef.a;
			var elseClause = tDef.b;
			var testClause = function (_v1) {
				var predicate = _v1.a;
				var ifClause = _v1.b;
				return $elm$json$Json$Encode$object(
					A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							'test',
							$gicentre$elm_vegalite$VegaLite$booleanOpSpec(predicate)),
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$textChannelProperty, ifClause)));
			};
			return A2(
				$elm$core$List$cons,
				_Utils_Tuple2(
					'condition',
					A2($elm$json$Json$Encode$list, testClause, tests)),
				A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$textChannelProperty, elseClause));
	}
};
var $gicentre$elm_vegalite$VegaLite$tooltips = function (tDefss) {
	return $elm$core$List$cons(
		_Utils_Tuple2(
			'tooltip',
			A2(
				$elm$json$Json$Encode$list,
				function (tDefs) {
					return $elm$json$Json$Encode$object(
						A2($elm$core$List$concatMap, $gicentre$elm_vegalite$VegaLite$textChannelProperty, tDefs));
				},
				tDefss)));
};
var $gicentre$elm_vegalite$VegaLite$VLTransform = {$: 'VLTransform'};
var $elm$json$Json$Decode$decodeString = _Json_runOnString;
var $elm$core$List$isEmpty = function (xs) {
	if (!xs.b) {
		return true;
	} else {
		return false;
	}
};
var $elm$json$Json$Decode$list = _Json_decodeList;
var $elm$json$Json$Decode$value = _Json_decodeValue;
var $gicentre$elm_vegalite$VegaLite$transform = function (transforms) {
	var assemble = function (_v60) {
		var trName = _v60.a;
		var val = _v60.b;
		switch (trName) {
			case 'aggregate':
				var _v1 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((_v1.$ === 'Ok') && _v1.a.b) && _v1.a.b.b) && (!_v1.a.b.b.b)) {
					var _v2 = _v1.a;
					var ops = _v2.a;
					var _v3 = _v2.b;
					var groups = _v3.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('aggregate', ops),
								_Utils_Tuple2('groupby', groups)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'bin':
				var _v4 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if (((((_v4.$ === 'Ok') && _v4.a.b) && _v4.a.b.b) && _v4.a.b.b.b) && (!_v4.a.b.b.b.b)) {
					var _v5 = _v4.a;
					var binParams = _v5.a;
					var _v6 = _v5.b;
					var field = _v6.a;
					var _v7 = _v6.b;
					var label = _v7.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('bin', binParams),
								_Utils_Tuple2('field', field),
								_Utils_Tuple2('as', label)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'calculate':
				var _v8 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((_v8.$ === 'Ok') && _v8.a.b) && _v8.a.b.b) && (!_v8.a.b.b.b)) {
					var _v9 = _v8.a;
					var ex = _v9.a;
					var _v10 = _v9.b;
					var label = _v10.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('calculate', ex),
								_Utils_Tuple2('as', label)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'impute':
				var _v11 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((((((((_v11.$ === 'Ok') && _v11.a.b) && _v11.a.b.b) && _v11.a.b.b.b) && _v11.a.b.b.b.b) && _v11.a.b.b.b.b.b) && _v11.a.b.b.b.b.b.b) && _v11.a.b.b.b.b.b.b.b) && _v11.a.b.b.b.b.b.b.b.b) && (!_v11.a.b.b.b.b.b.b.b.b.b)) {
					var _v12 = _v11.a;
					var imp = _v12.a;
					var _v13 = _v12.b;
					var key = _v13.a;
					var _v14 = _v13.b;
					var frameObj = _v14.a;
					var _v15 = _v14.b;
					var keyValsObj = _v15.a;
					var _v16 = _v15.b;
					var keyValSequenceObj = _v16.a;
					var _v17 = _v16.b;
					var methodObj = _v17.a;
					var _v18 = _v17.b;
					var groupbyObj = _v18.a;
					var _v19 = _v18.b;
					var valueObj = _v19.a;
					return $elm$json$Json$Encode$object(
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2('impute', imp),
									_Utils_Tuple2('key', key)
								]),
							_Utils_ap(
								_Utils_eq(frameObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
									[
										_Utils_Tuple2('frame', frameObj)
									]),
								_Utils_ap(
									_Utils_eq(keyValsObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
										[
											_Utils_Tuple2('keyvals', keyValsObj)
										]),
									_Utils_ap(
										_Utils_eq(keyValSequenceObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
											[
												_Utils_Tuple2('keyvals', keyValSequenceObj)
											]),
										_Utils_ap(
											_Utils_eq(methodObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
												[
													_Utils_Tuple2('method', methodObj)
												]),
											_Utils_ap(
												_Utils_eq(groupbyObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
													[
														_Utils_Tuple2('groupby', groupbyObj)
													]),
												_Utils_eq(valueObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
													[
														_Utils_Tuple2('value', valueObj)
													]))))))));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'lookup':
				var _v20 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((((_v20.$ === 'Ok') && _v20.a.b) && _v20.a.b.b) && _v20.a.b.b.b) && _v20.a.b.b.b.b) && (!_v20.a.b.b.b.b.b)) {
					var _v21 = _v20.a;
					var key1 = _v21.a;
					var _v22 = _v21.b;
					var dataSpec = _v22.a;
					var _v23 = _v22.b;
					var key2 = _v23.a;
					var _v24 = _v23.b;
					var fields = _v24.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('lookup', key1),
								_Utils_Tuple2(
								'from',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2('data', dataSpec),
											_Utils_Tuple2('key', key2),
											_Utils_Tuple2('fields', fields)
										])))
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'lookupAs':
				var _v25 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((((_v25.$ === 'Ok') && _v25.a.b) && _v25.a.b.b) && _v25.a.b.b.b) && _v25.a.b.b.b.b) && (!_v25.a.b.b.b.b.b)) {
					var _v26 = _v25.a;
					var key1 = _v26.a;
					var _v27 = _v26.b;
					var dataSpec = _v27.a;
					var _v28 = _v27.b;
					var key2 = _v28.a;
					var _v29 = _v28.b;
					var asName = _v29.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('lookup', key1),
								_Utils_Tuple2(
								'from',
								$elm$json$Json$Encode$object(
									_List_fromArray(
										[
											_Utils_Tuple2('data', dataSpec),
											_Utils_Tuple2('key', key2)
										]))),
								_Utils_Tuple2('as', asName)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'flattenAs':
				var _v30 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((_v30.$ === 'Ok') && _v30.a.b) && _v30.a.b.b) && (!_v30.a.b.b.b)) {
					var _v31 = _v30.a;
					var fields = _v31.a;
					var _v32 = _v31.b;
					var names = _v32.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('flatten', fields),
								_Utils_Tuple2('as', names)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'foldAs':
				var _v33 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if (((((_v33.$ === 'Ok') && _v33.a.b) && _v33.a.b.b) && _v33.a.b.b.b) && (!_v33.a.b.b.b.b)) {
					var _v34 = _v33.a;
					var fields = _v34.a;
					var _v35 = _v34.b;
					var keyName = _v35.a;
					var _v36 = _v35.b;
					var valName = _v36.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('fold', fields),
								_Utils_Tuple2(
								'as',
								$gicentre$elm_vegalite$VegaLite$toList(
									_List_fromArray(
										[keyName, valName])))
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'stack':
				var _v37 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if ((((((((_v37.$ === 'Ok') && _v37.a.b) && _v37.a.b.b) && _v37.a.b.b.b) && _v37.a.b.b.b.b) && _v37.a.b.b.b.b.b) && _v37.a.b.b.b.b.b.b) && (!_v37.a.b.b.b.b.b.b.b)) {
					var _v38 = _v37.a;
					var field = _v38.a;
					var _v39 = _v38.b;
					var grp = _v39.a;
					var _v40 = _v39.b;
					var start = _v40.a;
					var _v41 = _v40.b;
					var end = _v41.a;
					var _v42 = _v41.b;
					var offsetObj = _v42.a;
					var _v43 = _v42.b;
					var sortObj = _v43.a;
					return $elm$json$Json$Encode$object(
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2('stack', field),
									_Utils_Tuple2('groupby', grp),
									_Utils_Tuple2(
									'as',
									$gicentre$elm_vegalite$VegaLite$toList(
										_List_fromArray(
											[start, end])))
								]),
							_Utils_ap(
								_Utils_eq(offsetObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
									[
										_Utils_Tuple2('offset', offsetObj)
									]),
								_Utils_eq(sortObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
									[
										_Utils_Tuple2('sort', sortObj)
									]))));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'timeUnit':
				var _v44 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if (((((_v44.$ === 'Ok') && _v44.a.b) && _v44.a.b.b) && _v44.a.b.b.b) && (!_v44.a.b.b.b.b)) {
					var _v45 = _v44.a;
					var tu = _v45.a;
					var _v46 = _v45.b;
					var field = _v46.a;
					var _v47 = _v46.b;
					var label = _v47.a;
					return $elm$json$Json$Encode$object(
						_List_fromArray(
							[
								_Utils_Tuple2('timeUnit', tu),
								_Utils_Tuple2('field', field),
								_Utils_Tuple2('as', label)
							]));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'window':
				var _v48 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if (((((((_v48.$ === 'Ok') && _v48.a.b) && _v48.a.b.b) && _v48.a.b.b.b) && _v48.a.b.b.b.b) && _v48.a.b.b.b.b.b) && (!_v48.a.b.b.b.b.b.b)) {
					var _v49 = _v48.a;
					var winObj = _v49.a;
					var _v50 = _v49.b;
					var frameObj = _v50.a;
					var _v51 = _v50.b;
					var peersObj = _v51.a;
					var _v52 = _v51.b;
					var groupbyObj = _v52.a;
					var _v53 = _v52.b;
					var sortObj = _v53.a;
					return $elm$json$Json$Encode$object(
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2('window', winObj)
								]),
							_Utils_ap(
								_Utils_eq(frameObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
									[
										_Utils_Tuple2('frame', frameObj)
									]),
								_Utils_ap(
									_Utils_eq(peersObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
										[
											_Utils_Tuple2('ignorePeers', peersObj)
										]),
									_Utils_ap(
										_Utils_eq(groupbyObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
											[
												_Utils_Tuple2('groupby', groupbyObj)
											]),
										_Utils_eq(sortObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
											[
												_Utils_Tuple2('sort', sortObj)
											]))))));
				} else {
					return $elm$json$Json$Encode$null;
				}
			case 'joinaggregate':
				var _v54 = A2(
					$elm$json$Json$Decode$decodeString,
					$elm$json$Json$Decode$list($elm$json$Json$Decode$value),
					A2($elm$json$Json$Encode$encode, 0, val));
				if (((((((_v54.$ === 'Ok') && _v54.a.b) && _v54.a.b.b) && _v54.a.b.b.b) && _v54.a.b.b.b.b) && _v54.a.b.b.b.b.b) && (!_v54.a.b.b.b.b.b.b)) {
					var _v55 = _v54.a;
					var joinObjs = _v55.a;
					var _v56 = _v55.b;
					var frameObj = _v56.a;
					var _v57 = _v56.b;
					var peersObj = _v57.a;
					var _v58 = _v57.b;
					var groupbyObj = _v58.a;
					var _v59 = _v58.b;
					var sortObj = _v59.a;
					return $elm$json$Json$Encode$object(
						_Utils_ap(
							_List_fromArray(
								[
									_Utils_Tuple2('joinaggregate', joinObjs)
								]),
							_Utils_ap(
								_Utils_eq(frameObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
									[
										_Utils_Tuple2('frame', frameObj)
									]),
								_Utils_ap(
									_Utils_eq(peersObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
										[
											_Utils_Tuple2('ignorePeers', peersObj)
										]),
									_Utils_ap(
										_Utils_eq(groupbyObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
											[
												_Utils_Tuple2('groupby', groupbyObj)
											]),
										_Utils_eq(sortObj, $elm$json$Json$Encode$null) ? _List_Nil : _List_fromArray(
											[
												_Utils_Tuple2('sort', sortObj)
											]))))));
				} else {
					return $elm$json$Json$Encode$null;
				}
			default:
				return $elm$json$Json$Encode$object(
					_List_fromArray(
						[
							_Utils_Tuple2(trName, val)
						]));
		}
	};
	return $elm$core$List$isEmpty(transforms) ? _Utils_Tuple2($gicentre$elm_vegalite$VegaLite$VLTransform, $elm$json$Json$Encode$null) : _Utils_Tuple2(
		$gicentre$elm_vegalite$VegaLite$VLTransform,
		A2($elm$json$Json$Encode$list, assemble, transforms));
};
var $author$project$DistPlot$distPlot = function (config) {
	var trans = A2(
		$elm$core$Basics$composeL,
		$gicentre$elm_vegalite$VegaLite$transform,
		$gicentre$elm_vegalite$VegaLite$filter(
			$gicentre$elm_vegalite$VegaLite$fiExpr(config.tailExpression)));
	var selectedEnc = A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				A2(
					$elm$core$Basics$composeL,
					$gicentre$elm_vegalite$VegaLite$encoding,
					A2(
						$gicentre$elm_vegalite$VegaLite$position,
						$gicentre$elm_vegalite$VegaLite$X,
						_List_fromArray(
							[
								$gicentre$elm_vegalite$VegaLite$pName('X'),
								$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Quantitative),
								$gicentre$elm_vegalite$VegaLite$pAxis(
								_List_fromArray(
									[
										$gicentre$elm_vegalite$VegaLite$axTitle(config.xAxisTitle)
									]))
							]))),
				A2(
					$gicentre$elm_vegalite$VegaLite$position,
					$gicentre$elm_vegalite$VegaLite$Y,
					_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$pName('N(X)'),
							$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Quantitative),
							$gicentre$elm_vegalite$VegaLite$pAxis(
							_List_fromArray(
								[
									$gicentre$elm_vegalite$VegaLite$axTitle('Frequency')
								]))
						]))),
			$gicentre$elm_vegalite$VegaLite$tooltips(
				_List_fromArray(
					[
						_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$tName('X'),
							$gicentre$elm_vegalite$VegaLite$tMType($gicentre$elm_vegalite$VegaLite$Quantitative)
						]),
						_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$tName('N(X)'),
							$gicentre$elm_vegalite$VegaLite$tFormat('.0f')
						])
					]))),
		$gicentre$elm_vegalite$VegaLite$color(
			_List_fromArray(
				[
					$gicentre$elm_vegalite$VegaLite$mStr('red'),
					$gicentre$elm_vegalite$VegaLite$mLegend(_List_Nil)
				])));
	var encPMF = A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			A2(
				$elm$core$Basics$composeL,
				$gicentre$elm_vegalite$VegaLite$encoding,
				A2(
					$gicentre$elm_vegalite$VegaLite$position,
					$gicentre$elm_vegalite$VegaLite$X,
					_List_fromArray(
						[
							$gicentre$elm_vegalite$VegaLite$pName('X'),
							$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Quantitative),
							$gicentre$elm_vegalite$VegaLite$pScale(
							_List_fromArray(
								[
									$gicentre$elm_vegalite$VegaLite$scDomain(
									$gicentre$elm_vegalite$VegaLite$doNums(
										_List_fromArray(
											[config.minX, config.maxX])))
								]))
						]))),
			A2(
				$gicentre$elm_vegalite$VegaLite$position,
				$gicentre$elm_vegalite$VegaLite$Y,
				_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$pName('N(X)'),
						$gicentre$elm_vegalite$VegaLite$pAggregate($gicentre$elm_vegalite$VegaLite$opSum),
						$gicentre$elm_vegalite$VegaLite$pMType($gicentre$elm_vegalite$VegaLite$Quantitative),
						$gicentre$elm_vegalite$VegaLite$pScale(
						_List_fromArray(
							[
								$gicentre$elm_vegalite$VegaLite$scDomain(
								$gicentre$elm_vegalite$VegaLite$doNums(
									_List_fromArray(
										[0.0, config.maxY])))
							]))
					]))),
		$gicentre$elm_vegalite$VegaLite$tooltips(
			_List_fromArray(
				[
					_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$tName('X'),
						$gicentre$elm_vegalite$VegaLite$tMType($gicentre$elm_vegalite$VegaLite$Quantitative)
					]),
					_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$tName('N(X)'),
						$gicentre$elm_vegalite$VegaLite$tMType($gicentre$elm_vegalite$VegaLite$Quantitative),
						$gicentre$elm_vegalite$VegaLite$tFormat('.0f')
					])
				])));
	var d = A2(
		$elm$core$Basics$composeL,
		A2(
			$elm$core$Basics$composeL,
			$gicentre$elm_vegalite$VegaLite$dataFromColumns(_List_Nil),
			A2(
				$gicentre$elm_vegalite$VegaLite$dataColumn,
				'X',
				$gicentre$elm_vegalite$VegaLite$nums(config.xs))),
		A2(
			$gicentre$elm_vegalite$VegaLite$dataColumn,
			'N(X)',
			$gicentre$elm_vegalite$VegaLite$nums(config.ys)));
	return $gicentre$elm_vegalite$VegaLite$toVegaLite(
		_List_fromArray(
			[
				$gicentre$elm_vegalite$VegaLite$width($author$project$Defaults$defaults.distPlotWidth),
				$gicentre$elm_vegalite$VegaLite$height($author$project$Defaults$defaults.distPlotHeight),
				d(_List_Nil),
				$gicentre$elm_vegalite$VegaLite$layer(
				_List_fromArray(
					[
						$gicentre$elm_vegalite$VegaLite$asSpec(
						_List_fromArray(
							[
								config.mark(_List_Nil),
								encPMF(_List_Nil)
							])),
						$gicentre$elm_vegalite$VegaLite$asSpec(
						_List_fromArray(
							[
								config.mark(_List_Nil),
								selectedEnc(_List_Nil),
								trans(_List_Nil)
							]))
					]))
			]));
};
var $author$project$Main$distPlotToJS = _Platform_outgoingPort('distPlotToJS', $elm$core$Basics$identity);
var $author$project$Main$distPlotCmd = function (model) {
	var _v0 = model.distPlotConfig;
	if (_v0.$ === 'Nothing') {
		return $elm$core$Platform$Cmd$none;
	} else {
		var config = _v0.a;
		return $author$project$Main$distPlotToJS(
			$author$project$DistPlot$distPlot(config));
	}
};
var $elm$file$File$Select$file = F2(
	function (mimes, toMsg) {
		return A2(
			$elm$core$Task$perform,
			toMsg,
			_File_uploadOne(mimes));
	});
var $elm_community$list_extra$List$Extra$find = F2(
	function (predicate, list) {
		find:
		while (true) {
			if (!list.b) {
				return $elm$core$Maybe$Nothing;
			} else {
				var first = list.a;
				var rest = list.b;
				if (predicate(first)) {
					return $elm$core$Maybe$Just(first);
				} else {
					var $temp$predicate = predicate,
						$temp$list = rest;
					predicate = $temp$predicate;
					list = $temp$list;
					continue find;
				}
			}
		}
	});
var $elm$random$Random$Generator = function (a) {
	return {$: 'Generator', a: a};
};
var $elm$core$Basics$negate = function (n) {
	return -n;
};
var $elm$core$Basics$abs = function (n) {
	return (n < 0) ? (-n) : n;
};
var $elm$core$Bitwise$and = _Bitwise_and;
var $elm$random$Random$Seed = F2(
	function (a, b) {
		return {$: 'Seed', a: a, b: b};
	});
var $elm$core$Bitwise$shiftRightZfBy = _Bitwise_shiftRightZfBy;
var $elm$random$Random$next = function (_v0) {
	var state0 = _v0.a;
	var incr = _v0.b;
	return A2($elm$random$Random$Seed, ((state0 * 1664525) + incr) >>> 0, incr);
};
var $elm$core$Bitwise$xor = _Bitwise_xor;
var $elm$random$Random$peel = function (_v0) {
	var state = _v0.a;
	var word = (state ^ (state >>> ((state >>> 28) + 4))) * 277803737;
	return ((word >>> 22) ^ word) >>> 0;
};
var $elm$random$Random$float = F2(
	function (a, b) {
		return $elm$random$Random$Generator(
			function (seed0) {
				var seed1 = $elm$random$Random$next(seed0);
				var range = $elm$core$Basics$abs(b - a);
				var n1 = $elm$random$Random$peel(seed1);
				var n0 = $elm$random$Random$peel(seed0);
				var lo = (134217727 & n1) * 1.0;
				var hi = (67108863 & n0) * 1.0;
				var val = ((hi * 134217728.0) + lo) / 9007199254740992.0;
				var scaled = (val * range) + a;
				return _Utils_Tuple2(
					scaled,
					$elm$random$Random$next(seed1));
			});
	});
var $elm$random$Random$Generate = function (a) {
	return {$: 'Generate', a: a};
};
var $elm$random$Random$initialSeed = function (x) {
	var _v0 = $elm$random$Random$next(
		A2($elm$random$Random$Seed, 0, 1013904223));
	var state1 = _v0.a;
	var incr = _v0.b;
	var state2 = (state1 + x) >>> 0;
	return $elm$random$Random$next(
		A2($elm$random$Random$Seed, state2, incr));
};
var $elm$time$Time$Name = function (a) {
	return {$: 'Name', a: a};
};
var $elm$time$Time$Offset = function (a) {
	return {$: 'Offset', a: a};
};
var $elm$time$Time$Zone = F2(
	function (a, b) {
		return {$: 'Zone', a: a, b: b};
	});
var $elm$time$Time$customZone = $elm$time$Time$Zone;
var $elm$time$Time$now = _Time_now($elm$time$Time$millisToPosix);
var $elm$time$Time$posixToMillis = function (_v0) {
	var millis = _v0.a;
	return millis;
};
var $elm$random$Random$init = A2(
	$elm$core$Task$andThen,
	function (time) {
		return $elm$core$Task$succeed(
			$elm$random$Random$initialSeed(
				$elm$time$Time$posixToMillis(time)));
	},
	$elm$time$Time$now);
var $elm$random$Random$step = F2(
	function (_v0, seed) {
		var generator = _v0.a;
		return generator(seed);
	});
var $elm$random$Random$onEffects = F3(
	function (router, commands, seed) {
		if (!commands.b) {
			return $elm$core$Task$succeed(seed);
		} else {
			var generator = commands.a.a;
			var rest = commands.b;
			var _v1 = A2($elm$random$Random$step, generator, seed);
			var value = _v1.a;
			var newSeed = _v1.b;
			return A2(
				$elm$core$Task$andThen,
				function (_v2) {
					return A3($elm$random$Random$onEffects, router, rest, newSeed);
				},
				A2($elm$core$Platform$sendToApp, router, value));
		}
	});
var $elm$random$Random$onSelfMsg = F3(
	function (_v0, _v1, seed) {
		return $elm$core$Task$succeed(seed);
	});
var $elm$random$Random$map = F2(
	function (func, _v0) {
		var genA = _v0.a;
		return $elm$random$Random$Generator(
			function (seed0) {
				var _v1 = genA(seed0);
				var a = _v1.a;
				var seed1 = _v1.b;
				return _Utils_Tuple2(
					func(a),
					seed1);
			});
	});
var $elm$random$Random$cmdMap = F2(
	function (func, _v0) {
		var generator = _v0.a;
		return $elm$random$Random$Generate(
			A2($elm$random$Random$map, func, generator));
	});
_Platform_effectManagers['Random'] = _Platform_createManager($elm$random$Random$init, $elm$random$Random$onEffects, $elm$random$Random$onSelfMsg, $elm$random$Random$cmdMap);
var $elm$random$Random$command = _Platform_leaf('Random');
var $elm$random$Random$generate = F2(
	function (tagger, generator) {
		return $elm$random$Random$command(
			$elm$random$Random$Generate(
				A2($elm$random$Random$map, tagger, generator)));
	});
var $author$project$DataSet$getNewData = function (dataName) {
	return $elm_community$list_extra$List$Extra$find(
		function (d) {
			return _Utils_eq(d.name, dataName);
		});
};
var $author$project$DataSet$getCounts = function (data) {
	return A2(
		$elm$core$List$map,
		function ($) {
			return $.count;
		},
		data.frequencies);
};
var $author$project$DataSet$getN = function (data) {
	return A3(
		$elm$core$List$foldl,
		$elm$core$Basics$add,
		0,
		$author$project$DataSet$getCounts(data));
};
var $elm$core$Maybe$map = F2(
	function (f, maybe) {
		if (maybe.$ === 'Just') {
			var value = maybe.a;
			return $elm$core$Maybe$Just(
				f(value));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	});
var $author$project$DataSet$getSuccess = F2(
	function (txt, data) {
		var n = $author$project$DataSet$getN(data);
		return A2(
			$elm$core$Maybe$map,
			function (labelFreq) {
				return {
					failureLbl: 'Not ' + function ($) {
						return $.label;
					}(labelFreq),
					n: n,
					numFailures: n - function ($) {
						return $.count;
					}(labelFreq),
					numSuccess: function ($) {
						return $.count;
					}(labelFreq),
					successLbl: function ($) {
						return $.label;
					}(labelFreq)
				};
			},
			A2(
				$elm_community$list_extra$List$Extra$find,
				function (p) {
					return _Utils_eq(
						function ($) {
							return $.label;
						}(p),
						txt);
				},
				data.frequencies));
	});
var $elm$core$List$filter = F2(
	function (isGood, list) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, xs) {
					return isGood(x) ? A2($elm$core$List$cons, x, xs) : xs;
				}),
			_List_Nil,
			list);
	});
var $author$project$ReadCSV$clearBlankRows = $elm$core$List$filter(
	function (s) {
		return $elm$core$String$length(s) > 0;
	});
var $author$project$ReadCSV$colIdx = function (rawheader) {
	return function (h) {
		return A2(
			$elm$core$Tuple$mapSecond,
			$elm$core$List$range(0),
			A2(
				$elm$core$Tuple$mapSecond,
				function (n) {
					return n - 1;
				},
				A2(
					$elm$core$Tuple$mapSecond,
					$elm$core$List$length,
					_Utils_Tuple2(h, h))));
	}(
		A2($elm$core$String$split, ',', rawheader));
};
var $elm$core$List$drop = F2(
	function (n, list) {
		drop:
		while (true) {
			if (n <= 0) {
				return list;
			} else {
				if (!list.b) {
					return list;
				} else {
					var x = list.a;
					var xs = list.b;
					var $temp$n = n - 1,
						$temp$list = xs;
					n = $temp$n;
					list = $temp$list;
					continue drop;
				}
			}
		}
	});
var $elm_community$list_extra$List$Extra$getAt = F2(
	function (idx, xs) {
		return (idx < 0) ? $elm$core$Maybe$Nothing : $elm$core$List$head(
			A2($elm$core$List$drop, idx, xs));
	});
var $author$project$ReadCSV$getCol = F2(
	function (rows, i) {
		return A2(
			$elm$core$List$map,
			A2(
				$elm$core$Basics$composeR,
				$elm_community$list_extra$List$Extra$getAt(i),
				$elm$core$Maybe$withDefault('')),
			rows);
	});
var $elm$core$Tuple$pair = F2(
	function (a, b) {
		return _Utils_Tuple2(a, b);
	});
var $elm_community$list_extra$List$Extra$zip = $elm$core$List$map2($elm$core$Tuple$pair);
var $author$project$ReadCSV$getCols = function (pair) {
	var _v0 = pair;
	var _v1 = _v0.a;
	var header = _v1.a;
	var idx = _v1.b;
	var rows = _v0.b;
	var cols = A2(
		$elm$core$List$map,
		$author$project$ReadCSV$getCol(rows),
		idx);
	return A2($elm_community$list_extra$List$Extra$zip, header, cols);
};
var $elm_community$list_extra$List$Extra$uncons = function (list) {
	if (!list.b) {
		return $elm$core$Maybe$Nothing;
	} else {
		var first = list.a;
		var rest = list.b;
		return $elm$core$Maybe$Just(
			_Utils_Tuple2(first, rest));
	}
};
var $author$project$ReadCSV$getHeadTail = function (rawfile) {
	var lines = A2(
		$elm$core$String$split,
		'\n',
		$elm$core$String$trim(rawfile));
	return $elm_community$list_extra$List$Extra$uncons(lines);
};
var $elm$core$Tuple$mapBoth = F3(
	function (funcA, funcB, _v0) {
		var x = _v0.a;
		var y = _v0.b;
		return _Utils_Tuple2(
			funcA(x),
			funcB(y));
	});
var $author$project$ReadCSV$splitBody = function (body) {
	return A2(
		$elm$core$List$map,
		$elm$core$String$split(','),
		body);
};
var $author$project$ReadCSV$getVariables = function (rawfile) {
	return A2(
		$elm$core$Maybe$map,
		$author$project$ReadCSV$getCols,
		A2(
			$elm$core$Maybe$map,
			A2($elm$core$Tuple$mapBoth, $author$project$ReadCSV$colIdx, $author$project$ReadCSV$splitBody),
			A2(
				$elm$core$Maybe$map,
				$elm$core$Tuple$mapSecond($author$project$ReadCSV$clearBlankRows),
				$author$project$ReadCSV$getHeadTail(rawfile))));
};
var $elm$random$Random$listHelp = F4(
	function (revList, n, gen, seed) {
		listHelp:
		while (true) {
			if (n < 1) {
				return _Utils_Tuple2(revList, seed);
			} else {
				var _v0 = gen(seed);
				var value = _v0.a;
				var newSeed = _v0.b;
				var $temp$revList = A2($elm$core$List$cons, value, revList),
					$temp$n = n - 1,
					$temp$gen = gen,
					$temp$seed = newSeed;
				revList = $temp$revList;
				n = $temp$n;
				gen = $temp$gen;
				seed = $temp$seed;
				continue listHelp;
			}
		}
	});
var $elm$random$Random$list = F2(
	function (n, _v0) {
		var gen = _v0.a;
		return $elm$random$Random$Generator(
			function (seed) {
				return A4($elm$random$Random$listHelp, _List_Nil, n, gen, seed);
			});
	});
var $elm$file$File$name = _File_name;
var $author$project$Main$samplePlotToJS = _Platform_outgoingPort('samplePlotToJS', $elm$core$Basics$identity);
var $author$project$Main$originalPlotCmd = A2(
	$author$project$Main$samplePlotCmd,
	$author$project$Main$samplePlotToJS,
	function ($) {
		return $.originalSample;
	});
var $author$project$DataSet$makeEmptySample = function (sample) {
	return _Utils_update(
		sample,
		{numFailures: 0, numSuccess: 0});
};
var $author$project$Main$resetBootstrap = F2(
	function (sample, model) {
		return _Utils_update(
			model,
			{
				bootstrapSample: A2($elm$core$Maybe$map, $author$project$DataSet$makeEmptySample, sample)
			});
	});
var $author$project$Main$resetTrials = function (model) {
	return _Utils_update(
		model,
		{level: $elm$core$Maybe$Nothing, tail: $author$project$Limits$None, tailLimit: $author$project$Limits$NoBounds, trials: 0, ys: $elm$core$Dict$empty});
};
var $rundis$elm_bootstrap$Bootstrap$Modal$Show = {$: 'Show'};
var $rundis$elm_bootstrap$Bootstrap$Modal$shown = $rundis$elm_bootstrap$Bootstrap$Modal$Show;
var $elm$file$File$toString = _File_toString;
var $author$project$Binomial$initBar = F3(
	function (a, i, p) {
		return {i: i, k: i, pi: p, v: (i + 1) * a};
	});
var $author$project$Binomial$initSqrHist = function (ps) {
	var n = $elm$core$List$length(ps);
	var ks = A2($elm$core$List$range, 0, n);
	var a = 1.0 / n;
	return A3(
		$elm$core$List$map2,
		$author$project$Binomial$initBar(a),
		ks,
		ps);
};
var $elm$core$Array$bitMask = 4294967295 >>> (32 - $elm$core$Array$shiftStep);
var $elm$core$Basics$ge = _Utils_ge;
var $elm$core$Elm$JsArray$unsafeGet = _JsArray_unsafeGet;
var $elm$core$Array$getHelp = F3(
	function (shift, index, tree) {
		getHelp:
		while (true) {
			var pos = $elm$core$Array$bitMask & (index >>> shift);
			var _v0 = A2($elm$core$Elm$JsArray$unsafeGet, pos, tree);
			if (_v0.$ === 'SubTree') {
				var subTree = _v0.a;
				var $temp$shift = shift - $elm$core$Array$shiftStep,
					$temp$index = index,
					$temp$tree = subTree;
				shift = $temp$shift;
				index = $temp$index;
				tree = $temp$tree;
				continue getHelp;
			} else {
				var values = _v0.a;
				return A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, values);
			}
		}
	});
var $elm$core$Bitwise$shiftLeftBy = _Bitwise_shiftLeftBy;
var $elm$core$Array$tailIndex = function (len) {
	return (len >>> 5) << 5;
};
var $elm$core$Array$get = F2(
	function (index, _v0) {
		var len = _v0.a;
		var startShift = _v0.b;
		var tree = _v0.c;
		var tail = _v0.d;
		return ((index < 0) || (_Utils_cmp(index, len) > -1)) ? $elm$core$Maybe$Nothing : ((_Utils_cmp(
			index,
			$elm$core$Array$tailIndex(len)) > -1) ? $elm$core$Maybe$Just(
			A2($elm$core$Elm$JsArray$unsafeGet, $elm$core$Array$bitMask & index, tail)) : $elm$core$Maybe$Just(
			A3($elm$core$Array$getHelp, startShift, index, tree)));
	});
var $author$project$Binomial$convertToSquareHistogram = F5(
	function (n, vs, ks, min, u) {
		var j = $elm$core$Basics$floor(u * n);
		var mk = A2($elm$core$Array$get, j, ks);
		var mv = A2($elm$core$Array$get, j, vs);
		var _v0 = _Utils_Tuple2(mv, mk);
		if (_v0.a.$ === 'Nothing') {
			var _v1 = _v0.a;
			return -1;
		} else {
			if (_v0.b.$ === 'Nothing') {
				var _v2 = _v0.b;
				return -1;
			} else {
				var v = _v0.a.a;
				var k = _v0.b.a;
				return (_Utils_cmp(u, v) < 0) ? (min + j) : (min + k);
			}
		}
	});
var $elm$core$Array$fromListHelp = F3(
	function (list, nodeList, nodeListSize) {
		fromListHelp:
		while (true) {
			var _v0 = A2($elm$core$Elm$JsArray$initializeFromList, $elm$core$Array$branchFactor, list);
			var jsArray = _v0.a;
			var remainingItems = _v0.b;
			if (_Utils_cmp(
				$elm$core$Elm$JsArray$length(jsArray),
				$elm$core$Array$branchFactor) < 0) {
				return A2(
					$elm$core$Array$builderToArray,
					true,
					{nodeList: nodeList, nodeListSize: nodeListSize, tail: jsArray});
			} else {
				var $temp$list = remainingItems,
					$temp$nodeList = A2(
					$elm$core$List$cons,
					$elm$core$Array$Leaf(jsArray),
					nodeList),
					$temp$nodeListSize = nodeListSize + 1;
				list = $temp$list;
				nodeList = $temp$nodeList;
				nodeListSize = $temp$nodeListSize;
				continue fromListHelp;
			}
		}
	});
var $elm$core$Array$fromList = function (list) {
	if (!list.b) {
		return $elm$core$Array$empty;
	} else {
		return A3($elm$core$Array$fromListHelp, list, _List_Nil, 0);
	}
};
var $author$project$Binomial$makeConvertToSquareHistogram = F2(
	function (min, bars) {
		var vs = $elm$core$Array$fromList(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.v;
				},
				bars));
		var n = $elm$core$List$length(bars);
		var ks = $elm$core$Array$fromList(
			A2(
				$elm$core$List$map,
				function ($) {
					return $.k;
				},
				bars));
		return A4($author$project$Binomial$convertToSquareHistogram, n, vs, ks, min);
	});
var $elm$core$List$sortBy = _List_sortBy;
var $author$project$Binomial$postProcBars = function (bars) {
	return A2(
		$elm$core$List$sortBy,
		function ($) {
			return $.i;
		},
		_Utils_ap(
			bars.under,
			_Utils_ap(bars.over, bars.full)));
};
var $author$project$Binomial$emptySortedBars = function (n) {
	return {a: 1.0 / n, full: _List_Nil, n: n, over: _List_Nil, under: _List_Nil};
};
var $author$project$Binomial$processBar = F2(
	function (bar, sortedBars) {
		var fillHeight = sortedBars.a;
		return (_Utils_cmp(bar.pi, fillHeight) < 0) ? _Utils_update(
			sortedBars,
			{
				under: A2($elm$core$List$cons, bar, sortedBars.under)
			}) : ((_Utils_cmp(bar.pi, fillHeight) > 0) ? _Utils_update(
			sortedBars,
			{
				over: A2($elm$core$List$cons, bar, sortedBars.over)
			}) : _Utils_update(
			sortedBars,
			{
				full: A2($elm$core$List$cons, bar, sortedBars.full)
			}));
	});
var $author$project$Binomial$sortBars = function (bars) {
	var n = $elm$core$List$length(bars);
	return A3(
		$elm$core$List$foldl,
		$author$project$Binomial$processBar,
		$author$project$Binomial$emptySortedBars(n),
		bars);
};
var $elm$core$List$sum = function (numbers) {
	return A3($elm$core$List$foldl, $elm$core$Basics$add, 0, numbers);
};
var $author$project$Binomial$binomLogCoef = F2(
	function (n, k) {
		var nF = n;
		var ks = A2(
			$elm$core$List$map,
			$elm$core$Basics$toFloat,
			A2($elm$core$List$range, 0, k - 1));
		var kF = k;
		var terms = A2(
			$elm$core$List$map,
			function (i) {
				return A2($elm$core$Basics$logBase, 10, nF - i) - A2($elm$core$Basics$logBase, 10, kF - i);
			},
			ks);
		return $elm$core$List$sum(terms);
	});
var $elm_community$list_extra$List$Extra$scanl = F3(
	function (f, b, xs) {
		var scan1 = F2(
			function (x, accAcc) {
				if (accAcc.b) {
					var acc = accAcc.a;
					return A2(
						$elm$core$List$cons,
						A2(f, x, acc),
						accAcc);
				} else {
					return _List_Nil;
				}
			});
		return $elm$core$List$reverse(
			A3(
				$elm$core$List$foldl,
				scan1,
				_List_fromArray(
					[b]),
				xs));
	});
var $author$project$Binomial$binomLogCoefRange = F3(
	function (n, start, stop) {
		var nums = A2(
			$elm$core$List$map,
			$elm$core$Basics$toFloat,
			A2($elm$core$List$range, start + 1, n));
		var logDiff = function (k) {
			return A2($elm$core$Basics$logBase, 10, (n - k) + 1) - A2($elm$core$Basics$logBase, 10, k);
		};
		var logDiffs = A2($elm$core$List$map, logDiff, nums);
		var coefStart = A2($author$project$Binomial$binomLogCoef, n, start);
		return A3($elm_community$list_extra$List$Extra$scanl, $elm$core$Basics$add, coefStart, logDiffs);
	});
var $elm$core$Basics$pow = _Basics_pow;
var $author$project$Binomial$probRange = F4(
	function (n, p, start, stop) {
		var xs = A2(
			$elm$core$List$map,
			$elm$core$Basics$toFloat,
			A2($elm$core$List$range, start, stop));
		var term = F2(
			function (logCoef, x) {
				return (logCoef + (x * A2($elm$core$Basics$logBase, 10, p))) + ((n - x) * A2($elm$core$Basics$logBase, 10, 1 - p));
			});
		var binomceof = A3($author$project$Binomial$binomLogCoefRange, n, start, stop);
		var logProbs = A3($elm$core$List$map2, term, binomceof, xs);
		return A2(
			$elm$core$List$map,
			function (logp) {
				return A2($elm$core$Basics$pow, 10, logp);
			},
			logProbs);
	});
var $author$project$Binomial$meanBinom = F2(
	function (n, p) {
		return n * p;
	});
var $elm$core$Basics$min = F2(
	function (x, y) {
		return (_Utils_cmp(x, y) < 0) ? x : y;
	});
var $elm$core$Basics$round = _Basics_round;
var $author$project$Binomial$sdBinom = F2(
	function (n, p) {
		return A2(
			$elm$core$Basics$pow,
			A2($author$project$Binomial$meanBinom, n, p) * (1 - p),
			0.5);
	});
var $author$project$Binomial$trimmedXRange = F3(
	function (trimAt, n, p) {
		var sd = A2($author$project$Binomial$sdBinom, n, p);
		var mean = A2($author$project$Binomial$meanBinom, n, p);
		var minX = A2(
			$elm$core$Basics$max,
			0,
			$elm$core$Basics$round(mean - (6 * sd)));
		var maxX = A2(
			$elm$core$Basics$min,
			n,
			$elm$core$Basics$round(mean + (6 * sd)));
		return (_Utils_cmp(n, trimAt) < 0) ? _Utils_Tuple2(0, n) : _Utils_Tuple2(minX, maxX);
	});
var $author$project$Binomial$trimmedProbs = F3(
	function (trimAt, n, p) {
		var _v0 = A3($author$project$Binomial$trimmedXRange, trimAt, n, p);
		var minX = _v0.a;
		var maxX = _v0.b;
		return A4($author$project$Binomial$probRange, n, p, minX, maxX);
	});
var $author$project$Binomial$updateBars = function (bars) {
	updateBars:
	while (true) {
		var small = A2(
			$elm$core$List$sortBy,
			function ($) {
				return $.pi;
			},
			bars.under);
		var big = $elm$core$List$reverse(
			A2(
				$elm$core$List$sortBy,
				function ($) {
					return $.pi;
				},
				bars.over));
		var _v0 = _Utils_Tuple2(small, big);
		if (!_v0.a.b) {
			return bars;
		} else {
			if (!_v0.b.b) {
				return bars;
			} else {
				var _v1 = _v0.a;
				var minBar = _v1.a;
				var restUnder = _v1.b;
				var _v2 = _v0.b;
				var maxBar = _v2.a;
				var restOver = _v2.b;
				var min = _Utils_update(
					minBar,
					{k: maxBar.i, pi: bars.a, v: (minBar.i * bars.a) + minBar.pi});
				var max = _Utils_update(
					maxBar,
					{pi: maxBar.pi - (bars.a - minBar.pi)});
				var full = A2($elm$core$List$cons, min, bars.full);
				if (_Utils_cmp(max.pi, bars.a) < 0) {
					var $temp$bars = _Utils_update(
						bars,
						{
							full: full,
							over: restOver,
							under: A2($elm$core$List$cons, max, restUnder)
						});
					bars = $temp$bars;
					continue updateBars;
				} else {
					if (_Utils_cmp(max.pi, bars.a) > 0) {
						var $temp$bars = _Utils_update(
							bars,
							{
								full: full,
								over: A2($elm$core$List$cons, max, restOver),
								under: restUnder
							});
						bars = $temp$bars;
						continue updateBars;
					} else {
						var $temp$bars = _Utils_update(
							bars,
							{
								full: A2($elm$core$List$cons, max, full),
								over: restOver,
								under: restUnder
							});
						bars = $temp$bars;
						continue updateBars;
					}
				}
			}
		}
	}
};
var $author$project$Binomial$getBinomGen = F2(
	function (n, p) {
		var ps = A3($author$project$Binomial$trimmedProbs, $author$project$Defaults$defaults.trimAt, n, p);
		var bars = $author$project$Binomial$postProcBars(
			$author$project$Binomial$updateBars(
				$author$project$Binomial$sortBars(
					$author$project$Binomial$initSqrHist(ps))));
		var _v0 = A3($author$project$Binomial$trimmedXRange, $author$project$Defaults$defaults.trimAt, n, p);
		var min = _v0.a;
		return A2($author$project$Binomial$makeConvertToSquareHistogram, min, bars);
	});
var $author$project$Main$updateBinomGen = function (sample) {
	var n = sample.n;
	var p = sample.numSuccess / n;
	return A2($author$project$Binomial$getBinomGen, n, p);
};
var $author$project$Main$updateBinom = F2(
	function (newSample, model) {
		return _Utils_update(
			model,
			{
				binomGen: A2($elm$core$Maybe$map, $author$project$Main$updateBinomGen, newSample)
			});
	});
var $gicentre$elm_vegalite$VegaLite$Circle = {$: 'Circle'};
var $gicentre$elm_vegalite$VegaLite$circle = $gicentre$elm_vegalite$VegaLite$mark($gicentre$elm_vegalite$VegaLite$Circle);
var $author$project$CountDict$updateTallest = F2(
	function (pair, currentMax) {
		var _v0 = pair;
		var newY = _v0.b;
		return A2($elm$core$Basics$max, newY, currentMax);
	});
var $author$project$CountDict$tallestBar = function (yDict) {
	return A3(
		$elm$core$List$foldl,
		$author$project$CountDict$updateTallest,
		0,
		$elm$core$Dict$toList(yDict));
};
var $author$project$DistPlot$getMaxHeight = function (countDict) {
	return A2(
		$elm$core$Basics$max,
		$author$project$Defaults$defaults.distMinHeight,
		$author$project$CountDict$tallestBar(countDict));
};
var $elm$core$String$fromFloat = _String_fromNumber;
var $author$project$DistPlot$getTailExpression = F2(
	function (mean, tailLimit) {
		switch (tailLimit.$) {
			case 'NoBounds':
				return 'false';
			case 'Lower':
				var l = tailLimit.a;
				return 'datum.X <= ' + $elm$core$String$fromFloat(l);
			case 'Upper':
				var u = tailLimit.a;
				return 'datum.X >= ' + $elm$core$String$fromFloat(u);
			default:
				var l = tailLimit.a;
				var u = tailLimit.b;
				return (_Utils_eq(mean, l) || _Utils_eq(mean, u)) ? 'true' : A2(
					$elm$core$String$join,
					' ',
					_List_fromArray(
						[
							'datum.X <=',
							$elm$core$String$fromFloat(l),
							'|| datum.X >=',
							$elm$core$String$fromFloat(u)
						]));
		}
	});
var $author$project$DistPlot$combineDistColumns = F2(
	function (pair, columns) {
		var _v0 = columns;
		var oldXs = _v0.a;
		var oldYs = _v0.b;
		var _v1 = pair;
		var newX = _v1.a;
		var newY = _v1.b;
		return _Utils_Tuple2(
			A2($elm$core$List$cons, newX, oldXs),
			A2($elm$core$List$cons, newY, oldYs));
	});
var $author$project$DistPlot$distColumns = function (yDict) {
	return A3(
		$elm$core$Tuple$mapBoth,
		$elm$core$List$map($elm$core$Basics$toFloat),
		$elm$core$List$map($elm$core$Basics$toFloat),
		A3(
			$elm$core$List$foldl,
			$author$project$DistPlot$combineDistColumns,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			$elm$core$Dict$toList(yDict)));
};
var $author$project$DistPlot$countPairToDots = function (pair) {
	var _v0 = pair;
	var x = _v0.a;
	var cnt = _v0.b;
	var ys = A2($elm$core$List$range, 1, cnt);
	var xs = A2($elm$core$List$repeat, cnt, x);
	return _Utils_Tuple2(xs, ys);
};
var $author$project$DistPlot$combineDotColumns = F2(
	function (nextPair, columns) {
		var _v0 = columns;
		var oldXs = _v0.a;
		var oldYs = _v0.b;
		var _v1 = $author$project$DistPlot$countPairToDots(nextPair);
		var newXs = _v1.a;
		var newYs = _v1.b;
		return _Utils_Tuple2(
			_Utils_ap(newXs, oldXs),
			_Utils_ap(newYs, oldYs));
	});
var $author$project$DistPlot$dotColumns = function (yDict) {
	var countList = $elm$core$Dict$toList(yDict);
	return A3(
		$elm$core$Tuple$mapBoth,
		$elm$core$List$map($elm$core$Basics$toFloat),
		$elm$core$List$map($elm$core$Basics$toFloat),
		A3(
			$elm$core$List$foldl,
			$author$project$DistPlot$combineDotColumns,
			_Utils_Tuple2(_List_Nil, _List_Nil),
			countList));
};
var $author$project$DistPlot$getXandY = F2(
	function (isLarge, countDict) {
		return isLarge ? $author$project$DistPlot$distColumns(countDict) : $author$project$DistPlot$dotColumns(countDict);
	});
var $elm$core$List$maximum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$max, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$core$List$minimum = function (list) {
	if (list.b) {
		var x = list.a;
		var xs = list.b;
		return $elm$core$Maybe$Just(
			A3($elm$core$List$foldl, $elm$core$Basics$min, x, xs));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $author$project$Main$updateDistPlotConfig = function (model) {
	var _v0 = model.originalSample;
	if (_v0.$ === 'Nothing') {
		return model;
	} else {
		var sample = _v0.a;
		var numSD = $author$project$Defaults$defaults.numSD;
		var n = sample.n;
		var p = sample.numSuccess / n;
		var sd = (p * (1 - p)) / n;
		var mean = n * p;
		var tailExpr = A2($author$project$DistPlot$getTailExpression, mean, model.tailLimit);
		var isLarge = _Utils_cmp(model.trials, $author$project$Defaults$defaults.largePlot) > 0;
		var _v1 = A2($author$project$DistPlot$getXandY, isLarge, model.ys);
		var xs = _v1.a;
		var ys = _v1.b;
		var largestX = A2(
			$elm$core$Maybe$withDefault,
			n,
			$elm$core$List$maximum(xs)) + 3;
		var ps = A2(
			$elm$core$List$map,
			function (x) {
				return x / n;
			},
			xs);
		var smallestX = A2(
			$elm$core$Maybe$withDefault,
			0,
			$elm$core$List$minimum(xs)) - 3;
		var config = $elm$core$Maybe$Just(
			{
				mark: isLarge ? $gicentre$elm_vegalite$VegaLite$bar : $gicentre$elm_vegalite$VegaLite$circle,
				maxX: A2($elm$core$Basics$min, 1, largestX / n),
				maxY: $author$project$DistPlot$getMaxHeight(model.ys),
				minX: A2($elm$core$Basics$max, 0, smallestX / n),
				tailExpression: tailExpr,
				xAxisTitle: 'Proportion of ' + sample.successLbl,
				xs: ps,
				ys: ys
			});
		return _Utils_update(
			model,
			{distPlotConfig: config});
	}
};
var $author$project$Main$apply = F2(
	function (f, x) {
		return f(x);
	});
var $elm_community$list_extra$List$Extra$last = function (items) {
	last:
	while (true) {
		if (!items.b) {
			return $elm$core$Maybe$Nothing;
		} else {
			if (!items.b.b) {
				var x = items.a;
				return $elm$core$Maybe$Just(x);
			} else {
				var rest = items.b;
				var $temp$items = rest;
				items = $temp$items;
				continue last;
			}
		}
	}
};
var $elm$core$Maybe$map2 = F3(
	function (func, ma, mb) {
		if (ma.$ === 'Nothing') {
			return $elm$core$Maybe$Nothing;
		} else {
			var a = ma.a;
			if (mb.$ === 'Nothing') {
				return $elm$core$Maybe$Nothing;
			} else {
				var b = mb.a;
				return $elm$core$Maybe$Just(
					A2(func, a, b));
			}
		}
	});
var $author$project$DataSet$updateSample = F2(
	function (x, sample) {
		return _Utils_update(
			sample,
			{numFailures: sample.n - x, numSuccess: x});
	});
var $author$project$Main$updateLast = F2(
	function (ws, model) {
		var lastX = A3(
			$elm$core$Maybe$map2,
			$author$project$Main$apply,
			model.binomGen,
			$elm_community$list_extra$List$Extra$last(ws));
		var newBootstrap = A3($elm$core$Maybe$map2, $author$project$DataSet$updateSample, lastX, model.bootstrapSample);
		return _Utils_update(
			model,
			{bootstrapSample: newBootstrap});
	});
var $author$project$Main$updateOriginal = F2(
	function (newSample, model) {
		return _Utils_update(
			model,
			{originalSample: newSample});
	});
var $author$project$DataSet$createDataFromFreq = F3(
	function (name, lbls, cnts) {
		return {
			counts: cnts,
			frequencies: A2(
				$elm$core$List$map,
				$author$project$DataSet$makeLabelFreq,
				A2($elm_community$list_extra$List$Extra$zip, lbls, cnts)),
			labels: lbls,
			name: name
		};
	});
var $elm_community$list_extra$List$Extra$groupWhile = F2(
	function (isSameGroup, items) {
		return A3(
			$elm$core$List$foldr,
			F2(
				function (x, acc) {
					if (!acc.b) {
						return _List_fromArray(
							[
								_Utils_Tuple2(x, _List_Nil)
							]);
					} else {
						var _v1 = acc.a;
						var y = _v1.a;
						var restOfGroup = _v1.b;
						var groups = acc.b;
						return A2(isSameGroup, x, y) ? A2(
							$elm$core$List$cons,
							_Utils_Tuple2(
								x,
								A2($elm$core$List$cons, y, restOfGroup)),
							groups) : A2(
							$elm$core$List$cons,
							_Utils_Tuple2(x, _List_Nil),
							acc);
					}
				}),
			_List_Nil,
			items);
	});
var $elm_community$list_extra$List$Extra$group = $elm_community$list_extra$List$Extra$groupWhile($elm$core$Basics$eq);
var $elm$core$List$sort = function (xs) {
	return A2($elm$core$List$sortBy, $elm$core$Basics$identity, xs);
};
var $author$project$DataSet$createDataFromRegular = F2(
	function (name, catcol) {
		var freqs = A2(
			$elm$core$List$map,
			$elm$core$Tuple$mapSecond(
				A2(
					$elm$core$Basics$composeR,
					$elm$core$List$length,
					$elm$core$Basics$add(1))),
			$elm_community$list_extra$List$Extra$group(
				$elm$core$List$sort(catcol)));
		return {
			counts: A2($elm$core$List$map, $elm$core$Tuple$second, freqs),
			frequencies: A2($elm$core$List$map, $author$project$DataSet$makeLabelFreq, freqs),
			labels: A2($elm$core$List$map, $elm$core$Tuple$first, freqs),
			name: name
		};
	});
var $author$project$Main$createDataFromUser = function (model) {
	var _v0 = _Utils_Tuple3(
		model.fileType,
		_Utils_Tuple2(model.selectedVariable, model.selectedData),
		model.counts);
	_v0$2:
	while (true) {
		if ((_v0.b.a.$ === 'Just') && (_v0.b.b.$ === 'Just')) {
			switch (_v0.a.$) {
				case 'Regular':
					var _v1 = _v0.a;
					var _v2 = _v0.b;
					var _var = _v2.a.a;
					var data = _v2.b.a;
					return $elm$core$Maybe$Just(
						A2($author$project$DataSet$createDataFromRegular, _var + ('--' + model.fileName), data));
				case 'Frequency':
					if (_v0.c.$ === 'Just') {
						var _v3 = _v0.a;
						var _v4 = _v0.b;
						var _var = _v4.a.a;
						var lbls = _v4.b.a;
						var cnts = _v0.c.a;
						return $elm$core$Maybe$Just(
							A3($author$project$DataSet$createDataFromFreq, _var + ('--' + model.fileName), lbls, cnts));
					} else {
						break _v0$2;
					}
				default:
					break _v0$2;
			}
		} else {
			break _v0$2;
		}
	}
	return $elm$core$Maybe$Nothing;
};
var $author$project$Main$updatePerspective = function (model) {
	return _Utils_update(
		model,
		{
			perspectiveData: $author$project$Main$createDataFromUser(model)
		});
};
var $author$project$Limits$Lower = function (a) {
	return {$: 'Lower', a: a};
};
var $author$project$Limits$TwoTail = F2(
	function (a, b) {
		return {$: 'TwoTail', a: a, b: b};
	});
var $author$project$Limits$Upper = function (a) {
	return {$: 'Upper', a: a};
};
var $author$project$Main$divideBy = F2(
	function (denom, numer) {
		return numer / denom;
	});
var $author$project$Main$getPercentiles = F3(
	function (n, trials, counts) {
		return function (el) {
			return A2(
				$elm_community$list_extra$List$Extra$zip,
				A2($elm$core$List$map, $elm$core$Tuple$first, el),
				A3(
					$elm_community$list_extra$List$Extra$scanl,
					$elm$core$Basics$add,
					0,
					A2($elm$core$List$map, $elm$core$Tuple$second, el)));
		}(
			A2(
				$elm$core$List$map,
				A2(
					$elm$core$Tuple$mapBoth,
					$author$project$Main$divideBy(n),
					$author$project$Main$divideBy(trials)),
				$elm$core$Dict$toList(counts)));
	});
var $elm_community$list_extra$List$Extra$takeWhile = function (predicate) {
	var takeWhileMemo = F2(
		function (memo, list) {
			takeWhileMemo:
			while (true) {
				if (!list.b) {
					return $elm$core$List$reverse(memo);
				} else {
					var x = list.a;
					var xs = list.b;
					if (predicate(x)) {
						var $temp$memo = A2($elm$core$List$cons, x, memo),
							$temp$list = xs;
						memo = $temp$memo;
						list = $temp$list;
						continue takeWhileMemo;
					} else {
						return $elm$core$List$reverse(memo);
					}
				}
			}
		});
	return takeWhileMemo(_List_Nil);
};
var $author$project$Main$leftTailBound = F2(
	function (tailArea, percentiles) {
		return A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$first,
			$elm_community$list_extra$List$Extra$last(
				A2(
					$elm_community$list_extra$List$Extra$takeWhile,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$second,
						$elm$core$Basics$gt(tailArea)),
					percentiles)));
	});
var $elm_community$list_extra$List$Extra$dropWhileRight = function (p) {
	return A2(
		$elm$core$List$foldr,
		F2(
			function (x, xs) {
				return (p(x) && $elm$core$List$isEmpty(xs)) ? _List_Nil : A2($elm$core$List$cons, x, xs);
			}),
		_List_Nil);
};
var $author$project$Main$rightTailBound = F2(
	function (tailArea, percentiles) {
		var oneMinus = 1 - tailArea;
		return A2(
			$elm$core$Maybe$map,
			$elm$core$Tuple$first,
			$elm_community$list_extra$List$Extra$last(
				A2(
					$elm_community$list_extra$List$Extra$dropWhileRight,
					A2(
						$elm$core$Basics$composeR,
						$elm$core$Tuple$second,
						$elm$core$Basics$lt(oneMinus)),
					percentiles)));
	});
var $author$project$Main$getBound = function (model) {
	var _v0 = _Utils_Tuple3(model.originalSample, model.tail, model.level);
	_v0$3:
	while (true) {
		if ((_v0.a.$ === 'Just') && (_v0.c.$ === 'Just')) {
			switch (_v0.b.$) {
				case 'Left':
					var sample = _v0.a.a;
					var _v1 = _v0.b;
					var level = _v0.c.a;
					var tailArea = 1 - level;
					return A2(
						$elm$core$Maybe$withDefault,
						$author$project$Limits$NoBounds,
						A2(
							$elm$core$Maybe$map,
							$author$project$Limits$Lower,
							A2(
								$author$project$Main$leftTailBound,
								tailArea,
								A3($author$project$Main$getPercentiles, sample.n, model.trials, model.ys))));
				case 'Right':
					var sample = _v0.a.a;
					var _v2 = _v0.b;
					var level = _v0.c.a;
					var tailArea = 1 - level;
					return A2(
						$elm$core$Maybe$withDefault,
						$author$project$Limits$NoBounds,
						A2(
							$elm$core$Maybe$map,
							$author$project$Limits$Upper,
							A2(
								$author$project$Main$rightTailBound,
								tailArea,
								A3($author$project$Main$getPercentiles, sample.n, model.trials, model.ys))));
				case 'Two':
					var sample = _v0.a.a;
					var _v3 = _v0.b;
					var level = _v0.c.a;
					var tailArea = (1 - level) / 2;
					var percentiles = A3($author$project$Main$getPercentiles, sample.n, model.trials, model.ys);
					var right = A2($author$project$Main$rightTailBound, tailArea, percentiles);
					var left = A2($author$project$Main$leftTailBound, tailArea, percentiles);
					return A2(
						$elm$core$Maybe$withDefault,
						$author$project$Limits$NoBounds,
						A3($elm$core$Maybe$map2, $author$project$Limits$TwoTail, left, right));
				default:
					break _v0$3;
			}
		} else {
			break _v0$3;
		}
	}
	return $author$project$Limits$NoBounds;
};
var $author$project$Main$updateTailBounds = function (model) {
	return _Utils_update(
		model,
		{
			tailLimit: $author$project$Main$getBound(model)
		});
};
var $elm$core$Dict$get = F2(
	function (targetKey, dict) {
		get:
		while (true) {
			if (dict.$ === 'RBEmpty_elm_builtin') {
				return $elm$core$Maybe$Nothing;
			} else {
				var key = dict.b;
				var value = dict.c;
				var left = dict.d;
				var right = dict.e;
				var _v1 = A2($elm$core$Basics$compare, targetKey, key);
				switch (_v1.$) {
					case 'LT':
						var $temp$targetKey = targetKey,
							$temp$dict = left;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
					case 'EQ':
						return $elm$core$Maybe$Just(value);
					default:
						var $temp$targetKey = targetKey,
							$temp$dict = right;
						targetKey = $temp$targetKey;
						dict = $temp$dict;
						continue get;
				}
			}
		}
	});
var $elm$core$Dict$getMin = function (dict) {
	getMin:
	while (true) {
		if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
			var left = dict.d;
			var $temp$dict = left;
			dict = $temp$dict;
			continue getMin;
		} else {
			return dict;
		}
	}
};
var $elm$core$Dict$moveRedLeft = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.e.d.$ === 'RBNode_elm_builtin') && (dict.e.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var lLeft = _v1.d;
			var lRight = _v1.e;
			var _v2 = dict.e;
			var rClr = _v2.a;
			var rK = _v2.b;
			var rV = _v2.c;
			var rLeft = _v2.d;
			var _v3 = rLeft.a;
			var rlK = rLeft.b;
			var rlV = rLeft.c;
			var rlL = rLeft.d;
			var rlR = rLeft.e;
			var rRight = _v2.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				rlK,
				rlV,
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					rlL),
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, rK, rV, rlR, rRight));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v4 = dict.d;
			var lClr = _v4.a;
			var lK = _v4.b;
			var lV = _v4.c;
			var lLeft = _v4.d;
			var lRight = _v4.e;
			var _v5 = dict.e;
			var rClr = _v5.a;
			var rK = _v5.b;
			var rV = _v5.c;
			var rLeft = _v5.d;
			var rRight = _v5.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$moveRedRight = function (dict) {
	if (((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) && (dict.e.$ === 'RBNode_elm_builtin')) {
		if ((dict.d.d.$ === 'RBNode_elm_builtin') && (dict.d.d.a.$ === 'Red')) {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v1 = dict.d;
			var lClr = _v1.a;
			var lK = _v1.b;
			var lV = _v1.c;
			var _v2 = _v1.d;
			var _v3 = _v2.a;
			var llK = _v2.b;
			var llV = _v2.c;
			var llLeft = _v2.d;
			var llRight = _v2.e;
			var lRight = _v1.e;
			var _v4 = dict.e;
			var rClr = _v4.a;
			var rK = _v4.b;
			var rV = _v4.c;
			var rLeft = _v4.d;
			var rRight = _v4.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				$elm$core$Dict$Red,
				lK,
				lV,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, llK, llV, llLeft, llRight),
				A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					lRight,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight)));
		} else {
			var clr = dict.a;
			var k = dict.b;
			var v = dict.c;
			var _v5 = dict.d;
			var lClr = _v5.a;
			var lK = _v5.b;
			var lV = _v5.c;
			var lLeft = _v5.d;
			var lRight = _v5.e;
			var _v6 = dict.e;
			var rClr = _v6.a;
			var rK = _v6.b;
			var rV = _v6.c;
			var rLeft = _v6.d;
			var rRight = _v6.e;
			if (clr.$ === 'Black') {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			} else {
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					$elm$core$Dict$Black,
					k,
					v,
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, lK, lV, lLeft, lRight),
					A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, rK, rV, rLeft, rRight));
			}
		}
	} else {
		return dict;
	}
};
var $elm$core$Dict$removeHelpPrepEQGT = F7(
	function (targetKey, dict, color, key, value, left, right) {
		if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Red')) {
			var _v1 = left.a;
			var lK = left.b;
			var lV = left.c;
			var lLeft = left.d;
			var lRight = left.e;
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				lK,
				lV,
				lLeft,
				A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Red, key, value, lRight, right));
		} else {
			_v2$2:
			while (true) {
				if ((right.$ === 'RBNode_elm_builtin') && (right.a.$ === 'Black')) {
					if (right.d.$ === 'RBNode_elm_builtin') {
						if (right.d.a.$ === 'Black') {
							var _v3 = right.a;
							var _v4 = right.d;
							var _v5 = _v4.a;
							return $elm$core$Dict$moveRedRight(dict);
						} else {
							break _v2$2;
						}
					} else {
						var _v6 = right.a;
						var _v7 = right.d;
						return $elm$core$Dict$moveRedRight(dict);
					}
				} else {
					break _v2$2;
				}
			}
			return dict;
		}
	});
var $elm$core$Dict$removeMin = function (dict) {
	if ((dict.$ === 'RBNode_elm_builtin') && (dict.d.$ === 'RBNode_elm_builtin')) {
		var color = dict.a;
		var key = dict.b;
		var value = dict.c;
		var left = dict.d;
		var lColor = left.a;
		var lLeft = left.d;
		var right = dict.e;
		if (lColor.$ === 'Black') {
			if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
				var _v3 = lLeft.a;
				return A5(
					$elm$core$Dict$RBNode_elm_builtin,
					color,
					key,
					value,
					$elm$core$Dict$removeMin(left),
					right);
			} else {
				var _v4 = $elm$core$Dict$moveRedLeft(dict);
				if (_v4.$ === 'RBNode_elm_builtin') {
					var nColor = _v4.a;
					var nKey = _v4.b;
					var nValue = _v4.c;
					var nLeft = _v4.d;
					var nRight = _v4.e;
					return A5(
						$elm$core$Dict$balance,
						nColor,
						nKey,
						nValue,
						$elm$core$Dict$removeMin(nLeft),
						nRight);
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			}
		} else {
			return A5(
				$elm$core$Dict$RBNode_elm_builtin,
				color,
				key,
				value,
				$elm$core$Dict$removeMin(left),
				right);
		}
	} else {
		return $elm$core$Dict$RBEmpty_elm_builtin;
	}
};
var $elm$core$Dict$removeHelp = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBEmpty_elm_builtin') {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		} else {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_cmp(targetKey, key) < 0) {
				if ((left.$ === 'RBNode_elm_builtin') && (left.a.$ === 'Black')) {
					var _v4 = left.a;
					var lLeft = left.d;
					if ((lLeft.$ === 'RBNode_elm_builtin') && (lLeft.a.$ === 'Red')) {
						var _v6 = lLeft.a;
						return A5(
							$elm$core$Dict$RBNode_elm_builtin,
							color,
							key,
							value,
							A2($elm$core$Dict$removeHelp, targetKey, left),
							right);
					} else {
						var _v7 = $elm$core$Dict$moveRedLeft(dict);
						if (_v7.$ === 'RBNode_elm_builtin') {
							var nColor = _v7.a;
							var nKey = _v7.b;
							var nValue = _v7.c;
							var nLeft = _v7.d;
							var nRight = _v7.e;
							return A5(
								$elm$core$Dict$balance,
								nColor,
								nKey,
								nValue,
								A2($elm$core$Dict$removeHelp, targetKey, nLeft),
								nRight);
						} else {
							return $elm$core$Dict$RBEmpty_elm_builtin;
						}
					}
				} else {
					return A5(
						$elm$core$Dict$RBNode_elm_builtin,
						color,
						key,
						value,
						A2($elm$core$Dict$removeHelp, targetKey, left),
						right);
				}
			} else {
				return A2(
					$elm$core$Dict$removeHelpEQGT,
					targetKey,
					A7($elm$core$Dict$removeHelpPrepEQGT, targetKey, dict, color, key, value, left, right));
			}
		}
	});
var $elm$core$Dict$removeHelpEQGT = F2(
	function (targetKey, dict) {
		if (dict.$ === 'RBNode_elm_builtin') {
			var color = dict.a;
			var key = dict.b;
			var value = dict.c;
			var left = dict.d;
			var right = dict.e;
			if (_Utils_eq(targetKey, key)) {
				var _v1 = $elm$core$Dict$getMin(right);
				if (_v1.$ === 'RBNode_elm_builtin') {
					var minKey = _v1.b;
					var minValue = _v1.c;
					return A5(
						$elm$core$Dict$balance,
						color,
						minKey,
						minValue,
						left,
						$elm$core$Dict$removeMin(right));
				} else {
					return $elm$core$Dict$RBEmpty_elm_builtin;
				}
			} else {
				return A5(
					$elm$core$Dict$balance,
					color,
					key,
					value,
					left,
					A2($elm$core$Dict$removeHelp, targetKey, right));
			}
		} else {
			return $elm$core$Dict$RBEmpty_elm_builtin;
		}
	});
var $elm$core$Dict$remove = F2(
	function (key, dict) {
		var _v0 = A2($elm$core$Dict$removeHelp, key, dict);
		if ((_v0.$ === 'RBNode_elm_builtin') && (_v0.a.$ === 'Red')) {
			var _v1 = _v0.a;
			var k = _v0.b;
			var v = _v0.c;
			var l = _v0.d;
			var r = _v0.e;
			return A5($elm$core$Dict$RBNode_elm_builtin, $elm$core$Dict$Black, k, v, l, r);
		} else {
			var x = _v0;
			return x;
		}
	});
var $elm$core$Dict$update = F3(
	function (targetKey, alter, dictionary) {
		var _v0 = alter(
			A2($elm$core$Dict$get, targetKey, dictionary));
		if (_v0.$ === 'Just') {
			var value = _v0.a;
			return A3($elm$core$Dict$insert, targetKey, value, dictionary);
		} else {
			return A2($elm$core$Dict$remove, targetKey, dictionary);
		}
	});
var $author$project$CountDict$updateCount = function (maybeN) {
	if (maybeN.$ === 'Just') {
		var n = maybeN.a;
		return $elm$core$Maybe$Just(n + 1);
	} else {
		return $elm$core$Maybe$Just(1);
	}
};
var $author$project$CountDict$updateY = F2(
	function (x, ys) {
		return A3($elm$core$Dict$update, x, $author$project$CountDict$updateCount, ys);
	});
var $author$project$CountDict$updateCountDict = F3(
	function (binomGen, cnts, outcomes) {
		return A3(
			$elm$core$List$foldl,
			$author$project$CountDict$updateY,
			cnts,
			A2($elm$core$List$map, binomGen, outcomes));
	});
var $author$project$Main$updateYs = F2(
	function (ws, model) {
		var _v0 = function () {
			var _v1 = model.binomGen;
			if (_v1.$ === 'Nothing') {
				return _Utils_Tuple2(model.ys, model.trials);
			} else {
				var binomGen = _v1.a;
				return _Utils_Tuple2(
					A3($author$project$CountDict$updateCountDict, binomGen, model.ys, ws),
					model.trials + $elm$core$List$length(ws));
			}
		}();
		var newYs = _v0.a;
		var newTrials = _v0.b;
		return _Utils_update(
			model,
			{trials: newTrials, ys: newYs});
	});
var $author$project$Main$update = F2(
	function (msg, model) {
		switch (msg.$) {
			case 'CsvRequested':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{counts: $elm$core$Maybe$Nothing, fileName: 'None', fileType: $author$project$Main$NoType, selectedData: $elm$core$Maybe$Nothing, selectedVariable: $elm$core$Maybe$Nothing, variables: $elm$core$Maybe$Nothing}),
					A2(
						$elm$file$File$Select$file,
						_List_fromArray(
							['text/csv']),
						$author$project$Main$CsvSelected));
			case 'CsvSelected':
				var file = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							fileName: $elm$file$File$name(file)
						}),
					A2(
						$elm$core$Task$perform,
						$author$project$Main$CsvLoaded,
						$elm$file$File$toString(file)));
			case 'CsvLoaded':
				var content = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{
							csv: $elm$core$Maybe$Just(content),
							variables: $author$project$ReadCSV$getVariables(content)
						}),
					$elm$core$Platform$Cmd$none);
			case 'DataDropMsg':
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{dataDropState: state}),
					$elm$core$Platform$Cmd$none);
			case 'SuccessDropMsg':
				var state = msg.a;
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{successDropState: state}),
					$elm$core$Platform$Cmd$none);
			case 'ChangeData':
				var name = msg.a;
				var data = A2($author$project$DataSet$getNewData, name, model.datasets);
				var finalModel = $author$project$Main$resetTrials(
					_Utils_update(
						model,
						{originalSample: $elm$core$Maybe$Nothing, selected: data}));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			case 'ChangeSuccess':
				var name = msg.a;
				var newSample = A2(
					$elm$core$Maybe$andThen,
					$author$project$DataSet$getSuccess(name),
					model.selected);
				var finalModel = $author$project$Main$updateTailBounds(
					$author$project$Main$updateDistPlotConfig(
						A2(
							$author$project$Main$updateBinom,
							newSample,
							A2(
								$author$project$Main$resetBootstrap,
								newSample,
								A2($author$project$Main$updateOriginal, newSample, model)))));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			case 'CloseModal':
				var finalModel = _Utils_update(
					model,
					{modalVisibility: $rundis$elm_bootstrap$Bootstrap$Modal$hidden});
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			case 'ShowModal':
				return _Utils_Tuple2(
					_Utils_update(
						model,
						{modalVisibility: $rundis$elm_bootstrap$Bootstrap$Modal$shown}),
					$elm$core$Platform$Cmd$none);
			case 'ChangeFileType':
				var s = msg.a;
				var ftype = function () {
					switch (s) {
						case 'reg':
							return $author$project$Main$Regular;
						case 'freq':
							return $author$project$Main$Frequency;
						default:
							return $author$project$Main$NoType;
					}
				}();
				return _Utils_Tuple2(
					$author$project$Main$resetTrials(
						_Utils_update(
							model,
							{fileType: ftype})),
					$elm$core$Platform$Cmd$none);
			case 'SelectVariable':
				var lbl = msg.a;
				var _v2 = function () {
					var _v3 = model.variables;
					if (_v3.$ === 'Nothing') {
						return _Utils_Tuple2($elm$core$Maybe$Nothing, $elm$core$Maybe$Nothing);
					} else {
						var vars = _v3.a;
						return A2(
							$elm$core$Tuple$mapSecond,
							$elm$core$Maybe$map($elm$core$Tuple$second),
							A2(
								$elm$core$Tuple$mapSecond,
								$elm_community$list_extra$List$Extra$find(
									function (p) {
										return _Utils_eq(p.a, lbl);
									}),
								_Utils_Tuple2(
									$elm$core$Maybe$Just(lbl),
									vars)));
					}
				}();
				var _var = _v2.a;
				var col = _v2.b;
				return _Utils_Tuple2(
					$author$project$Main$updatePerspective(
						_Utils_update(
							model,
							{selectedData: col, selectedVariable: _var})),
					$elm$core$Platform$Cmd$none);
			case 'SelectCount':
				var lbl = msg.a;
				var rawcol = function () {
					var _v4 = model.variables;
					if (_v4.$ === 'Nothing') {
						return $elm$core$Maybe$Nothing;
					} else {
						var vars = _v4.a;
						return A2(
							$elm$core$Maybe$map,
							$elm$core$Tuple$second,
							A2(
								$elm_community$list_extra$List$Extra$find,
								function (p) {
									return _Utils_eq(p.a, lbl);
								},
								vars));
					}
				}();
				var counts = A2(
					$elm$core$Maybe$map,
					$elm$core$List$map(
						A2(
							$elm$core$Basics$composeR,
							$elm$core$String$toInt,
							$elm$core$Maybe$withDefault(0))),
					rawcol);
				return _Utils_Tuple2(
					$author$project$Main$updatePerspective(
						_Utils_update(
							model,
							{counts: counts})),
					$elm$core$Platform$Cmd$none);
			case 'LoadUserData':
				var _v5 = model.perspectiveData;
				if (_v5.$ === 'Nothing') {
					return _Utils_Tuple2(model, $elm$core$Platform$Cmd$none);
				} else {
					var data = _v5.a;
					var finalModel = $author$project$Main$resetTrials(
						_Utils_update(
							model,
							{
								datasets: A2($elm$core$List$cons, data, model.datasets),
								modalVisibility: $rundis$elm_bootstrap$Bootstrap$Modal$hidden,
								originalSample: $elm$core$Maybe$Nothing,
								selected: $elm$core$Maybe$Just(data)
							}));
					return _Utils_Tuple2(
						finalModel,
						$elm$core$Platform$Cmd$batch(
							_List_fromArray(
								[
									$author$project$Main$originalPlotCmd(finalModel),
									$author$project$Main$bootstrapPlotCmd(finalModel),
									$author$project$Main$distPlotCmd(finalModel)
								])));
				}
			case 'Collect':
				var n = msg.a;
				return _Utils_Tuple2(
					model,
					A2(
						$elm$random$Random$generate,
						$author$project$Main$NewStatistics,
						A2(
							$elm$random$Random$list,
							n,
							A2($elm$random$Random$float, 0, 1))));
			case 'Reset':
				var finalModel = $author$project$Main$updateDistPlotConfig(
					$author$project$Main$resetTrials(
						A2($author$project$Main$resetBootstrap, model.originalSample, model)));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			case 'NewStatistics':
				var ws = msg.a;
				var finalModel = $author$project$Main$updateTailBounds(
					$author$project$Main$updateDistPlotConfig(
						A2(
							$author$project$Main$updateLast,
							ws,
							A2($author$project$Main$updateYs, ws, model))));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			case 'ChangeTail':
				var tail = msg.a;
				var finalModel = $author$project$Main$updateDistPlotConfig(
					$author$project$Main$updateTailBounds(
						_Utils_update(
							model,
							{tail: tail})));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
			default:
				var level = msg.a;
				var finalModel = $author$project$Main$updateDistPlotConfig(
					$author$project$Main$updateTailBounds(
						_Utils_update(
							model,
							{
								level: $elm$core$Maybe$Just(level)
							})));
				return _Utils_Tuple2(
					finalModel,
					$elm$core$Platform$Cmd$batch(
						_List_fromArray(
							[
								$author$project$Main$originalPlotCmd(finalModel),
								$author$project$Main$bootstrapPlotCmd(finalModel),
								$author$project$Main$distPlotCmd(finalModel)
							])));
		}
	});
var $elm$html$Html$div = _VirtualDom_node('div');
var $elm$html$Html$Attributes$stringProperty = F2(
	function (key, string) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$string(string));
	});
var $elm$html$Html$Attributes$id = $elm$html$Html$Attributes$stringProperty('id');
var $rundis$elm_bootstrap$Bootstrap$Grid$Column = function (a) {
	return {$: 'Column', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$col = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Grid$Column(
			{children: children, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$applyModifier = F2(
	function (modifier, options) {
		var value = modifier.a;
		return _Utils_update(
			options,
			{
				attributes: _Utils_ap(options.attributes, value)
			});
	});
var $elm$html$Html$Attributes$class = $elm$html$Html$Attributes$stringProperty('className');
var $rundis$elm_bootstrap$Bootstrap$Form$defaultOptions = {attributes: _List_Nil};
var $rundis$elm_bootstrap$Bootstrap$Form$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('form-group')
			]),
		options.attributes);
};
var $rundis$elm_bootstrap$Bootstrap$Form$group = F2(
	function (options, children) {
		return A2(
			$elm$html$Html$div,
			$rundis$elm_bootstrap$Bootstrap$Form$toAttributes(options),
			children);
	});
var $elm$html$Html$h4 = _VirtualDom_node('h4');
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col = {$: 'Col'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width = F2(
	function (screenSize, columnCount) {
		return {columnCount: columnCount, screenSize: screenSize};
	});
var $rundis$elm_bootstrap$Bootstrap$General$Internal$XS = {$: 'XS'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign = F2(
	function (align_, options) {
		var _v0 = align_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						alignXs: $elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						alignSm: $elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						alignMd: $elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						alignLg: $elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						alignXl: $elm$core$Maybe$Just(align_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset = F2(
	function (offset_, options) {
		var _v0 = offset_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						offsetXs: $elm$core$Maybe$Just(offset_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						offsetSm: $elm$core$Maybe$Just(offset_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						offsetMd: $elm$core$Maybe$Just(offset_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						offsetLg: $elm$core$Maybe$Just(offset_)
					});
			default:
				return _Utils_update(
					options,
					{
						offsetXl: $elm$core$Maybe$Just(offset_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder = F2(
	function (order_, options) {
		var _v0 = order_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						orderXs: $elm$core$Maybe$Just(order_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						orderSm: $elm$core$Maybe$Just(order_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						orderMd: $elm$core$Maybe$Just(order_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						orderLg: $elm$core$Maybe$Just(order_)
					});
			default:
				return _Utils_update(
					options,
					{
						orderXl: $elm$core$Maybe$Just(order_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull = F2(
	function (pull_, options) {
		var _v0 = pull_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pullXs: $elm$core$Maybe$Just(pull_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pullSm: $elm$core$Maybe$Just(pull_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pullMd: $elm$core$Maybe$Just(pull_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pullLg: $elm$core$Maybe$Just(pull_)
					});
			default:
				return _Utils_update(
					options,
					{
						pullXl: $elm$core$Maybe$Just(pull_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush = F2(
	function (push_, options) {
		var _v0 = push_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						pushXs: $elm$core$Maybe$Just(push_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						pushSm: $elm$core$Maybe$Just(push_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						pushMd: $elm$core$Maybe$Just(push_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						pushLg: $elm$core$Maybe$Just(push_)
					});
			default:
				return _Utils_update(
					options,
					{
						pushXl: $elm$core$Maybe$Just(push_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth = F2(
	function (width_, options) {
		var _v0 = width_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						widthXs: $elm$core$Maybe$Just(width_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						widthSm: $elm$core$Maybe$Just(width_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						widthMd: $elm$core$Maybe$Just(width_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						widthLg: $elm$core$Maybe$Just(width_)
					});
			default:
				return _Utils_update(
					options,
					{
						widthXl: $elm$core$Maybe$Just(width_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'ColAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'ColWidth':
				var width_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColWidth, width_, options);
			case 'ColOffset':
				var offset_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOffset, offset_, options);
			case 'ColPull':
				var pull_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPull, pull_, options);
			case 'ColPush':
				var push_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColPush, push_, options);
			case 'ColOrder':
				var order_ = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOrder, order_, options);
			case 'ColAlign':
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColAlign, align, options);
			default:
				var align = modifier.a;
				return _Utils_update(
					options,
					{
						textAlign: $elm$core$Maybe$Just(align)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption = function (size) {
	switch (size.$) {
		case 'Col':
			return $elm$core$Maybe$Nothing;
		case 'Col1':
			return $elm$core$Maybe$Just('1');
		case 'Col2':
			return $elm$core$Maybe$Just('2');
		case 'Col3':
			return $elm$core$Maybe$Just('3');
		case 'Col4':
			return $elm$core$Maybe$Just('4');
		case 'Col5':
			return $elm$core$Maybe$Just('5');
		case 'Col6':
			return $elm$core$Maybe$Just('6');
		case 'Col7':
			return $elm$core$Maybe$Just('7');
		case 'Col8':
			return $elm$core$Maybe$Just('8');
		case 'Col9':
			return $elm$core$Maybe$Just('9');
		case 'Col10':
			return $elm$core$Maybe$Just('10');
		case 'Col11':
			return $elm$core$Maybe$Just('11');
		case 'Col12':
			return $elm$core$Maybe$Just('12');
		default:
			return $elm$core$Maybe$Just('auto');
	}
};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption = function (size) {
	switch (size.$) {
		case 'XS':
			return $elm$core$Maybe$Nothing;
		case 'SM':
			return $elm$core$Maybe$Just('sm');
		case 'MD':
			return $elm$core$Maybe$Just('md');
		case 'LG':
			return $elm$core$Maybe$Just('lg');
		default:
			return $elm$core$Maybe$Just('xl');
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass = function (_v0) {
	var screenSize = _v0.screenSize;
	var columnCount = _v0.columnCount;
	return $elm$html$Html$Attributes$class(
		'col' + (A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return '-' + v;
				},
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$columnCountOption(columnCount)))));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes = function (widths) {
	var width_ = function (w) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthClass, w);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, width_, widths));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions = {alignLg: $elm$core$Maybe$Nothing, alignMd: $elm$core$Maybe$Nothing, alignSm: $elm$core$Maybe$Nothing, alignXl: $elm$core$Maybe$Nothing, alignXs: $elm$core$Maybe$Nothing, attributes: _List_Nil, offsetLg: $elm$core$Maybe$Nothing, offsetMd: $elm$core$Maybe$Nothing, offsetSm: $elm$core$Maybe$Nothing, offsetXl: $elm$core$Maybe$Nothing, offsetXs: $elm$core$Maybe$Nothing, orderLg: $elm$core$Maybe$Nothing, orderMd: $elm$core$Maybe$Nothing, orderSm: $elm$core$Maybe$Nothing, orderXl: $elm$core$Maybe$Nothing, orderXs: $elm$core$Maybe$Nothing, pullLg: $elm$core$Maybe$Nothing, pullMd: $elm$core$Maybe$Nothing, pullSm: $elm$core$Maybe$Nothing, pullXl: $elm$core$Maybe$Nothing, pullXs: $elm$core$Maybe$Nothing, pushLg: $elm$core$Maybe$Nothing, pushMd: $elm$core$Maybe$Nothing, pushSm: $elm$core$Maybe$Nothing, pushXl: $elm$core$Maybe$Nothing, pushXs: $elm$core$Maybe$Nothing, textAlign: $elm$core$Maybe$Nothing, widthLg: $elm$core$Maybe$Nothing, widthMd: $elm$core$Maybe$Nothing, widthSm: $elm$core$Maybe$Nothing, widthXl: $elm$core$Maybe$Nothing, widthXs: $elm$core$Maybe$Nothing};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption = function (size) {
	switch (size.$) {
		case 'Offset0':
			return '0';
		case 'Offset1':
			return '1';
		case 'Offset2':
			return '2';
		case 'Offset3':
			return '3';
		case 'Offset4':
			return '4';
		case 'Offset5':
			return '5';
		case 'Offset6':
			return '6';
		case 'Offset7':
			return '7';
		case 'Offset8':
			return '8';
		case 'Offset9':
			return '9';
		case 'Offset10':
			return '10';
		default:
			return '11';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString = function (screenSize) {
	var _v0 = $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize);
	if (_v0.$ === 'Just') {
		var s = _v0.a;
		return '-' + (s + '-');
	} else {
		return '-';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass = function (_v0) {
	var screenSize = _v0.screenSize;
	var offsetCount = _v0.offsetCount;
	return $elm$html$Html$Attributes$class(
		'offset' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetCountOption(offsetCount)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes = function (offsets) {
	var offset_ = function (m) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetClass, m);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, offset_, offsets));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption = function (size) {
	switch (size.$) {
		case 'OrderFirst':
			return 'first';
		case 'Order1':
			return '1';
		case 'Order2':
			return '2';
		case 'Order3':
			return '3';
		case 'Order4':
			return '4';
		case 'Order5':
			return '5';
		case 'Order6':
			return '6';
		case 'Order7':
			return '7';
		case 'Order8':
			return '8';
		case 'Order9':
			return '9';
		case 'Order10':
			return '10';
		case 'Order11':
			return '11';
		case 'Order12':
			return '12';
		default:
			return 'last';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes = function (orders) {
	var order_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'order' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderColOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, order_, orders));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption = function (size) {
	switch (size.$) {
		case 'Move0':
			return '0';
		case 'Move1':
			return '1';
		case 'Move2':
			return '2';
		case 'Move3':
			return '3';
		case 'Move4':
			return '4';
		case 'Move5':
			return '5';
		case 'Move6':
			return '6';
		case 'Move7':
			return '7';
		case 'Move8':
			return '8';
		case 'Move9':
			return '9';
		case 'Move10':
			return '10';
		case 'Move11':
			return '11';
		default:
			return '12';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes = function (pulls) {
	var pull_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'pull' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, pull_, pulls));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes = function (pushes) {
	var push_ = function (m) {
		if (m.$ === 'Just') {
			var screenSize = m.a.screenSize;
			var moveCount = m.a.moveCount;
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class(
					'push' + ($rundis$elm_bootstrap$Bootstrap$Grid$Internal$screenSizeToPartialString(screenSize) + $rundis$elm_bootstrap$Bootstrap$Grid$Internal$moveCountOption(moveCount))));
		} else {
			return $elm$core$Maybe$Nothing;
		}
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, push_, pushes));
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption = function (dir) {
	switch (dir.$) {
		case 'Center':
			return 'center';
		case 'Left':
			return 'left';
		default:
			return 'right';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass = function (_v0) {
	var dir = _v0.dir;
	var size = _v0.size;
	return $elm$html$Html$Attributes$class(
		'text' + (A2(
			$elm$core$Maybe$withDefault,
			'-',
			A2(
				$elm$core$Maybe$map,
				function (s) {
					return '-' + (s + '-');
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size))) + $rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignDirOption(dir)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption = function (align) {
	switch (align.$) {
		case 'Top':
			return 'start';
		case 'Middle':
			return 'center';
		default:
			return 'end';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass = F2(
	function (prefix, _v0) {
		var align = _v0.align;
		var screenSize = _v0.screenSize;
		return $elm$html$Html$Attributes$class(
			_Utils_ap(
				prefix,
				_Utils_ap(
					A2(
						$elm$core$Maybe$withDefault,
						'',
						A2(
							$elm$core$Maybe$map,
							function (v) {
								return v + '-';
							},
							$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))),
					$rundis$elm_bootstrap$Bootstrap$Grid$Internal$verticalAlignOption(align))));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes = F2(
	function (prefix, aligns) {
		var align = function (a) {
			return A2(
				$elm$core$Maybe$map,
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignClass(prefix),
				a);
		};
		return A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2($elm$core$List$map, align, aligns));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyColOption, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultColOptions, modifiers);
	var shouldAddDefaultXs = !$elm$core$List$length(
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			_List_fromArray(
				[options.widthXs, options.widthSm, options.widthMd, options.widthLg, options.widthXl])));
	return _Utils_ap(
		$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colWidthsToAttributes(
			_List_fromArray(
				[
					shouldAddDefaultXs ? $elm$core$Maybe$Just(
					A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col)) : options.widthXs,
					options.widthSm,
					options.widthMd,
					options.widthLg,
					options.widthXl
				])),
		_Utils_ap(
			$rundis$elm_bootstrap$Bootstrap$Grid$Internal$offsetsToAttributes(
				_List_fromArray(
					[options.offsetXs, options.offsetSm, options.offsetMd, options.offsetLg, options.offsetXl])),
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$pullsToAttributes(
					_List_fromArray(
						[options.pullXs, options.pullSm, options.pullMd, options.pullLg, options.pullXl])),
				_Utils_ap(
					$rundis$elm_bootstrap$Bootstrap$Grid$Internal$pushesToAttributes(
						_List_fromArray(
							[options.pushXs, options.pushSm, options.pushMd, options.pushLg, options.pushXl])),
					_Utils_ap(
						$rundis$elm_bootstrap$Bootstrap$Grid$Internal$orderToAttributes(
							_List_fromArray(
								[options.orderXs, options.orderSm, options.orderMd, options.orderLg, options.orderXl])),
						_Utils_ap(
							A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
								'align-self-',
								_List_fromArray(
									[options.alignXs, options.alignSm, options.alignMd, options.alignLg, options.alignXl])),
							_Utils_ap(
								function () {
									var _v0 = options.textAlign;
									if (_v0.$ === 'Just') {
										var a = _v0.a;
										return _List_fromArray(
											[
												$rundis$elm_bootstrap$Bootstrap$Internal$Text$textAlignClass(a)
											]);
									} else {
										return _List_Nil;
									}
								}(),
								options.attributes)))))));
};
var $elm$virtual_dom$VirtualDom$keyedNode = function (tag) {
	return _VirtualDom_keyedNode(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$Keyed$node = $elm$virtual_dom$VirtualDom$keyedNode;
var $rundis$elm_bootstrap$Bootstrap$Grid$renderCol = function (column) {
	switch (column.$) {
		case 'Column':
			var options = column.a.options;
			var children = column.a.children;
			return A2(
				$elm$html$Html$div,
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
		case 'ColBreak':
			var e = column.a;
			return e;
		default:
			var options = column.a.options;
			var children = column.a.children;
			return A3(
				$elm$html$Html$Keyed$node,
				'div',
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
				children);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign = F2(
	function (align, options) {
		var _v0 = align.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						hAlignXs: $elm$core$Maybe$Just(align)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						hAlignSm: $elm$core$Maybe$Just(align)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						hAlignMd: $elm$core$Maybe$Just(align)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						hAlignLg: $elm$core$Maybe$Just(align)
					});
			default:
				return _Utils_update(
					options,
					{
						hAlignXl: $elm$core$Maybe$Just(align)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign = F2(
	function (align_, options) {
		var _v0 = align_.screenSize;
		switch (_v0.$) {
			case 'XS':
				return _Utils_update(
					options,
					{
						vAlignXs: $elm$core$Maybe$Just(align_)
					});
			case 'SM':
				return _Utils_update(
					options,
					{
						vAlignSm: $elm$core$Maybe$Just(align_)
					});
			case 'MD':
				return _Utils_update(
					options,
					{
						vAlignMd: $elm$core$Maybe$Just(align_)
					});
			case 'LG':
				return _Utils_update(
					options,
					{
						vAlignLg: $elm$core$Maybe$Just(align_)
					});
			default:
				return _Utils_update(
					options,
					{
						vAlignXl: $elm$core$Maybe$Just(align_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'RowAttrs':
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
			case 'RowVAlign':
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowVAlign, align, options);
			default:
				var align = modifier.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowHAlign, align, options);
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions = {attributes: _List_Nil, hAlignLg: $elm$core$Maybe$Nothing, hAlignMd: $elm$core$Maybe$Nothing, hAlignSm: $elm$core$Maybe$Nothing, hAlignXl: $elm$core$Maybe$Nothing, hAlignXs: $elm$core$Maybe$Nothing, vAlignLg: $elm$core$Maybe$Nothing, vAlignMd: $elm$core$Maybe$Nothing, vAlignSm: $elm$core$Maybe$Nothing, vAlignXl: $elm$core$Maybe$Nothing, vAlignXs: $elm$core$Maybe$Nothing};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption = function (align) {
	switch (align.$) {
		case 'Left':
			return 'start';
		case 'Center':
			return 'center';
		case 'Right':
			return 'end';
		case 'Around':
			return 'around';
		default:
			return 'between';
	}
};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass = function (_v0) {
	var align = _v0.align;
	var screenSize = _v0.screenSize;
	return $elm$html$Html$Attributes$class(
		'justify-content-' + (A2(
			$elm$core$Maybe$withDefault,
			'',
			A2(
				$elm$core$Maybe$map,
				function (v) {
					return v + '-';
				},
				$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(screenSize))) + $rundis$elm_bootstrap$Bootstrap$General$Internal$horizontalAlignOption(align)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes = function (aligns) {
	var align = function (a) {
		return A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$General$Internal$hAlignClass, a);
	};
	return A2(
		$elm$core$List$filterMap,
		$elm$core$Basics$identity,
		A2($elm$core$List$map, align, aligns));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$applyRowOption, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$defaultRowOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('row')
			]),
		_Utils_ap(
			A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$vAlignsToAttributes,
				'align-items-',
				_List_fromArray(
					[options.vAlignXs, options.vAlignSm, options.vAlignMd, options.vAlignLg, options.vAlignXl])),
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$hAlignsToAttributes(
					_List_fromArray(
						[options.hAlignXs, options.hAlignSm, options.hAlignMd, options.hAlignLg, options.hAlignXl])),
				options.attributes)));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$row = F2(
	function (options, cols) {
		return A2(
			$elm$html$Html$div,
			$rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Grid$renderCol, cols));
	});
var $elm$virtual_dom$VirtualDom$text = _VirtualDom_text;
var $elm$html$Html$text = $elm$virtual_dom$VirtualDom$text;
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col10 = {$: 'Col10'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth = function (a) {
	return {$: 'ColWidth', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$width = F2(
	function (size, count) {
		return $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColWidth(
			A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$Width, size, count));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$xs10 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col10);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col12 = {$: 'Col12'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$xs12 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col12);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2 = {$: 'Col2'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$xs2 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2);
var $author$project$Main$sampleGrid = F3(
	function (label, statistics, plotDiv) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$group,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$elm$html$Html$h4,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text(label)
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs10]),
									_List_fromArray(
										[statistics])),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs2]),
									_List_Nil)
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs12]),
									_List_fromArray(
										[plotDiv]))
								]))
						]))
				]));
	});
var $elm$html$Html$br = _VirtualDom_node('br');
var $author$project$Binomial$roundFloat = F2(
	function (digits, n) {
		var div = A2($elm$core$Basics$pow, 10, digits);
		var shifted = n * div;
		return function (x) {
			return x / div;
		}(
			$elm$core$Basics$round(shifted));
	});
var $elm$virtual_dom$VirtualDom$style = _VirtualDom_style;
var $elm$html$Html$Attributes$style = $elm$virtual_dom$VirtualDom$style;
var $author$project$Main$statisticView = function (maybeSuccess) {
	if (maybeSuccess.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		var success = maybeSuccess.a;
		var prop = $elm$core$String$fromFloat(
			A2($author$project$Binomial$roundFloat, 3, success.numSuccess / success.n));
		var n = $elm$core$String$fromInt(success.n);
		var lbl = success.successLbl;
		var emptySample = !(success.numSuccess + success.numFailures);
		var cnt = $elm$core$String$fromInt(success.numSuccess);
		return emptySample ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
			$elm$html$Html$div,
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$style, 'font-size', 'smaller')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('N = ' + n),
					A2($elm$html$Html$br, _List_Nil, _List_Nil),
					$elm$html$Html$text('Count(' + (lbl + (') = ' + cnt))),
					A2($elm$html$Html$br, _List_Nil, _List_Nil),
					$elm$html$Html$text('Prop(' + (lbl + (') = ' + prop))),
					A2($elm$html$Html$br, _List_Nil, _List_Nil)
				]));
	}
};
var $author$project$Main$bootstrapSampleView = function (model) {
	var _v0 = model.originalSample;
	if (_v0.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		return A3(
			$author$project$Main$sampleGrid,
			'Bootstrap Sample',
			$author$project$Main$statisticView(model.bootstrapSample),
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('bootstrapPlot')
					]),
				_List_Nil));
	}
};
var $author$project$Main$Collect = function (a) {
	return {$: 'Collect', a: a};
};
var $author$project$Main$Reset = {$: 'Reset'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3 = {$: 'Col3'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$xs3 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col9 = {$: 'Col9'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$xs9 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$XS, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col9);
var $author$project$Layout$collectButtonGrid = F3(
	function (reset, buttons, count) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Form$group,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs9]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h4,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Collect Statistics')
												]))
										])),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs3]),
									_List_fromArray(
										[reset]))
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs10]),
									_List_fromArray(
										[buttons]))
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$xs10]),
									_List_fromArray(
										[count]))
								]))
						]))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$attrs = function (attrs_) {
	return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$Attrs(attrs_);
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$GroupItem = function (a) {
	return {$: 'GroupItem', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size)
					});
			case 'Vertical':
				return _Utils_update(
					options,
					{vertical: true});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $elm$virtual_dom$VirtualDom$attribute = F2(
	function (key, value) {
		return A2(
			_VirtualDom_attribute,
			_VirtualDom_noOnOrFormAction(key),
			_VirtualDom_noJavaScriptOrHtmlUri(value));
	});
var $elm$html$Html$Attributes$attribute = $elm$virtual_dom$VirtualDom$attribute;
var $elm$html$Html$Attributes$classList = function (classes) {
	return $elm$html$Html$Attributes$class(
		A2(
			$elm$core$String$join,
			' ',
			A2(
				$elm$core$List$map,
				$elm$core$Tuple$first,
				A2($elm$core$List$filter, $elm$core$Tuple$second, classes))));
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$defaultOptions = {attributes: _List_Nil, size: $elm$core$Maybe$Nothing, vertical: false};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$groupAttributes = F2(
	function (toggle, modifiers) {
		var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$ButtonGroup$applyModifier, $rundis$elm_bootstrap$Bootstrap$ButtonGroup$defaultOptions, modifiers);
		return _Utils_ap(
			_List_fromArray(
				[
					A2($elm$html$Html$Attributes$attribute, 'role', 'group'),
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('btn-group', true),
							_Utils_Tuple2('btn-group-toggle', toggle),
							_Utils_Tuple2('btn-group-vertical', options.vertical)
						])),
					A2($elm$html$Html$Attributes$attribute, 'data-toggle', 'buttons')
				]),
			_Utils_ap(
				function () {
					var _v0 = A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.size);
					if (_v0.$ === 'Just') {
						var s = _v0.a;
						return _List_fromArray(
							[
								$elm$html$Html$Attributes$class('btn-group-' + s)
							]);
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$buttonGroupItem = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$GroupItem(
			A2(
				$elm$html$Html$div,
				A2($rundis$elm_bootstrap$Bootstrap$ButtonGroup$groupAttributes, false, options),
				A2(
					$elm$core$List$map,
					function (_v0) {
						var elem = _v0.a;
						return elem;
					},
					items)));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$renderGroup = function (_v0) {
	var elem = _v0.a;
	return elem;
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$buttonGroup = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$renderGroup(
			A2($rundis$elm_bootstrap$Bootstrap$ButtonGroup$buttonGroupItem, options, items));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$ButtonItem = function (a) {
	return {$: 'ButtonItem', a: a};
};
var $elm$html$Html$button = _VirtualDom_node('button');
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size)
					});
			case 'Coloring':
				var coloring = modifier.a;
				return _Utils_update(
					options,
					{
						coloring: $elm$core$Maybe$Just(coloring)
					});
			case 'Block':
				return _Utils_update(
					options,
					{block: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			default:
				var attrs = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions = {attributes: _List_Nil, block: false, coloring: $elm$core$Maybe$Nothing, disabled: false, size: $elm$core$Maybe$Nothing};
var $elm$html$Html$Attributes$boolProperty = F2(
	function (key, bool) {
		return A2(
			_VirtualDom_property,
			key,
			$elm$json$Json$Encode$bool(bool));
	});
var $elm$html$Html$Attributes$disabled = $elm$html$Html$Attributes$boolProperty('disabled');
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass = function (role) {
	switch (role.$) {
		case 'Primary':
			return 'primary';
		case 'Secondary':
			return 'secondary';
		case 'Success':
			return 'success';
		case 'Info':
			return 'info';
		case 'Warning':
			return 'warning';
		case 'Danger':
			return 'danger';
		case 'Dark':
			return 'dark';
		case 'Light':
			return 'light';
		default:
			return 'link';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Internal$Button$applyModifier, $rundis$elm_bootstrap$Bootstrap$Internal$Button$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('btn', true),
						_Utils_Tuple2('btn-block', options.block),
						_Utils_Tuple2('disabled', options.disabled)
					])),
				$elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			function () {
				var _v0 = A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption, options.size);
				if (_v0.$ === 'Just') {
					var s = _v0.a;
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$class('btn-' + s)
						]);
				} else {
					return _List_Nil;
				}
			}(),
			_Utils_ap(
				function () {
					var _v1 = options.coloring;
					if (_v1.$ === 'Just') {
						if (_v1.a.$ === 'Roled') {
							var role = _v1.a.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									'btn-' + $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						} else {
							var role = _v1.a.a;
							return _List_fromArray(
								[
									$elm$html$Html$Attributes$class(
									'btn-outline-' + $rundis$elm_bootstrap$Bootstrap$Internal$Button$roleClass(role))
								]);
						}
					} else {
						return _List_Nil;
					}
				}(),
				options.attributes)));
};
var $rundis$elm_bootstrap$Bootstrap$Button$button = F2(
	function (options, children) {
		return A2(
			$elm$html$Html$button,
			$rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options),
			children);
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$button = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$ButtonItem(
			A2($rundis$elm_bootstrap$Bootstrap$Button$button, options, children));
	});
var $author$project$Display$changeThousandsToK = function (n) {
	var zeros = $elm$core$Basics$round(
		A2($elm$core$Basics$logBase, 10, n));
	return (zeros < 3) ? $elm$core$String$fromInt(n) : ($elm$core$String$fromInt(
		A2($elm$core$Basics$pow, 10, zeros - 3)) + 'K');
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs = function (a) {
	return {$: 'Attrs', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$attrs = function (attrs_) {
	return $rundis$elm_bootstrap$Bootstrap$Internal$Button$Attrs(attrs_);
};
var $elm$virtual_dom$VirtualDom$MayPreventDefault = function (a) {
	return {$: 'MayPreventDefault', a: a};
};
var $elm$virtual_dom$VirtualDom$on = _VirtualDom_on;
var $elm$html$Html$Events$preventDefaultOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayPreventDefault(decoder));
	});
var $rundis$elm_bootstrap$Bootstrap$Button$onClick = function (message) {
	return $rundis$elm_bootstrap$Bootstrap$Button$attrs(
		_List_fromArray(
			[
				A2(
				$elm$html$Html$Events$preventDefaultOn,
				'click',
				$elm$json$Json$Decode$succeed(
					_Utils_Tuple2(message, true)))
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring = function (a) {
	return {$: 'Coloring', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary = {$: 'Primary'};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled = function (a) {
	return {$: 'Roled', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$primary = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Roled($rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary));
var $author$project$CollectButtons$collectButton = F2(
	function (toOnClick, n) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$ButtonGroup$button,
			_List_fromArray(
				[
					$rundis$elm_bootstrap$Bootstrap$Button$primary,
					$rundis$elm_bootstrap$Bootstrap$Button$onClick(
					toOnClick(n))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(
					$author$project$Display$changeThousandsToK(n))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$General$Internal$SM = {$: 'SM'};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$Size = function (a) {
	return {$: 'Size', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$small = $rundis$elm_bootstrap$Bootstrap$ButtonGroup$Size($rundis$elm_bootstrap$Bootstrap$General$Internal$SM);
var $author$project$CollectButtons$collectButtons = F2(
	function (toOnClick, ns) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$ButtonGroup$buttonGroup,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$ButtonGroup$small,
							$rundis$elm_bootstrap$Bootstrap$ButtonGroup$attrs(
							_List_fromArray(
								[
									A2($elm$html$Html$Attributes$style, 'display', 'block')
								]))
						]),
					A2(
						$elm$core$List$map,
						$author$project$CollectButtons$collectButton(toOnClick),
						ns))
				]));
	});
var $elm$core$Basics$neq = _Utils_notEqual;
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Size = function (a) {
	return {$: 'Size', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$small = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Size($rundis$elm_bootstrap$Bootstrap$General$Internal$SM);
var $author$project$CollectButtons$resetButton = function (onClickMsg) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Button$button,
		_List_fromArray(
			[
				$rundis$elm_bootstrap$Bootstrap$Button$primary,
				$rundis$elm_bootstrap$Bootstrap$Button$onClick(onClickMsg),
				$rundis$elm_bootstrap$Bootstrap$Button$small
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('Reset')
			]));
};
var $elm$core$String$right = F2(
	function (n, string) {
		return (n < 1) ? '' : A3(
			$elm$core$String$slice,
			-n,
			$elm$core$String$length(string),
			string);
	});
var $author$project$Display$pullOffThree = function (s) {
	var n = $elm$core$String$length(s);
	return (!n) ? $elm$core$Maybe$Nothing : ((n >= 3) ? $elm$core$Maybe$Just(
		_Utils_Tuple2(
			A2($elm$core$String$right, 3, s),
			A2($elm$core$String$left, n - 3, s))) : $elm$core$Maybe$Just(
		_Utils_Tuple2(s, '')));
};
var $elm_community$list_extra$List$Extra$unfoldr = F2(
	function (f, seed) {
		var _v0 = f(seed);
		if (_v0.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var _v1 = _v0.a;
			var a = _v1.a;
			var b = _v1.b;
			return A2(
				$elm$core$List$cons,
				a,
				A2($elm_community$list_extra$List$Extra$unfoldr, f, b));
		}
	});
var $author$project$Display$stringAndAddCommas = function (n) {
	return A2(
		$elm$core$String$join,
		',',
		$elm$core$List$reverse(
			A2(
				$elm_community$list_extra$List$Extra$unfoldr,
				$author$project$Display$pullOffThree,
				$elm$core$String$fromInt(n))));
};
var $author$project$CollectButtons$totalCollectedTxt = function (numTrials) {
	return $elm$html$Html$text(
		$author$project$Display$stringAndAddCommas(numTrials) + ' statistics collected');
};
var $author$project$Main$collectButtonView = function (model) {
	return A3(
		$author$project$Layout$collectButtonGrid,
		$author$project$CollectButtons$resetButton($author$project$Main$Reset),
		(!_Utils_eq(model.originalSample, $elm$core$Maybe$Nothing)) ? A2($author$project$CollectButtons$collectButtons, $author$project$Main$Collect, $author$project$Defaults$defaults.collectNs) : A2($elm$html$Html$div, _List_Nil, _List_Nil),
		$author$project$CollectButtons$totalCollectedTxt(model.trials));
};
var $elm$html$Html$b = _VirtualDom_node('b');
var $rundis$elm_bootstrap$Bootstrap$Grid$ColBreak = function (a) {
	return {$: 'ColBreak', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$colBreak = function (attributes) {
	return $rundis$elm_bootstrap$Bootstrap$Grid$ColBreak(
		A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('w-100')
					]),
				attributes),
			_List_Nil));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$container = F2(
	function (attributes, children) {
		return A2(
			$elm$html$Html$div,
			_Utils_ap(
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('container')
					]),
				attributes),
			children);
	});
var $author$project$Main$ChangeLevel = function (a) {
	return {$: 'ChangeLevel', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$RadioButtonItem = function (a) {
	return {$: 'RadioButtonItem', a: a};
};
var $elm$html$Html$Attributes$autocomplete = function (bool) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'autocomplete',
		bool ? 'on' : 'off');
};
var $elm$html$Html$Attributes$checked = $elm$html$Html$Attributes$boolProperty('checked');
var $elm$html$Html$input = _VirtualDom_node('input');
var $elm$html$Html$label = _VirtualDom_node('label');
var $elm$html$Html$Attributes$type_ = $elm$html$Html$Attributes$stringProperty('type');
var $rundis$elm_bootstrap$Bootstrap$Button$radioButton = F3(
	function (checked, options, children) {
		var hideRadio = A2($elm$html$Html$Attributes$attribute, 'data-toggle', 'button');
		return A2(
			$elm$html$Html$label,
			A2(
				$elm$core$List$cons,
				$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('active', checked)
						])),
				A2(
					$elm$core$List$cons,
					hideRadio,
					$rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(options))),
			A2(
				$elm$core$List$cons,
				A2(
					$elm$html$Html$input,
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('radio'),
							$elm$html$Html$Attributes$checked(checked),
							$elm$html$Html$Attributes$autocomplete(false)
						]),
					_List_Nil),
				children));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton = F3(
	function (checked, options, children) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$RadioButtonItem(
			A3($rundis$elm_bootstrap$Bootstrap$Button$radioButton, checked, options, children));
	});
var $author$project$Main$makeLevelButton = F3(
	function (model, conf, txt) {
		return A3(
			$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
			function () {
				var _v0 = model.level;
				if (_v0.$ === 'Nothing') {
					return false;
				} else {
					var lvl = _v0.a;
					return _Utils_eq(lvl, conf);
				}
			}(),
			_List_fromArray(
				[
					$rundis$elm_bootstrap$Bootstrap$Button$primary,
					$rundis$elm_bootstrap$Bootstrap$Button$small,
					$rundis$elm_bootstrap$Bootstrap$Button$onClick(
					$author$project$Main$ChangeLevel(conf))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroupItem = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$GroupItem(
			A2(
				$elm$html$Html$div,
				A2($rundis$elm_bootstrap$Bootstrap$ButtonGroup$groupAttributes, true, options),
				A2(
					$elm$core$List$map,
					function (_v0) {
						var elem = _v0.a;
						return elem;
					},
					items)));
	});
var $rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$ButtonGroup$renderGroup(
			A2($rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroupItem, options, items));
	});
var $author$project$Main$levelButtonView = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup,
		_List_Nil,
		A3(
			$elm$core$List$map2,
			$author$project$Main$makeLevelButton(model),
			$author$project$Defaults$defaults.levels,
			$author$project$Defaults$defaults.levelsTxt));
};
var $author$project$Main$outputText = function (model) {
	var _v0 = model.tailLimit;
	switch (_v0.$) {
		case 'NoBounds':
			return '??';
		case 'Lower':
			var l = _v0.a;
			return 'p > ' + $elm$core$String$fromFloat(
				A2($author$project$Binomial$roundFloat, 4, l));
		case 'Upper':
			var u = _v0.a;
			return 'p < ' + $elm$core$String$fromFloat(
				A2($author$project$Binomial$roundFloat, 4, u));
		default:
			var l = _v0.a;
			var u = _v0.b;
			return $elm$core$String$fromFloat(
				A2($author$project$Binomial$roundFloat, 4, l)) + (' < p < ' + $elm$core$String$fromFloat(
				A2($author$project$Binomial$roundFloat, 4, u)));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm3 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8 = {$: 'Col8'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8);
var $author$project$Main$ChangeTail = function (a) {
	return {$: 'ChangeTail', a: a};
};
var $author$project$Limits$Left = {$: 'Left'};
var $author$project$Limits$Right = {$: 'Right'};
var $author$project$Limits$Two = {$: 'Two'};
var $author$project$Main$tailButtonView = function (model) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButtonGroup,
		_List_Nil,
		_List_fromArray(
			[
				A3(
				$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
				_Utils_eq(model.tail, $author$project$Limits$Left),
				_List_fromArray(
					[
						$rundis$elm_bootstrap$Bootstrap$Button$primary,
						$rundis$elm_bootstrap$Bootstrap$Button$small,
						$rundis$elm_bootstrap$Bootstrap$Button$onClick(
						$author$project$Main$ChangeTail($author$project$Limits$Left))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Left-tail')
					])),
				A3(
				$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
				_Utils_eq(model.tail, $author$project$Limits$Right),
				_List_fromArray(
					[
						$rundis$elm_bootstrap$Bootstrap$Button$primary,
						$rundis$elm_bootstrap$Bootstrap$Button$small,
						$rundis$elm_bootstrap$Bootstrap$Button$onClick(
						$author$project$Main$ChangeTail($author$project$Limits$Right))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Right-tail')
					])),
				A3(
				$rundis$elm_bootstrap$Bootstrap$ButtonGroup$radioButton,
				_Utils_eq(model.tail, $author$project$Limits$Two),
				_List_fromArray(
					[
						$rundis$elm_bootstrap$Bootstrap$Button$primary,
						$rundis$elm_bootstrap$Bootstrap$Button$small,
						$rundis$elm_bootstrap$Bootstrap$Button$onClick(
						$author$project$Main$ChangeTail($author$project$Limits$Two))
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Two-tail')
					]))
			]));
};
var $author$project$Main$confLimitsView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Grid$container,
				_List_Nil,
				_List_fromArray(
					[
						A2(
						$elm$html$Html$h4,
						_List_Nil,
						_List_fromArray(
							[
								$elm$html$Html$text('Confidence Interval')
							])),
						A2(
						$rundis$elm_bootstrap$Bootstrap$Grid$row,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm3]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Tail')
											]))
									])),
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8]),
								_List_fromArray(
									[
										$author$project$Main$tailButtonView(model)
									])),
								$rundis$elm_bootstrap$Bootstrap$Grid$colBreak(_List_Nil),
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm3]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Level')
											]))
									])),
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8]),
								_List_fromArray(
									[
										$author$project$Main$levelButtonView(model)
									])),
								$rundis$elm_bootstrap$Bootstrap$Grid$colBreak(_List_Nil),
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm3]),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Interval')
											]))
									])),
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$col,
								_List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm8]),
								_List_fromArray(
									[
										$elm$html$Html$text(
										$author$project$Main$outputText(model))
									]))
							]))
					]))
			]));
};
var $author$project$Main$ChangeFileType = function (a) {
	return {$: 'ChangeFileType', a: a};
};
var $author$project$Main$CloseModal = {$: 'CloseModal'};
var $author$project$Main$CsvRequested = {$: 'CsvRequested'};
var $author$project$Main$LoadUserData = {$: 'LoadUserData'};
var $author$project$Main$SelectCount = function (a) {
	return {$: 'SelectCount', a: a};
};
var $author$project$Main$SelectVariable = function (a) {
	return {$: 'SelectVariable', a: a};
};
var $author$project$Main$ShowModal = {$: 'ShowModal'};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$AlignMenuRight = {$: 'AlignMenuRight'};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$alignMenuRight = $rundis$elm_bootstrap$Bootstrap$Dropdown$AlignMenuRight;
var $rundis$elm_bootstrap$Bootstrap$Modal$Body = function (a) {
	return {$: 'Body', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Modal$Config = function (a) {
	return {$: 'Config', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Modal$body = F3(
	function (attributes, children, _v0) {
		var conf = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Modal$Config(
			_Utils_update(
				conf,
				{
					body: $elm$core$Maybe$Just(
						$rundis$elm_bootstrap$Bootstrap$Modal$Body(
							{attributes: attributes, children: children}))
				}));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Bordered = {$: 'Bordered'};
var $rundis$elm_bootstrap$Bootstrap$Table$bordered = $rundis$elm_bootstrap$Bootstrap$Table$Bordered;
var $rundis$elm_bootstrap$Bootstrap$Form$Col = function (a) {
	return {$: 'Col', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$col = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Col(
			{children: children, elemFn: $elm$html$Html$div, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs = function (a) {
	return {$: 'ColAttrs', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs = function (attrs_) {
	return $rundis$elm_bootstrap$Bootstrap$Grid$Internal$ColAttrs(attrs_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$colLabel = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Col(
			{
				children: children,
				elemFn: $elm$html$Html$label,
				options: A2(
					$elm$core$List$cons,
					$rundis$elm_bootstrap$Bootstrap$Grid$Col$attrs(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('col-form-label')
							])),
					options)
			});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config = function (a) {
	return {$: 'Config', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config = function (input_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config(
		{attributes: _List_Nil, input: input_, predecessors: _List_Nil, size: $elm$core$Maybe$Nothing, successors: _List_Nil});
};
var $rundis$elm_bootstrap$Bootstrap$Modal$config = function (closeMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Modal$Config(
		{
			body: $elm$core$Maybe$Nothing,
			closeMsg: closeMsg,
			footer: $elm$core$Maybe$Nothing,
			header: $elm$core$Maybe$Nothing,
			options: {attrs: _List_Nil, centered: true, hideOnBackdropClick: true, modalSize: $elm$core$Maybe$Nothing, scrollableBody: false},
			withAnimation: $elm$core$Maybe$Nothing
		});
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Custom = {$: 'Custom'};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Select = function (a) {
	return {$: 'Select', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$create = F2(
	function (options, items) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Select$Select(
			{items: items, options: options});
	});
var $elm$html$Html$select = _VirtualDom_node('select');
var $rundis$elm_bootstrap$Bootstrap$Form$Select$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size_ = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size_)
					});
			case 'Id':
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(id_)
					});
			case 'Custom':
				return _Utils_update(
					options,
					{custom: true});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			case 'OnChange':
				var onChange_ = modifier.a;
				return _Utils_update(
					options,
					{
						onChange: $elm$core$Maybe$Just(onChange_)
					});
			case 'Validation':
				var validation_ = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation_)
					});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $elm$virtual_dom$VirtualDom$Normal = function (a) {
	return {$: 'Normal', a: a};
};
var $elm$html$Html$Events$on = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$Normal(decoder));
	});
var $elm$json$Json$Decode$field = _Json_decodeField;
var $elm$json$Json$Decode$at = F2(
	function (fields, decoder) {
		return A3($elm$core$List$foldr, $elm$json$Json$Decode$field, decoder, fields);
	});
var $elm$json$Json$Decode$string = _Json_decodeString;
var $elm$html$Html$Events$targetValue = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['target', 'value']),
	$elm$json$Json$Decode$string);
var $rundis$elm_bootstrap$Bootstrap$Form$Select$customEventOnChange = function (tagger) {
	return A2(
		$elm$html$Html$Events$on,
		'change',
		A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$defaultOptions = {attributes: _List_Nil, custom: false, disabled: false, id: $elm$core$Maybe$Nothing, onChange: $elm$core$Maybe$Nothing, size: $elm$core$Maybe$Nothing, validation: $elm$core$Maybe$Nothing};
var $elm$core$Basics$not = _Basics_not;
var $rundis$elm_bootstrap$Bootstrap$Form$Select$sizeAttribute = F2(
	function (isCustom, size_) {
		var prefix = isCustom ? 'custom-select-' : 'form-control-';
		return A2(
			$elm$core$Maybe$map,
			function (s) {
				return $elm$html$Html$Attributes$class(
					_Utils_ap(prefix, s));
			},
			$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size_));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString = function (validation) {
	if (validation.$ === 'Success') {
		return 'is-valid';
	} else {
		return 'is-invalid';
	}
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$validationAttribute = function (validation_) {
	return $elm$html$Html$Attributes$class(
		$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation_));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Select$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Select$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$classList(
				_List_fromArray(
					[
						_Utils_Tuple2('form-control', !options.custom),
						_Utils_Tuple2('custom-select', options.custom)
					])),
				$elm$html$Html$Attributes$disabled(options.disabled)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id),
						A2(
						$elm$core$Maybe$andThen,
						$rundis$elm_bootstrap$Bootstrap$Form$Select$sizeAttribute(options.custom),
						options.size),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Select$customEventOnChange, options.onChange),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Select$validationAttribute, options.validation)
					])),
			options.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$view = function (_v0) {
	var options = _v0.a.options;
	var items = _v0.a.items;
	return A2(
		$elm$html$Html$select,
		$rundis$elm_bootstrap$Bootstrap$Form$Select$toAttributes(options),
		A2(
			$elm$core$List$map,
			function (_v1) {
				var e = _v1.a;
				return e;
			},
			items));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$custom = function (options) {
	return A2(
		$elm$core$Basics$composeL,
		$rundis$elm_bootstrap$Bootstrap$Form$Select$view,
		$rundis$elm_bootstrap$Bootstrap$Form$Select$create(
			A2($elm$core$List$cons, $rundis$elm_bootstrap$Bootstrap$Form$Select$Custom, options)));
};
var $author$project$Main$dataPlaceholder = function (model) {
	var _v0 = model.selected;
	if (_v0.$ === 'Nothing') {
		return 'Select';
	} else {
		var data = _v0.a;
		return data.name;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Disabled = function (a) {
	return {$: 'Disabled', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$disabled = function (disabled_) {
	return $rundis$elm_bootstrap$Bootstrap$Internal$Button$Disabled(disabled_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Disabled = function (a) {
	return {$: 'Disabled', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$disabled = function (disabled_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Input$Disabled(disabled_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Disabled = function (a) {
	return {$: 'Disabled', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$disabled = function (disabled_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Select$Disabled(disabled_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Addon = function (a) {
	return {$: 'Addon', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$dropDir = function (maybeDir) {
	var toAttrs = function (dir) {
		return _List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				'drop' + function () {
					if (dir.$ === 'Dropleft') {
						return 'left';
					} else {
						return 'right';
					}
				}())
			]);
	};
	return A2(
		$elm$core$Maybe$withDefault,
		_List_Nil,
		A2($elm$core$Maybe$map, toAttrs, maybeDir));
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownAttributes = F2(
	function (status, config) {
		return _Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('btn-group', true),
							_Utils_Tuple2(
							'show',
							!_Utils_eq(status, $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed)),
							_Utils_Tuple2('dropup', config.isDropUp)
						]))
				]),
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Dropdown$dropDir(config.dropDirection),
				config.attributes));
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$menuStyles = F2(
	function (_v0, config) {
		var status = _v0.a.status;
		var toggleSize = _v0.a.toggleSize;
		var menuSize = _v0.a.menuSize;
		var px = function (n) {
			return $elm$core$String$fromFloat(n) + 'px';
		};
		var translate = F3(
			function (x, y, z) {
				return 'translate3d(' + (px(x) + (',' + (px(y) + (',' + (px(z) + ')')))));
			});
		var _default = _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'top', '0'),
				A2($elm$html$Html$Attributes$style, 'left', '0')
			]);
		var _v1 = _Utils_Tuple2(config.isDropUp, config.dropDirection);
		_v1$0:
		while (true) {
			if (_v1.b.$ === 'Just') {
				if (_v1.b.a.$ === 'Dropright') {
					if (_v1.a) {
						break _v1$0;
					} else {
						var _v2 = _v1.b.a;
						return _default;
					}
				} else {
					if (_v1.a) {
						break _v1$0;
					} else {
						var _v3 = _v1.b.a;
						return _Utils_ap(
							_default,
							_List_fromArray(
								[
									A2(
									$elm$html$Html$Attributes$style,
									'transform',
									A3(translate, (-toggleSize.width) - menuSize.width, 0, 0))
								]));
					}
				}
			} else {
				if (_v1.a) {
					break _v1$0;
				} else {
					return _Utils_ap(
						_default,
						_List_fromArray(
							[
								A2(
								$elm$html$Html$Attributes$style,
								'transform',
								A3(translate, -toggleSize.width, toggleSize.height, 0))
							]));
				}
			}
		}
		return _Utils_ap(
			_default,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$Attributes$style,
					'transform',
					A3(translate, -toggleSize.width, -menuSize.height, 0))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownMenu = F3(
	function (state, config, items) {
		var status = state.a.status;
		var menuSize = state.a.menuSize;
		var wrapperStyles = _Utils_eq(status, $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed) ? _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'height', '0'),
				A2($elm$html$Html$Attributes$style, 'overflow', 'hidden'),
				A2($elm$html$Html$Attributes$style, 'position', 'relative')
			]) : _List_fromArray(
			[
				A2($elm$html$Html$Attributes$style, 'position', 'relative')
			]);
		return A2(
			$elm$html$Html$div,
			wrapperStyles,
			_List_fromArray(
				[
					A2(
					$elm$html$Html$div,
					_Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('dropdown-menu', true),
										_Utils_Tuple2('dropdown-menu-right', config.hasMenuRight),
										_Utils_Tuple2(
										'show',
										!_Utils_eq(status, $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed))
									]))
							]),
						_Utils_ap(
							A2($rundis$elm_bootstrap$Bootstrap$Dropdown$menuStyles, state, config),
							config.menuAttrs)),
					A2(
						$elm$core$List$map,
						function (_v0) {
							var x = _v0.a;
							return x;
						},
						items))
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$applyModifier = F2(
	function (option, options) {
		switch (option.$) {
			case 'AlignMenuRight':
				return _Utils_update(
					options,
					{hasMenuRight: true});
			case 'Dropup':
				return _Utils_update(
					options,
					{isDropUp: true});
			case 'Attrs':
				var attrs_ = option.a;
				return _Utils_update(
					options,
					{attributes: attrs_});
			case 'DropToDir':
				var dir = option.a;
				return _Utils_update(
					options,
					{
						dropDirection: $elm$core$Maybe$Just(dir)
					});
			default:
				var attrs_ = option.a;
				return _Utils_update(
					options,
					{menuAttrs: attrs_});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$defaultOptions = {attributes: _List_Nil, dropDirection: $elm$core$Maybe$Nothing, hasMenuRight: false, isDropUp: false, menuAttrs: _List_Nil};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$toConfig = function (options) {
	return A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Dropdown$applyModifier, $rundis$elm_bootstrap$Bootstrap$Dropdown$defaultOptions, options);
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown = F2(
	function (state, _v0) {
		var status = state.a.status;
		var toggleMsg = _v0.toggleMsg;
		var toggleButton = _v0.toggleButton;
		var items = _v0.items;
		var options = _v0.options;
		var config = $rundis$elm_bootstrap$Bootstrap$Dropdown$toConfig(options);
		var _v1 = toggleButton;
		var buttonFn = _v1.a;
		return A2(
			$elm$html$Html$div,
			A2($rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownAttributes, status, config),
			_List_fromArray(
				[
					A2(buttonFn, toggleMsg, state),
					A3($rundis$elm_bootstrap$Bootstrap$Dropdown$dropdownMenu, state, config, items)
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$dropdown = F2(
	function (state, conf) {
		return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Addon(
			A2($rundis$elm_bootstrap$Bootstrap$Dropdown$dropdown, state, conf));
	});
var $author$project$Main$ChangeData = function (a) {
	return {$: 'ChangeData', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownItem = function (a) {
	return {$: 'DropdownItem', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$buttonItem = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownItem(
			A2(
				$elm$html$Html$button,
				_Utils_ap(
					_List_fromArray(
						[
							$elm$html$Html$Attributes$type_('button'),
							$elm$html$Html$Attributes$class('dropdown-item')
						]),
					attributes),
				children));
	});
var $elm$html$Html$Events$onClick = function (msg) {
	return A2(
		$elm$html$Html$Events$on,
		'click',
		$elm$json$Json$Decode$succeed(msg));
};
var $author$project$Main$dropdownItem = F2(
	function (msg, txt) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Dropdown$buttonItem,
			_List_fromArray(
				[
					$elm$html$Html$Events$onClick(
					msg(txt))
				]),
			_List_fromArray(
				[
					$elm$html$Html$text(txt)
				]));
	});
var $author$project$Main$dropdownDataItem = $author$project$Main$dropdownItem($author$project$Main$ChangeData);
var $author$project$Main$dropdownData = function (datasets) {
	return A2(
		$elm$core$List$map,
		$author$project$Main$dropdownDataItem,
		A2(
			$elm$core$List$map,
			function ($) {
				return $.name;
			},
			datasets));
};
var $author$project$Main$ChangeSuccess = function (a) {
	return {$: 'ChangeSuccess', a: a};
};
var $author$project$Main$dropdownSuccessItem = $author$project$Main$dropdownItem($author$project$Main$ChangeSuccess);
var $author$project$DataSet$getLabels = function (data) {
	return A2(
		$elm$core$List$map,
		function ($) {
			return $.label;
		},
		data.frequencies);
};
var $author$project$Main$dropdownSuccess = function (model) {
	var _v0 = model.selected;
	if (_v0.$ === 'Nothing') {
		return _List_Nil;
	} else {
		var d = _v0.a;
		return A2(
			$elm$core$List$map,
			$author$project$Main$dropdownSuccessItem,
			$author$project$DataSet$getLabels(d));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Modal$Footer = function (a) {
	return {$: 'Footer', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Modal$footer = F3(
	function (attributes, children, _v0) {
		var conf = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Modal$Config(
			_Utils_update(
				conf,
				{
					footer: $elm$core$Maybe$Just(
						$rundis$elm_bootstrap$Bootstrap$Modal$Footer(
							{attributes: attributes, children: children}))
				}));
	});
var $elm$html$Html$form = _VirtualDom_node('form');
var $rundis$elm_bootstrap$Bootstrap$Form$form = F2(
	function (attributes, children) {
		return A2($elm$html$Html$form, attributes, children);
	});
var $elm$html$Html$h5 = _VirtualDom_node('h5');
var $rundis$elm_bootstrap$Bootstrap$Modal$Header = function (a) {
	return {$: 'Header', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Modal$header = F3(
	function (attributes, children, _v0) {
		var conf = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Modal$Config(
			_Utils_update(
				conf,
				{
					header: $elm$core$Maybe$Just(
						$rundis$elm_bootstrap$Bootstrap$Modal$Header(
							{attributes: attributes, children: children}))
				}));
	});
var $rundis$elm_bootstrap$Bootstrap$Modal$titledHeader = F3(
	function (itemFn, attributes, children) {
		return A2(
			$rundis$elm_bootstrap$Bootstrap$Modal$header,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					itemFn,
					A2(
						$elm$core$List$cons,
						$elm$html$Html$Attributes$class('modal-title'),
						attributes),
					children)
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Modal$h5 = $rundis$elm_bootstrap$Bootstrap$Modal$titledHeader($elm$html$Html$h5);
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Id = function (a) {
	return {$: 'Id', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$id = function (id_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Select$Id(id_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$Item = function (a) {
	return {$: 'Item', a: a};
};
var $elm$html$Html$option = _VirtualDom_node('option');
var $rundis$elm_bootstrap$Bootstrap$Form$Select$item = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Select$Item(
			A2($elm$html$Html$option, attributes, children));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Td = function (a) {
	return {$: 'Td', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$td = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Td(
			{children: children, options: options});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Row = function (a) {
	return {$: 'Row', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$tr = F2(
	function (options, cells) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Row(
			{cells: cells, options: options});
	});
var $author$project$Main$makeTableRow = function (labelFreq) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Table$tr,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$rundis$elm_bootstrap$Bootstrap$Table$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(labelFreq.label)
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Table$td,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text(
						$elm$core$String$fromInt(labelFreq.count))
					]))
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$ml1 = $elm$html$Html$Attributes$class('ml-1');
var $rundis$elm_bootstrap$Bootstrap$Form$Select$OnChange = function (a) {
	return {$: 'OnChange', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Select$onChange = function (toMsg) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Select$OnChange(toMsg);
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Button$Outlined = function (a) {
	return {$: 'Outlined', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary = $rundis$elm_bootstrap$Bootstrap$Internal$Button$Coloring(
	$rundis$elm_bootstrap$Bootstrap$Internal$Button$Outlined($rundis$elm_bootstrap$Bootstrap$Internal$Button$Primary));
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Placeholder = function (a) {
	return {$: 'Placeholder', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$placeholder = function (value_) {
	return $rundis$elm_bootstrap$Bootstrap$Form$Input$Placeholder(value_);
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors = F2(
	function (addons, _v0) {
		var conf = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config(
			_Utils_update(
				conf,
				{predecessors: addons}));
	});
var $rundis$elm_bootstrap$Bootstrap$General$Internal$Right = {$: 'Right'};
var $rundis$elm_bootstrap$Bootstrap$General$Internal$HAlign = F2(
	function (screenSize, align) {
		return {align: align, screenSize: screenSize};
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowHAlign = function (a) {
	return {$: 'RowHAlign', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowHAlign = F2(
	function (size, align) {
		return $rundis$elm_bootstrap$Bootstrap$Grid$Internal$RowHAlign(
			A2($rundis$elm_bootstrap$Bootstrap$General$Internal$HAlign, size, align));
	});
var $rundis$elm_bootstrap$Bootstrap$Grid$Row$rightSm = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowHAlign, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$General$Internal$Right);
var $rundis$elm_bootstrap$Bootstrap$Form$renderCol = function (_v0) {
	var elemFn = _v0.a.elemFn;
	var options = _v0.a.options;
	var children = _v0.a.children;
	return A2(
		elemFn,
		$rundis$elm_bootstrap$Bootstrap$Grid$Internal$colAttributes(options),
		children);
};
var $rundis$elm_bootstrap$Bootstrap$Form$row = F2(
	function (options, cols) {
		return A2(
			$elm$html$Html$div,
			A2(
				$elm$core$List$cons,
				$elm$html$Html$Attributes$class('form-group'),
				$rundis$elm_bootstrap$Bootstrap$Grid$Internal$rowAttributes(options)),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Form$renderCol, cols));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$THead = function (a) {
	return {$: 'THead', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$thead = F2(
	function (options, rows) {
		return $rundis$elm_bootstrap$Bootstrap$Table$THead(
			{options: options, rows: rows});
	});
var $rundis$elm_bootstrap$Bootstrap$Table$simpleThead = function (cells) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Table$thead,
		_List_Nil,
		_List_fromArray(
			[
				A2($rundis$elm_bootstrap$Bootstrap$Table$tr, _List_Nil, cells)
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col2);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col4 = {$: 'Col4'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col4);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col6 = {$: 'Col6'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$SM, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col6);
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$small = function (_v0) {
	var conf = _v0.a;
	return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config(
		_Utils_update(
			conf,
			{
				size: $elm$core$Maybe$Just($rundis$elm_bootstrap$Bootstrap$General$Internal$SM)
			}));
};
var $rundis$elm_bootstrap$Bootstrap$Table$Small = {$: 'Small'};
var $rundis$elm_bootstrap$Bootstrap$Table$small = $rundis$elm_bootstrap$Bootstrap$Table$Small;
var $elm$html$Html$span = _VirtualDom_node('span');
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span = F2(
	function (attributes, children) {
		return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Addon(
			A2(
				$elm$html$Html$span,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('input-group-text'),
					attributes),
				children));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Striped = {$: 'Striped'};
var $rundis$elm_bootstrap$Bootstrap$Table$striped = $rundis$elm_bootstrap$Bootstrap$Table$Striped;
var $rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownToggle = function (a) {
	return {$: 'DropdownToggle', a: a};
};
var $elm$json$Json$Decode$andThen = _Json_andThen;
var $rundis$elm_bootstrap$Bootstrap$Dropdown$Open = {$: 'Open'};
var $rundis$elm_bootstrap$Bootstrap$Dropdown$nextStatus = function (status) {
	switch (status.$) {
		case 'Open':
			return $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed;
		case 'ListenClicks':
			return $rundis$elm_bootstrap$Bootstrap$Dropdown$Closed;
		default:
			return $rundis$elm_bootstrap$Bootstrap$Dropdown$Open;
	}
};
var $elm$json$Json$Decode$map3 = _Json_map3;
var $elm$json$Json$Decode$float = _Json_decodeFloat;
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight = A2($elm$json$Json$Decode$field, 'offsetHeight', $elm$json$Json$Decode$float);
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetWidth = A2($elm$json$Json$Decode$field, 'offsetWidth', $elm$json$Json$Decode$float);
var $elm$json$Json$Decode$map4 = _Json_map4;
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetLeft = A2($elm$json$Json$Decode$field, 'offsetLeft', $elm$json$Json$Decode$float);
var $elm$json$Json$Decode$null = _Json_decodeNull;
var $elm$json$Json$Decode$oneOf = _Json_oneOf;
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetParent = F2(
	function (x, decoder) {
		return $elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$json$Json$Decode$field,
					'offsetParent',
					$elm$json$Json$Decode$null(x)),
					A2($elm$json$Json$Decode$field, 'offsetParent', decoder)
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetTop = A2($elm$json$Json$Decode$field, 'offsetTop', $elm$json$Json$Decode$float);
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollLeft = A2($elm$json$Json$Decode$field, 'scrollLeft', $elm$json$Json$Decode$float);
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollTop = A2($elm$json$Json$Decode$field, 'scrollTop', $elm$json$Json$Decode$float);
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position = F2(
	function (x, y) {
		return A2(
			$elm$json$Json$Decode$andThen,
			function (_v0) {
				var x_ = _v0.a;
				var y_ = _v0.b;
				return A2(
					$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetParent,
					_Utils_Tuple2(x_, y_),
					A2($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position, x_, y_));
			},
			A5(
				$elm$json$Json$Decode$map4,
				F4(
					function (scrollLeft_, scrollTop_, offsetLeft_, offsetTop_) {
						return _Utils_Tuple2((x + offsetLeft_) - scrollLeft_, (y + offsetTop_) - scrollTop_);
					}),
				$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollLeft,
				$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$scrollTop,
				$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetLeft,
				$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetTop));
	});
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea = A4(
	$elm$json$Json$Decode$map3,
	F3(
		function (_v0, width, height) {
			var x = _v0.a;
			var y = _v0.b;
			return {height: height, left: x, top: y, width: width};
		}),
	A2($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$position, 0, 0),
	$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetWidth,
	$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$offsetHeight);
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode = function (idx) {
	return $elm$json$Json$Decode$at(
		_List_fromArray(
			[
				'childNodes',
				$elm$core$String$fromInt(idx)
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'nextSibling', decoder);
};
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className = A2(
	$elm$json$Json$Decode$at,
	_List_fromArray(
		['className']),
	$elm$json$Json$Decode$string);
var $elm$json$Json$Decode$fail = _Json_fail;
var $rundis$elm_bootstrap$Bootstrap$Dropdown$isToggle = A2(
	$elm$json$Json$Decode$andThen,
	function (_class) {
		return A2($elm$core$String$contains, 'dropdown-toggle', _class) ? $elm$json$Json$Decode$succeed(true) : $elm$json$Json$Decode$succeed(false);
	},
	$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className);
var $rundis$elm_bootstrap$Bootstrap$Dropdown$toggler = F2(
	function (path, decoder) {
		return $elm$json$Json$Decode$oneOf(
			_List_fromArray(
				[
					A2(
					$elm$json$Json$Decode$andThen,
					function (res) {
						return res ? A2($elm$json$Json$Decode$at, path, decoder) : $elm$json$Json$Decode$fail('');
					},
					A2($elm$json$Json$Decode$at, path, $rundis$elm_bootstrap$Bootstrap$Dropdown$isToggle)),
					A2(
					$elm$json$Json$Decode$andThen,
					function (_v0) {
						return A2(
							$rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
							_Utils_ap(
								path,
								_List_fromArray(
									['parentElement'])),
							decoder);
					},
					A2(
						$elm$json$Json$Decode$at,
						_Utils_ap(
							path,
							_List_fromArray(
								['parentElement'])),
						$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className)),
					$elm$json$Json$Decode$fail('No toggler found')
				]));
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$sizeDecoder = A3(
	$elm$json$Json$Decode$map2,
	$elm$core$Tuple$pair,
	A2(
		$rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
		_List_fromArray(
			['target']),
		$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea),
	A2(
		$rundis$elm_bootstrap$Bootstrap$Dropdown$toggler,
		_List_fromArray(
			['target']),
		$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$nextSibling(
			A2($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$childNode, 0, $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$boundingArea))));
var $rundis$elm_bootstrap$Bootstrap$Dropdown$clickHandler = F2(
	function (toMsg, state) {
		var status = state.a.status;
		return A2(
			$elm$json$Json$Decode$andThen,
			function (_v0) {
				var b = _v0.a;
				var m = _v0.b;
				return $elm$json$Json$Decode$succeed(
					toMsg(
						$rundis$elm_bootstrap$Bootstrap$Dropdown$State(
							{
								menuSize: m,
								status: $rundis$elm_bootstrap$Bootstrap$Dropdown$nextStatus(status),
								toggleSize: b
							})));
			},
			$rundis$elm_bootstrap$Bootstrap$Dropdown$sizeDecoder);
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$togglePrivate = F4(
	function (buttonOptions, children, toggleMsg, state) {
		return A2(
			$elm$html$Html$button,
			_Utils_ap(
				$rundis$elm_bootstrap$Bootstrap$Internal$Button$buttonAttributes(buttonOptions),
				_List_fromArray(
					[
						$elm$html$Html$Attributes$class('dropdown-toggle'),
						$elm$html$Html$Attributes$type_('button'),
						A2(
						$elm$html$Html$Events$on,
						'click',
						A2($rundis$elm_bootstrap$Bootstrap$Dropdown$clickHandler, toggleMsg, state))
					])),
			children);
	});
var $rundis$elm_bootstrap$Bootstrap$Dropdown$toggle = F2(
	function (buttonOptions, children) {
		return $rundis$elm_bootstrap$Bootstrap$Dropdown$DropdownToggle(
			A2($rundis$elm_bootstrap$Bootstrap$Dropdown$togglePrivate, buttonOptions, children));
	});
var $author$project$Main$successButtonToggle = function (model) {
	var disabled = function () {
		var _v0 = model.selected;
		if (_v0.$ === 'Nothing') {
			return true;
		} else {
			return false;
		}
	}();
	var attr = _List_fromArray(
		[
			$rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary,
			$rundis$elm_bootstrap$Bootstrap$Button$small,
			$rundis$elm_bootstrap$Bootstrap$Button$disabled(disabled)
		]);
	return A2($rundis$elm_bootstrap$Bootstrap$Dropdown$toggle, attr, _List_Nil);
};
var $author$project$Main$successPlaceholder = function (model) {
	var _v0 = model.originalSample;
	if (_v0.$ === 'Nothing') {
		return 'Select';
	} else {
		var s = _v0.a;
		return s.successLbl;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors = F2(
	function (addons, _v0) {
		var conf = _v0.a;
		return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Config(
			_Utils_update(
				conf,
				{successors: addons}));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$Inversed = {$: 'Inversed'};
var $elm$core$List$any = F2(
	function (isOkay, list) {
		any:
		while (true) {
			if (!list.b) {
				return false;
			} else {
				var x = list.a;
				var xs = list.b;
				if (isOkay(x)) {
					return true;
				} else {
					var $temp$isOkay = isOkay,
						$temp$list = xs;
					isOkay = $temp$isOkay;
					list = $temp$list;
					continue any;
				}
			}
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Table$isResponsive = function (option) {
	if (option.$ === 'Responsive') {
		return true;
	} else {
		return false;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody = function (a) {
	return {$: 'KeyedTBody', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$TBody = function (a) {
	return {$: 'TBody', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$InversedRow = function (a) {
	return {$: 'InversedRow', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow = function (a) {
	return {$: 'KeyedRow', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$InversedCell = function (a) {
	return {$: 'InversedCell', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$Th = function (a) {
	return {$: 'Th', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell = function (cell) {
	var inverseOptions = function (options) {
		return A2(
			$elm$core$List$map,
			function (opt) {
				if (opt.$ === 'RoledCell') {
					var role = opt.a;
					return $rundis$elm_bootstrap$Bootstrap$Table$InversedCell(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (cell.$ === 'Th') {
		var cellCfg = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellCfg,
				{
					options: inverseOptions(cellCfg.options)
				}));
	} else {
		var cellCfg = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Td(
			_Utils_update(
				cellCfg,
				{
					options: inverseOptions(cellCfg.options)
				}));
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow = function (row) {
	var inversedOptions = function (options) {
		return A2(
			$elm$core$List$map,
			function (opt) {
				if (opt.$ === 'RoledRow') {
					var role = opt.a;
					return $rundis$elm_bootstrap$Bootstrap$Table$InversedRow(role);
				} else {
					return opt;
				}
			},
			options);
	};
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		return $rundis$elm_bootstrap$Bootstrap$Table$Row(
			{
				cells: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell, cells),
				options: inversedOptions(options)
			});
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		return $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
			{
				cells: A2(
					$elm$core$List$map,
					function (_v1) {
						var key = _v1.a;
						var cell = _v1.b;
						return _Utils_Tuple2(
							key,
							$rundis$elm_bootstrap$Bootstrap$Table$mapInversedCell(cell));
					},
					cells),
				options: inversedOptions(options)
			});
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody = F2(
	function (isTableInversed, tbody_) {
		var _v0 = _Utils_Tuple2(isTableInversed, tbody_);
		if (!_v0.a) {
			return tbody_;
		} else {
			if (_v0.b.$ === 'TBody') {
				var body = _v0.b.a;
				return $rundis$elm_bootstrap$Bootstrap$Table$TBody(
					_Utils_update(
						body,
						{
							rows: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, body.rows)
						}));
			} else {
				var keyedBody = _v0.b.a;
				return $rundis$elm_bootstrap$Bootstrap$Table$KeyedTBody(
					_Utils_update(
						keyedBody,
						{
							rows: A2(
								$elm$core$List$map,
								function (_v1) {
									var key = _v1.a;
									var row = _v1.b;
									return _Utils_Tuple2(
										key,
										$rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow(row));
								},
								keyedBody.rows)
						}));
			}
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Table$InversedHead = {$: 'InversedHead'};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead = F2(
	function (isTableInversed, _v0) {
		var thead_ = _v0.a;
		var isHeadInversed = A2(
			$elm$core$List$any,
			function (opt) {
				return _Utils_eq(opt, $rundis$elm_bootstrap$Bootstrap$Table$InversedHead);
			},
			thead_.options);
		return $rundis$elm_bootstrap$Bootstrap$Table$THead(
			(isTableInversed || isHeadInversed) ? _Utils_update(
				thead_,
				{
					rows: A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$mapInversedRow, thead_.rows)
				}) : thead_);
	});
var $rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive = F2(
	function (options, table_) {
		var responsiveClass = $elm$html$Html$Attributes$class(
			'table-responsive' + A2(
				$elm$core$Maybe$withDefault,
				'',
				A2(
					$elm$core$Maybe$map,
					function (v) {
						return '-' + v;
					},
					A2(
						$elm$core$Maybe$andThen,
						$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption,
						A2(
							$elm$core$Maybe$andThen,
							function (opt) {
								if (opt.$ === 'Responsive') {
									var val = opt.a;
									return val;
								} else {
									return $elm$core$Maybe$Nothing;
								}
							},
							$elm$core$List$head(
								A2($elm$core$List$filter, $rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options)))))));
		return A2($elm$core$List$any, $rundis$elm_bootstrap$Bootstrap$Table$isResponsive, options) ? A2(
			$elm$html$Html$div,
			_List_fromArray(
				[responsiveClass]),
			_List_fromArray(
				[table_])) : table_;
	});
var $rundis$elm_bootstrap$Bootstrap$Table$CellAttr = function (a) {
	return {$: 'CellAttr', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttr = function (attr_) {
	return $rundis$elm_bootstrap$Bootstrap$Table$CellAttr(attr_);
};
var $elm$html$Html$Attributes$scope = $elm$html$Html$Attributes$stringProperty('scope');
var $rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh = function (cell) {
	if (cell.$ === 'Th') {
		var cellConfig = cell.a;
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			_Utils_update(
				cellConfig,
				{
					options: A2(
						$elm$core$List$cons,
						$rundis$elm_bootstrap$Bootstrap$Table$cellAttr(
							$elm$html$Html$Attributes$scope('row')),
						cellConfig.options)
				}));
	} else {
		return cell;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell = function (row) {
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		if (!cells.b) {
			return row;
		} else {
			var first = cells.a;
			var rest = cells.b;
			return $rundis$elm_bootstrap$Bootstrap$Table$Row(
				{
					cells: A2(
						$elm$core$List$cons,
						$rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first),
						rest),
					options: options
				});
		}
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		if (!cells.b) {
			return row;
		} else {
			var _v3 = cells.a;
			var firstKey = _v3.a;
			var first = _v3.b;
			var rest = cells.b;
			return $rundis$elm_bootstrap$Bootstrap$Table$KeyedRow(
				{
					cells: A2(
						$elm$core$List$cons,
						_Utils_Tuple2(
							firstKey,
							$rundis$elm_bootstrap$Bootstrap$Table$addScopeIfTh(first)),
						rest),
					options: options
				});
		}
	}
};
var $rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass = F2(
	function (prefix, role) {
		return $elm$html$Html$Attributes$class(
			prefix + ('-' + function () {
				switch (role.$) {
					case 'Primary':
						return 'primary';
					case 'Secondary':
						return 'secondary';
					case 'Success':
						return 'success';
					case 'Info':
						return 'info';
					case 'Warning':
						return 'warning';
					case 'Danger':
						return 'danger';
					case 'Light':
						return 'light';
					default:
						return 'dark';
				}
			}()));
	});
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttribute = function (option) {
	switch (option.$) {
		case 'RoledCell':
			if (option.a.$ === 'Roled') {
				var role = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role);
			} else {
				var _v1 = option.a;
				return $elm$html$Html$Attributes$class('table-active');
			}
		case 'InversedCell':
			if (option.a.$ === 'Roled') {
				var role = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg-', role);
			} else {
				var _v2 = option.a;
				return $elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$cellAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$cellAttribute, options);
};
var $elm$html$Html$td = _VirtualDom_node('td');
var $elm$html$Html$th = _VirtualDom_node('th');
var $rundis$elm_bootstrap$Bootstrap$Table$renderCell = function (cell) {
	if (cell.$ === 'Td') {
		var options = cell.a.options;
		var children = cell.a.children;
		return A2(
			$elm$html$Html$td,
			$rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	} else {
		var options = cell.a.options;
		var children = cell.a.children;
		return A2(
			$elm$html$Html$th,
			$rundis$elm_bootstrap$Bootstrap$Table$cellAttributes(options),
			children);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$rowClass = function (option) {
	switch (option.$) {
		case 'RoledRow':
			if (option.a.$ === 'Roled') {
				var role_ = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'table', role_);
			} else {
				var _v1 = option.a;
				return $elm$html$Html$Attributes$class('table-active');
			}
		case 'InversedRow':
			if (option.a.$ === 'Roled') {
				var role_ = option.a.a;
				return A2($rundis$elm_bootstrap$Bootstrap$Internal$Role$toClass, 'bg', role_);
			} else {
				var _v2 = option.a;
				return $elm$html$Html$Attributes$class('bg-active');
			}
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$rowAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$rowClass, options);
};
var $elm$html$Html$tr = _VirtualDom_node('tr');
var $rundis$elm_bootstrap$Bootstrap$Table$renderRow = function (row) {
	if (row.$ === 'Row') {
		var options = row.a.options;
		var cells = row.a.cells;
		return A2(
			$elm$html$Html$tr,
			$rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$renderCell, cells));
	} else {
		var options = row.a.options;
		var cells = row.a.cells;
		return A3(
			$elm$html$Html$Keyed$node,
			'tr',
			$rundis$elm_bootstrap$Bootstrap$Table$rowAttributes(options),
			A2(
				$elm$core$List$map,
				function (_v1) {
					var key = _v1.a;
					var cell = _v1.b;
					return _Utils_Tuple2(
						key,
						$rundis$elm_bootstrap$Bootstrap$Table$renderCell(cell));
				},
				cells));
	}
};
var $elm$html$Html$tbody = _VirtualDom_node('tbody');
var $rundis$elm_bootstrap$Bootstrap$Table$renderTBody = function (body) {
	if (body.$ === 'TBody') {
		var attributes = body.a.attributes;
		var rows = body.a.rows;
		return A2(
			$elm$html$Html$tbody,
			attributes,
			A2(
				$elm$core$List$map,
				function (row) {
					return $rundis$elm_bootstrap$Bootstrap$Table$renderRow(
						$rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row));
				},
				rows));
	} else {
		var attributes = body.a.attributes;
		var rows = body.a.rows;
		return A3(
			$elm$html$Html$Keyed$node,
			'tbody',
			attributes,
			A2(
				$elm$core$List$map,
				function (_v1) {
					var key = _v1.a;
					var row = _v1.b;
					return _Utils_Tuple2(
						key,
						$rundis$elm_bootstrap$Bootstrap$Table$renderRow(
							$rundis$elm_bootstrap$Bootstrap$Table$maybeAddScopeToFirstCell(row)));
				},
				rows));
	}
};
var $elm$html$Html$thead = _VirtualDom_node('thead');
var $rundis$elm_bootstrap$Bootstrap$Table$theadAttribute = function (option) {
	switch (option.$) {
		case 'InversedHead':
			return $elm$html$Html$Attributes$class('thead-dark');
		case 'DefaultHead':
			return $elm$html$Html$Attributes$class('thead-default');
		default:
			var attr_ = option.a;
			return attr_;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$theadAttributes = function (options) {
	return A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$theadAttribute, options);
};
var $rundis$elm_bootstrap$Bootstrap$Table$renderTHead = function (_v0) {
	var options = _v0.a.options;
	var rows = _v0.a.rows;
	return A2(
		$elm$html$Html$thead,
		$rundis$elm_bootstrap$Bootstrap$Table$theadAttributes(options),
		A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$renderRow, rows));
};
var $elm$html$Html$table = _VirtualDom_node('table');
var $rundis$elm_bootstrap$Bootstrap$Table$tableClass = function (option) {
	switch (option.$) {
		case 'Inversed':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-dark'));
		case 'Striped':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-striped'));
		case 'Bordered':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-bordered'));
		case 'Hover':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-hover'));
		case 'Small':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-sm'));
		case 'Responsive':
			return $elm$core$Maybe$Nothing;
		case 'Reflow':
			return $elm$core$Maybe$Just(
				$elm$html$Html$Attributes$class('table-reflow'));
		default:
			var attr_ = option.a;
			return $elm$core$Maybe$Just(attr_);
	}
};
var $rundis$elm_bootstrap$Bootstrap$Table$tableAttributes = function (options) {
	return A2(
		$elm$core$List$cons,
		$elm$html$Html$Attributes$class('table'),
		A2(
			$elm$core$List$filterMap,
			$elm$core$Basics$identity,
			A2($elm$core$List$map, $rundis$elm_bootstrap$Bootstrap$Table$tableClass, options)));
};
var $rundis$elm_bootstrap$Bootstrap$Table$table = function (rec) {
	var isInversed = A2(
		$elm$core$List$any,
		function (opt) {
			return _Utils_eq(opt, $rundis$elm_bootstrap$Bootstrap$Table$Inversed);
		},
		rec.options);
	var classOptions = A2(
		$elm$core$List$filter,
		function (opt) {
			return !$rundis$elm_bootstrap$Bootstrap$Table$isResponsive(opt);
		},
		rec.options);
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Table$maybeWrapResponsive,
		rec.options,
		A2(
			$elm$html$Html$table,
			$rundis$elm_bootstrap$Bootstrap$Table$tableAttributes(classOptions),
			_List_fromArray(
				[
					$rundis$elm_bootstrap$Bootstrap$Table$renderTHead(
					A2($rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTHead, isInversed, rec.thead)),
					$rundis$elm_bootstrap$Bootstrap$Table$renderTBody(
					A2($rundis$elm_bootstrap$Bootstrap$Table$maybeMapInversedTBody, isInversed, rec.tbody))
				])));
};
var $rundis$elm_bootstrap$Bootstrap$Table$tbody = F2(
	function (attributes, rows) {
		return $rundis$elm_bootstrap$Bootstrap$Table$TBody(
			{attributes: attributes, rows: rows});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Input = function (a) {
	return {$: 'Input', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$input = F2(
	function (inputFn, options) {
		return $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$Input(
			inputFn(options));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Text = {$: 'Text'};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Input = function (a) {
	return {$: 'Input', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$Type = function (a) {
	return {$: 'Type', a: a};
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$create = F2(
	function (tipe, options) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Input$Input(
			{
				options: A2(
					$elm$core$List$cons,
					$rundis$elm_bootstrap$Bootstrap$Form$Input$Type(tipe),
					options)
			});
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier = F2(
	function (modifier, options) {
		switch (modifier.$) {
			case 'Size':
				var size_ = modifier.a;
				return _Utils_update(
					options,
					{
						size: $elm$core$Maybe$Just(size_)
					});
			case 'Id':
				var id_ = modifier.a;
				return _Utils_update(
					options,
					{
						id: $elm$core$Maybe$Just(id_)
					});
			case 'Type':
				var tipe = modifier.a;
				return _Utils_update(
					options,
					{tipe: tipe});
			case 'Disabled':
				var val = modifier.a;
				return _Utils_update(
					options,
					{disabled: val});
			case 'Value':
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						value: $elm$core$Maybe$Just(value_)
					});
			case 'Placeholder':
				var value_ = modifier.a;
				return _Utils_update(
					options,
					{
						placeholder: $elm$core$Maybe$Just(value_)
					});
			case 'OnInput':
				var onInput_ = modifier.a;
				return _Utils_update(
					options,
					{
						onInput: $elm$core$Maybe$Just(onInput_)
					});
			case 'Validation':
				var validation_ = modifier.a;
				return _Utils_update(
					options,
					{
						validation: $elm$core$Maybe$Just(validation_)
					});
			case 'Readonly':
				var val = modifier.a;
				return _Utils_update(
					options,
					{readonly: val});
			case 'PlainText':
				var val = modifier.a;
				return _Utils_update(
					options,
					{plainText: val});
			default:
				var attrs_ = modifier.a;
				return _Utils_update(
					options,
					{
						attributes: _Utils_ap(options.attributes, attrs_)
					});
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions = {attributes: _List_Nil, disabled: false, id: $elm$core$Maybe$Nothing, onInput: $elm$core$Maybe$Nothing, placeholder: $elm$core$Maybe$Nothing, plainText: false, readonly: false, size: $elm$core$Maybe$Nothing, tipe: $rundis$elm_bootstrap$Bootstrap$Form$Input$Text, validation: $elm$core$Maybe$Nothing, value: $elm$core$Maybe$Nothing};
var $elm$html$Html$Events$alwaysStop = function (x) {
	return _Utils_Tuple2(x, true);
};
var $elm$virtual_dom$VirtualDom$MayStopPropagation = function (a) {
	return {$: 'MayStopPropagation', a: a};
};
var $elm$html$Html$Events$stopPropagationOn = F2(
	function (event, decoder) {
		return A2(
			$elm$virtual_dom$VirtualDom$on,
			event,
			$elm$virtual_dom$VirtualDom$MayStopPropagation(decoder));
	});
var $elm$html$Html$Events$onInput = function (tagger) {
	return A2(
		$elm$html$Html$Events$stopPropagationOn,
		'input',
		A2(
			$elm$json$Json$Decode$map,
			$elm$html$Html$Events$alwaysStop,
			A2($elm$json$Json$Decode$map, tagger, $elm$html$Html$Events$targetValue)));
};
var $elm$html$Html$Attributes$placeholder = $elm$html$Html$Attributes$stringProperty('placeholder');
var $elm$html$Html$Attributes$readonly = $elm$html$Html$Attributes$boolProperty('readOnly');
var $rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute = function (size) {
	return A2(
		$elm$core$Maybe$map,
		function (s) {
			return $elm$html$Html$Attributes$class('form-control-' + s);
		},
		$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute = function (inputType) {
	return $elm$html$Html$Attributes$type_(
		function () {
			switch (inputType.$) {
				case 'Text':
					return 'text';
				case 'Password':
					return 'password';
				case 'DatetimeLocal':
					return 'datetime-local';
				case 'Date':
					return 'date';
				case 'Month':
					return 'month';
				case 'Time':
					return 'time';
				case 'Week':
					return 'week';
				case 'Number':
					return 'number';
				case 'Email':
					return 'email';
				case 'Url':
					return 'url';
				case 'Search':
					return 'search';
				case 'Tel':
					return 'tel';
				default:
					return 'color';
			}
		}());
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute = function (validation) {
	return $elm$html$Html$Attributes$class(
		$rundis$elm_bootstrap$Bootstrap$Form$FormInternal$validationToString(validation));
};
var $elm$html$Html$Attributes$value = $elm$html$Html$Attributes$stringProperty('value');
var $rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes = function (modifiers) {
	var options = A3($elm$core$List$foldl, $rundis$elm_bootstrap$Bootstrap$Form$Input$applyModifier, $rundis$elm_bootstrap$Bootstrap$Form$Input$defaultOptions, modifiers);
	return _Utils_ap(
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class(
				options.plainText ? 'form-control-plaintext' : 'form-control'),
				$elm$html$Html$Attributes$disabled(options.disabled),
				$elm$html$Html$Attributes$readonly(options.readonly || options.plainText),
				$rundis$elm_bootstrap$Bootstrap$Form$Input$typeAttribute(options.tipe)
			]),
		_Utils_ap(
			A2(
				$elm$core$List$filterMap,
				$elm$core$Basics$identity,
				_List_fromArray(
					[
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$id, options.id),
						A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$Form$Input$sizeAttribute, options.size),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$value, options.value),
						A2($elm$core$Maybe$map, $elm$html$Html$Attributes$placeholder, options.placeholder),
						A2($elm$core$Maybe$map, $elm$html$Html$Events$onInput, options.onInput),
						A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Form$Input$validationAttribute, options.validation)
					])),
			options.attributes));
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$view = function (_v0) {
	var options = _v0.a.options;
	return A2(
		$elm$html$Html$input,
		$rundis$elm_bootstrap$Bootstrap$Form$Input$toAttributes(options),
		_List_Nil);
};
var $rundis$elm_bootstrap$Bootstrap$Form$Input$input = F2(
	function (tipe, options) {
		return $rundis$elm_bootstrap$Bootstrap$Form$Input$view(
			A2($rundis$elm_bootstrap$Bootstrap$Form$Input$create, tipe, options));
	});
var $rundis$elm_bootstrap$Bootstrap$Form$Input$text = $rundis$elm_bootstrap$Bootstrap$Form$Input$input($rundis$elm_bootstrap$Bootstrap$Form$Input$Text);
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text = $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$input($rundis$elm_bootstrap$Bootstrap$Form$Input$text);
var $rundis$elm_bootstrap$Bootstrap$Table$th = F2(
	function (options, children) {
		return $rundis$elm_bootstrap$Bootstrap$Table$Th(
			{children: children, options: options});
	});
var $author$project$Main$variableItem = function (lbl) {
	return A2(
		$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$value(lbl)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text(lbl)
			]));
};
var $author$project$Main$variableItems = function (model) {
	var varItems = function () {
		var _v0 = model.variables;
		if (_v0.$ === 'Nothing') {
			return _List_Nil;
		} else {
			var el = _v0.a;
			var varNames = A2($elm$core$List$map, $elm$core$Tuple$first, el);
			return A2($elm$core$List$map, $author$project$Main$variableItem, varNames);
		}
	}();
	var baseItems = _List_fromArray(
		[
			A2(
			$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
			_List_fromArray(
				[
					$elm$html$Html$Attributes$value('none')
				]),
			_List_fromArray(
				[
					$elm$html$Html$text('Not Selected')
				]))
		]);
	return _Utils_ap(baseItems, varItems);
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$sizeAttribute = function (size) {
	return A2(
		$elm$core$Maybe$map,
		function (s) {
			return $elm$html$Html$Attributes$class('input-group-' + s);
		},
		$rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size));
};
var $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view = function (_v0) {
	var conf = _v0.a;
	var _v1 = conf.input;
	var input_ = _v1.a;
	return A2(
		$elm$html$Html$div,
		_Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$class('input-group')
				]),
			_Utils_ap(
				A2(
					$elm$core$List$filterMap,
					$elm$core$Basics$identity,
					_List_fromArray(
						[
							A2($elm$core$Maybe$andThen, $rundis$elm_bootstrap$Bootstrap$Form$InputGroup$sizeAttribute, conf.size)
						])),
				conf.attributes)),
		_Utils_ap(
			A2(
				$elm$core$List$map,
				function (_v2) {
					var e = _v2.a;
					return A2(
						$elm$html$Html$div,
						_List_fromArray(
							[
								$elm$html$Html$Attributes$class('input-group-prepend')
							]),
						_List_fromArray(
							[e]));
				},
				conf.predecessors),
			_Utils_ap(
				_List_fromArray(
					[input_]),
				A2(
					$elm$core$List$map,
					function (_v3) {
						var e = _v3.a;
						return A2(
							$elm$html$Html$div,
							_List_fromArray(
								[
									$elm$html$Html$Attributes$class('input-group-append')
								]),
							_List_fromArray(
								[e]));
					},
					conf.successors))));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$StartClose = {$: 'StartClose'};
var $rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg = function (config_) {
	var _v0 = config_.withAnimation;
	if (_v0.$ === 'Just') {
		var animationMsg = _v0.a;
		return animationMsg($rundis$elm_bootstrap$Bootstrap$Modal$StartClose);
	} else {
		return config_.closeMsg;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Modal$isFade = function (conf) {
	return A2(
		$elm$core$Maybe$withDefault,
		false,
		A2(
			$elm$core$Maybe$map,
			function (_v0) {
				return true;
			},
			conf.withAnimation));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$backdrop = F2(
	function (visibility, conf) {
		var attributes = function () {
			switch (visibility.$) {
				case 'Show':
					return _Utils_ap(
						_List_fromArray(
							[
								$elm$html$Html$Attributes$classList(
								_List_fromArray(
									[
										_Utils_Tuple2('modal-backdrop', true),
										_Utils_Tuple2(
										'fade',
										$rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
										_Utils_Tuple2('show', true)
									]))
							]),
						conf.options.hideOnBackdropClick ? _List_fromArray(
							[
								$elm$html$Html$Events$onClick(
								$rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg(conf))
							]) : _List_Nil);
				case 'StartClose':
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', true),
									_Utils_Tuple2('fade', true),
									_Utils_Tuple2('show', true)
								]))
						]);
				case 'FadeClose':
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', true),
									_Utils_Tuple2('fade', true),
									_Utils_Tuple2('show', false)
								]))
						]);
				default:
					return _List_fromArray(
						[
							$elm$html$Html$Attributes$classList(
							_List_fromArray(
								[
									_Utils_Tuple2('modal-backdrop', false),
									_Utils_Tuple2(
									'fade',
									$rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
									_Utils_Tuple2('show', false)
								]))
						]);
			}
		}();
		return _List_fromArray(
			[
				A2($elm$html$Html$div, attributes, _List_Nil)
			]);
	});
var $rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target = function (decoder) {
	return A2($elm$json$Json$Decode$field, 'target', decoder);
};
var $rundis$elm_bootstrap$Bootstrap$Modal$containerClickDecoder = function (closeMsg) {
	return A2(
		$elm$json$Json$Decode$andThen,
		function (c) {
			return A2($elm$core$String$contains, 'elm-bootstrap-modal', c) ? $elm$json$Json$Decode$succeed(closeMsg) : $elm$json$Json$Decode$fail('ignoring');
		},
		$rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$target($rundis$elm_bootstrap$Bootstrap$Utilities$DomHelper$className));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$display = F2(
	function (visibility, conf) {
		switch (visibility.$) {
			case 'Show':
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2(
								'fade',
								$rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
								_Utils_Tuple2('show', true)
							]))
					]);
			case 'StartClose':
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2('fade', true),
								_Utils_Tuple2('show', true)
							]))
					]);
			case 'FadeClose':
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'pointer-events', 'none'),
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2('fade', true),
								_Utils_Tuple2('show', false)
							])),
						A2(
						$elm$html$Html$Events$on,
						'transitionend',
						$elm$json$Json$Decode$succeed(conf.closeMsg))
					]);
			default:
				return _List_fromArray(
					[
						A2($elm$html$Html$Attributes$style, 'height', '0px'),
						A2($elm$html$Html$Attributes$style, 'display', 'block'),
						$elm$html$Html$Attributes$classList(
						_List_fromArray(
							[
								_Utils_Tuple2('modal', true),
								_Utils_Tuple2(
								'fade',
								$rundis$elm_bootstrap$Bootstrap$Modal$isFade(conf)),
								_Utils_Tuple2('show', false)
							]))
					]);
		}
	});
var $rundis$elm_bootstrap$Bootstrap$Modal$modalClass = function (size) {
	var _v0 = $rundis$elm_bootstrap$Bootstrap$General$Internal$screenSizeOption(size);
	if (_v0.$ === 'Just') {
		var s = _v0.a;
		return _List_fromArray(
			[
				$elm$html$Html$Attributes$class('modal-' + s)
			]);
	} else {
		return _List_Nil;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Modal$modalAttributes = function (options) {
	return _Utils_ap(
		options.attrs,
		_Utils_ap(
			_List_fromArray(
				[
					$elm$html$Html$Attributes$classList(
					_List_fromArray(
						[
							_Utils_Tuple2('modal-dialog', true),
							_Utils_Tuple2('modal-dialog-centered', options.centered),
							_Utils_Tuple2('modal-dialog-scrollable', options.scrollableBody)
						])),
					A2($elm$html$Html$Attributes$style, 'pointer-events', 'auto')
				]),
			A2(
				$elm$core$Maybe$withDefault,
				_List_Nil,
				A2($elm$core$Maybe$map, $rundis$elm_bootstrap$Bootstrap$Modal$modalClass, options.modalSize))));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$renderBody = function (maybeBody) {
	if (maybeBody.$ === 'Just') {
		var cfg = maybeBody.a.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('modal-body'),
					cfg.attributes),
				cfg.children));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Modal$renderFooter = function (maybeFooter) {
	if (maybeFooter.$ === 'Just') {
		var cfg = maybeFooter.a.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('modal-footer'),
					cfg.attributes),
				cfg.children));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $rundis$elm_bootstrap$Bootstrap$Modal$closeButton = function (closeMsg) {
	return A2(
		$elm$html$Html$button,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$class('close'),
				$elm$html$Html$Events$onClick(closeMsg)
			]),
		_List_fromArray(
			[
				$elm$html$Html$text('×')
			]));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$renderHeader = function (conf_) {
	var _v0 = conf_.header;
	if (_v0.$ === 'Just') {
		var cfg = _v0.a.a;
		return $elm$core$Maybe$Just(
			A2(
				$elm$html$Html$div,
				A2(
					$elm$core$List$cons,
					$elm$html$Html$Attributes$class('modal-header'),
					cfg.attributes),
				_Utils_ap(
					cfg.children,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$Modal$closeButton(
							$rundis$elm_bootstrap$Bootstrap$Modal$getCloseMsg(conf_))
						]))));
	} else {
		return $elm$core$Maybe$Nothing;
	}
};
var $elm$html$Html$Attributes$tabindex = function (n) {
	return A2(
		_VirtualDom_attribute,
		'tabIndex',
		$elm$core$String$fromInt(n));
};
var $rundis$elm_bootstrap$Bootstrap$Modal$view = F2(
	function (visibility, _v0) {
		var conf = _v0.a;
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_Utils_ap(
				_List_fromArray(
					[
						A2(
						$elm$html$Html$div,
						_Utils_ap(
							_List_fromArray(
								[
									$elm$html$Html$Attributes$tabindex(-1)
								]),
							A2($rundis$elm_bootstrap$Bootstrap$Modal$display, visibility, conf)),
						_List_fromArray(
							[
								A2(
								$elm$html$Html$div,
								_Utils_ap(
									_List_fromArray(
										[
											A2($elm$html$Html$Attributes$attribute, 'role', 'document'),
											$elm$html$Html$Attributes$class('elm-bootstrap-modal')
										]),
									_Utils_ap(
										$rundis$elm_bootstrap$Bootstrap$Modal$modalAttributes(conf.options),
										conf.options.hideOnBackdropClick ? _List_fromArray(
											[
												A2(
												$elm$html$Html$Events$on,
												'click',
												$rundis$elm_bootstrap$Bootstrap$Modal$containerClickDecoder(conf.closeMsg))
											]) : _List_Nil)),
								_List_fromArray(
									[
										A2(
										$elm$html$Html$div,
										_List_fromArray(
											[
												$elm$html$Html$Attributes$class('modal-content')
											]),
										A2(
											$elm$core$List$filterMap,
											$elm$core$Basics$identity,
											_List_fromArray(
												[
													$rundis$elm_bootstrap$Bootstrap$Modal$renderHeader(conf),
													$rundis$elm_bootstrap$Bootstrap$Modal$renderBody(conf.body),
													$rundis$elm_bootstrap$Bootstrap$Modal$renderFooter(conf.footer)
												])))
									]))
							]))
					]),
				A2($rundis$elm_bootstrap$Bootstrap$Modal$backdrop, visibility, conf)));
	});
var $author$project$Main$dataEntryInputGroupView = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A2(
				$elm$html$Html$h4,
				_List_Nil,
				_List_fromArray(
					[
						$elm$html$Html$text('Data Selection')
					])),
				$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view(
				A2(
					$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$dropdown,
							model.dataDropState,
							{
								items: $author$project$Main$dropdownData(model.datasets),
								options: _List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Dropdown$alignMenuRight]),
								toggleButton: A2(
									$rundis$elm_bootstrap$Bootstrap$Dropdown$toggle,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary, $rundis$elm_bootstrap$Bootstrap$Button$small]),
									_List_Nil),
								toggleMsg: $author$project$Main$DataDropMsg
							})
						]),
					A2(
						$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Data')
											]))
									]))
							]),
						$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$small(
							$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config(
								$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text(
									_List_fromArray(
										[
											$rundis$elm_bootstrap$Bootstrap$Form$Input$placeholder(
											$author$project$Main$dataPlaceholder(model)),
											$rundis$elm_bootstrap$Bootstrap$Form$Input$disabled(true)
										]))))))),
				$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$view(
				A2(
					$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$successors,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$dropdown,
							model.successDropState,
							{
								items: $author$project$Main$dropdownSuccess(model),
								options: _List_fromArray(
									[$rundis$elm_bootstrap$Bootstrap$Dropdown$alignMenuRight]),
								toggleButton: $author$project$Main$successButtonToggle(model),
								toggleMsg: $author$project$Main$SuccessDropMsg
							})
						]),
					A2(
						$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$predecessors,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$span,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$elm$html$Html$b,
										_List_Nil,
										_List_fromArray(
											[
												$elm$html$Html$text('Success')
											]))
									]))
							]),
						$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$small(
							$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$config(
								$rundis$elm_bootstrap$Bootstrap$Form$InputGroup$text(
									_List_fromArray(
										[
											$rundis$elm_bootstrap$Bootstrap$Form$Input$placeholder(
											$author$project$Main$successPlaceholder(model)),
											$rundis$elm_bootstrap$Bootstrap$Form$Input$disabled(true)
										]))))))),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Button$button,
				_List_fromArray(
					[
						$rundis$elm_bootstrap$Bootstrap$Button$small,
						$rundis$elm_bootstrap$Bootstrap$Button$primary,
						$rundis$elm_bootstrap$Bootstrap$Button$attrs(
						_List_fromArray(
							[$rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$ml1])),
						$rundis$elm_bootstrap$Bootstrap$Button$onClick($author$project$Main$ShowModal)
					]),
				_List_fromArray(
					[
						$elm$html$Html$text('Upload CSV')
					])),
				A2(
				$rundis$elm_bootstrap$Bootstrap$Modal$view,
				model.modalVisibility,
				A3(
					$rundis$elm_bootstrap$Bootstrap$Modal$footer,
					_List_Nil,
					_List_fromArray(
						[
							A2(
							$rundis$elm_bootstrap$Bootstrap$Button$button,
							_List_fromArray(
								[
									$rundis$elm_bootstrap$Bootstrap$Button$outlinePrimary,
									$rundis$elm_bootstrap$Bootstrap$Button$attrs(
									_List_fromArray(
										[
											$elm$html$Html$Events$onClick($author$project$Main$CloseModal)
										]))
								]),
							_List_fromArray(
								[
									$elm$html$Html$text('Close')
								]))
						]),
					A3(
						$rundis$elm_bootstrap$Bootstrap$Modal$body,
						_List_Nil,
						_List_fromArray(
							[
								A2(
								$rundis$elm_bootstrap$Bootstrap$Grid$container,
								_List_Nil,
								_List_fromArray(
									[
										A2(
										$rundis$elm_bootstrap$Bootstrap$Form$form,
										_List_Nil,
										_List_fromArray(
											[
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																$elm$html$Html$text('Select File')
															])),
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																A2(
																$rundis$elm_bootstrap$Bootstrap$Button$button,
																_List_fromArray(
																	[
																		$rundis$elm_bootstrap$Bootstrap$Button$small,
																		$rundis$elm_bootstrap$Bootstrap$Button$primary,
																		$rundis$elm_bootstrap$Bootstrap$Button$attrs(
																		_List_fromArray(
																			[$rundis$elm_bootstrap$Bootstrap$Utilities$Spacing$ml1])),
																		$rundis$elm_bootstrap$Bootstrap$Button$onClick($author$project$Main$CsvRequested)
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('CSV File')
																	]))
															]))
													])),
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																$elm$html$Html$text('File Name')
															])),
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6]),
														_List_fromArray(
															[
																$elm$html$Html$text(model.fileName)
															]))
													])),
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																$elm$html$Html$text('File Type')
															])),
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6]),
														_List_fromArray(
															[
																A2(
																$rundis$elm_bootstrap$Bootstrap$Form$Select$custom,
																_List_fromArray(
																	[
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$disabled(model.fileName === 'None'),
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$id('myselect'),
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$onChange($author$project$Main$ChangeFileType)
																	]),
																_List_fromArray(
																	[
																		A2(
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$value('none')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Not Selected')
																			])),
																		A2(
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$value('reg')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Regular')
																			])),
																		A2(
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$item,
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$value('freq')
																			]),
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Frequency')
																			]))
																	]))
															]))
													])),
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																$elm$html$Html$text('Variable')
															])),
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6]),
														_List_fromArray(
															[
																A2(
																$rundis$elm_bootstrap$Bootstrap$Form$Select$custom,
																_List_fromArray(
																	[
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$disabled(
																		_Utils_eq(model.fileType, $author$project$Main$NoType)),
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$onChange($author$project$Main$SelectVariable)
																	]),
																$author$project$Main$variableItems(model))
															]))
													])),
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_Nil,
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$colLabel,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm4]),
														_List_fromArray(
															[
																$elm$html$Html$text('Counts')
															])),
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm6]),
														_List_fromArray(
															[
																A2(
																$rundis$elm_bootstrap$Bootstrap$Form$Select$custom,
																_List_fromArray(
																	[
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$disabled(
																		!_Utils_eq(model.fileType, $author$project$Main$Frequency)),
																		$rundis$elm_bootstrap$Bootstrap$Form$Select$onChange($author$project$Main$SelectCount)
																	]),
																$author$project$Main$variableItems(model))
															]))
													])),
												function () {
												var _v0 = model.perspectiveData;
												if (_v0.$ === 'Nothing') {
													return A2($elm$html$Html$div, _List_Nil, _List_Nil);
												} else {
													var data = _v0.a;
													return $rundis$elm_bootstrap$Bootstrap$Table$table(
														{
															options: _List_fromArray(
																[$rundis$elm_bootstrap$Bootstrap$Table$small, $rundis$elm_bootstrap$Bootstrap$Table$striped, $rundis$elm_bootstrap$Bootstrap$Table$bordered]),
															tbody: A2(
																$rundis$elm_bootstrap$Bootstrap$Table$tbody,
																_List_Nil,
																A2($elm$core$List$map, $author$project$Main$makeTableRow, data.frequencies)),
															thead: $rundis$elm_bootstrap$Bootstrap$Table$simpleThead(
																_List_fromArray(
																	[
																		A2(
																		$rundis$elm_bootstrap$Bootstrap$Table$th,
																		_List_Nil,
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Label')
																			])),
																		A2(
																		$rundis$elm_bootstrap$Bootstrap$Table$th,
																		_List_Nil,
																		_List_fromArray(
																			[
																				$elm$html$Html$text('Count')
																			]))
																	]))
														});
												}
											}(),
												A2(
												$rundis$elm_bootstrap$Bootstrap$Form$row,
												_List_fromArray(
													[$rundis$elm_bootstrap$Bootstrap$Grid$Row$rightSm]),
												_List_fromArray(
													[
														A2(
														$rundis$elm_bootstrap$Bootstrap$Form$col,
														_List_fromArray(
															[$rundis$elm_bootstrap$Bootstrap$Grid$Col$sm2]),
														_List_fromArray(
															[
																A2(
																$rundis$elm_bootstrap$Bootstrap$Button$button,
																_List_fromArray(
																	[
																		$rundis$elm_bootstrap$Bootstrap$Button$primary,
																		$rundis$elm_bootstrap$Bootstrap$Button$attrs(
																		_List_fromArray(
																			[
																				$elm$html$Html$Attributes$class('float-right')
																			])),
																		$rundis$elm_bootstrap$Bootstrap$Button$disabled(
																		_Utils_eq(model.perspectiveData, $elm$core$Maybe$Nothing)),
																		$rundis$elm_bootstrap$Bootstrap$Button$onClick($author$project$Main$LoadUserData)
																	]),
																_List_fromArray(
																	[
																		$elm$html$Html$text('Load')
																	]))
															]))
													]))
											]))
									]))
							]),
						A3(
							$rundis$elm_bootstrap$Bootstrap$Modal$h5,
							_List_Nil,
							_List_fromArray(
								[
									$elm$html$Html$text('Upload CSV data')
								]),
							$rundis$elm_bootstrap$Bootstrap$Modal$config($author$project$Main$CloseModal))))),
				A2($elm$html$Html$br, _List_Nil, _List_Nil)
			]));
};
var $author$project$Main$distPlotView = function (model) {
	return _Utils_eq(model.originalSample, $elm$core$Maybe$Nothing) ? A2($elm$html$Html$div, _List_Nil, _List_Nil) : A2(
		$elm$html$Html$div,
		_List_fromArray(
			[
				$elm$html$Html$Attributes$id('distPlot')
			]),
		_List_Nil);
};
var $elm$html$Html$h3 = _VirtualDom_node('h3');
var $rundis$elm_bootstrap$Bootstrap$General$Internal$MD = {$: 'MD'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$md12 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$MD, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col12);
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$md3 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$MD, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col3);
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$md4 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$MD, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col4);
var $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col5 = {$: 'Col5'};
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$md5 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$MD, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col5);
var $rundis$elm_bootstrap$Bootstrap$Grid$Col$md8 = A2($rundis$elm_bootstrap$Bootstrap$Grid$Internal$width, $rundis$elm_bootstrap$Bootstrap$General$Internal$MD, $rundis$elm_bootstrap$Bootstrap$Grid$Internal$Col8);
var $elm$html$Html$Attributes$href = function (url) {
	return A2(
		$elm$html$Html$Attributes$stringProperty,
		'href',
		_VirtualDom_noJavaScriptUri(url));
};
var $elm$virtual_dom$VirtualDom$node = function (tag) {
	return _VirtualDom_node(
		_VirtualDom_noScript(tag));
};
var $elm$html$Html$node = $elm$virtual_dom$VirtualDom$node;
var $elm$html$Html$Attributes$rel = _VirtualDom_attribute('rel');
var $rundis$elm_bootstrap$Bootstrap$CDN$stylesheet = A3(
	$elm$html$Html$node,
	'link',
	_List_fromArray(
		[
			$elm$html$Html$Attributes$rel('stylesheet'),
			$elm$html$Html$Attributes$href('https://stackpath.bootstrapcdn.com/bootstrap/4.3.1/css/bootstrap.min.css')
		]),
	_List_Nil);
var $author$project$Main$mainGrid = F7(
	function (dataEntry, originalSample, bootstrapSample, collectButtons, distPlot, confLimits, debug) {
		return A2(
			$elm$html$Html$div,
			_List_Nil,
			_List_fromArray(
				[
					A2(
					$rundis$elm_bootstrap$Bootstrap$Grid$container,
					_List_Nil,
					_List_fromArray(
						[
							$rundis$elm_bootstrap$Bootstrap$CDN$stylesheet,
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md12]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$h3,
											_List_Nil,
											_List_fromArray(
												[
													$elm$html$Html$text('Bootstrap Confidence Interval for Categorical Data')
												])),
											A2($elm$html$Html$br, _List_Nil, _List_Nil)
										]))
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md3]),
									_List_fromArray(
										[dataEntry])),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md4]),
									_List_fromArray(
										[collectButtons])),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md5]),
									_List_fromArray(
										[confLimits]))
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md4]),
									_List_fromArray(
										[
											A2(
											$elm$html$Html$div,
											_List_Nil,
											_List_fromArray(
												[
													originalSample,
													A2($elm$html$Html$br, _List_Nil, _List_Nil),
													bootstrapSample
												]))
										])),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md8]),
									_List_fromArray(
										[distPlot]))
								])),
							A2(
							$rundis$elm_bootstrap$Bootstrap$Grid$row,
							_List_Nil,
							_List_fromArray(
								[
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md4]),
									_List_Nil),
									A2(
									$rundis$elm_bootstrap$Bootstrap$Grid$col,
									_List_fromArray(
										[$rundis$elm_bootstrap$Bootstrap$Grid$Col$md8]),
									_List_fromArray(
										[debug]))
								]))
						]))
				]));
	});
var $author$project$Main$originalSampleView = function (model) {
	var _v0 = model.originalSample;
	if (_v0.$ === 'Nothing') {
		return A2($elm$html$Html$div, _List_Nil, _List_Nil);
	} else {
		return A3(
			$author$project$Main$sampleGrid,
			'Original Sample',
			$author$project$Main$statisticView(model.originalSample),
			A2(
				$elm$html$Html$div,
				_List_fromArray(
					[
						$elm$html$Html$Attributes$id('samplePlot')
					]),
				_List_Nil));
	}
};
var $author$project$Main$view = function (model) {
	return A2(
		$elm$html$Html$div,
		_List_Nil,
		_List_fromArray(
			[
				A7(
				$author$project$Main$mainGrid,
				$author$project$Main$dataEntryInputGroupView(model),
				$author$project$Main$originalSampleView(model),
				$author$project$Main$bootstrapSampleView(model),
				$author$project$Main$collectButtonView(model),
				$author$project$Main$distPlotView(model),
				$author$project$Main$confLimitsView(model),
				A2($elm$html$Html$div, _List_Nil, _List_Nil))
			]));
};
var $author$project$Main$main = $elm$browser$Browser$element(
	{init: $author$project$Main$init, subscriptions: $author$project$Main$subscriptions, update: $author$project$Main$update, view: $author$project$Main$view});
_Platform_export({'Main':{'init':$author$project$Main$main(
	$elm$json$Json$Decode$succeed(_Utils_Tuple0))(0)}});}(this));