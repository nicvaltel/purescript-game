(() => {
  // output/Effect.Console/foreign.js
  var log = function(s) {
    return function() {
      console.log(s);
    };
  };

  // output/Data.Show/foreign.js
  var showIntImpl = function(n) {
    return n.toString();
  };
  var showNumberImpl = function(n) {
    var str = n.toString();
    return isNaN(str + ".0") ? str : str + ".0";
  };
  var showStringImpl = function(s) {
    var l = s.length;
    return '"' + s.replace(
      /[\0-\x1F\x7F"\\]/g,
      // eslint-disable-line no-control-regex
      function(c, i) {
        switch (c) {
          case '"':
          case "\\":
            return "\\" + c;
          case "\x07":
            return "\\a";
          case "\b":
            return "\\b";
          case "\f":
            return "\\f";
          case "\n":
            return "\\n";
          case "\r":
            return "\\r";
          case "	":
            return "\\t";
          case "\v":
            return "\\v";
        }
        var k = i + 1;
        var empty2 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
        return "\\" + c.charCodeAt(0).toString(10) + empty2;
      }
    ) + '"';
  };
  var showArrayImpl = function(f) {
    return function(xs) {
      var ss = [];
      for (var i = 0, l = xs.length; i < l; i++) {
        ss[i] = f(xs[i]);
      }
      return "[" + ss.join(",") + "]";
    };
  };

  // output/Type.Proxy/index.js
  var $$Proxy = /* @__PURE__ */ function() {
    function $$Proxy2() {
    }
    ;
    $$Proxy2.value = new $$Proxy2();
    return $$Proxy2;
  }();

  // output/Data.Symbol/index.js
  var reflectSymbol = function(dict) {
    return dict.reflectSymbol;
  };

  // output/Record.Unsafe/foreign.js
  var unsafeGet = function(label) {
    return function(rec) {
      return rec[label];
    };
  };

  // output/Data.Show/index.js
  var showString = {
    show: showStringImpl
  };
  var showRecordFields = function(dict) {
    return dict.showRecordFields;
  };
  var showRecord = function() {
    return function() {
      return function(dictShowRecordFields) {
        var showRecordFields1 = showRecordFields(dictShowRecordFields);
        return {
          show: function(record) {
            return "{" + (showRecordFields1($$Proxy.value)(record) + "}");
          }
        };
      };
    };
  };
  var showNumber = {
    show: showNumberImpl
  };
  var showInt = {
    show: showIntImpl
  };
  var show = function(dict) {
    return dict.show;
  };
  var showArray = function(dictShow) {
    return {
      show: showArrayImpl(show(dictShow))
    };
  };
  var showRecordFieldsCons = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShowRecordFields) {
      var showRecordFields1 = showRecordFields(dictShowRecordFields);
      return function(dictShow) {
        var show1 = show(dictShow);
        return {
          showRecordFields: function(v) {
            return function(record) {
              var tail2 = showRecordFields1($$Proxy.value)(record);
              var key = reflectSymbol2($$Proxy.value);
              var focus = unsafeGet(key)(record);
              return " " + (key + (": " + (show1(focus) + ("," + tail2))));
            };
          }
        };
      };
    };
  };
  var showRecordFieldsConsNil = function(dictIsSymbol) {
    var reflectSymbol2 = reflectSymbol(dictIsSymbol);
    return function(dictShow) {
      var show1 = show(dictShow);
      return {
        showRecordFields: function(v) {
          return function(record) {
            var key = reflectSymbol2($$Proxy.value);
            var focus = unsafeGet(key)(record);
            return " " + (key + (": " + (show1(focus) + " ")));
          };
        }
      };
    };
  };

  // output/Control.Semigroupoid/index.js
  var semigroupoidFn = {
    compose: function(f) {
      return function(g) {
        return function(x) {
          return f(g(x));
        };
      };
    }
  };

  // output/Control.Category/index.js
  var identity = function(dict) {
    return dict.identity;
  };
  var categoryFn = {
    identity: function(x) {
      return x;
    },
    Semigroupoid0: function() {
      return semigroupoidFn;
    }
  };

  // output/Data.Boolean/index.js
  var otherwise = true;

  // output/Data.Functor/index.js
  var map = function(dict) {
    return dict.map;
  };

  // output/Control.Apply/index.js
  var apply = function(dict) {
    return dict.apply;
  };

  // output/Control.Applicative/index.js
  var pure = function(dict) {
    return dict.pure;
  };
  var liftA1 = function(dictApplicative) {
    var apply2 = apply(dictApplicative.Apply0());
    var pure1 = pure(dictApplicative);
    return function(f) {
      return function(a) {
        return apply2(pure1(f))(a);
      };
    };
  };

  // output/Data.Array/foreign.js
  var replicateFill = function(count, value) {
    if (count < 1) {
      return [];
    }
    var result = new Array(count);
    return result.fill(value);
  };
  var replicatePolyfill = function(count, value) {
    var result = [];
    var n = 0;
    for (var i = 0; i < count; i++) {
      result[n++] = value;
    }
    return result;
  };
  var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
  var fromFoldableImpl = function() {
    function Cons2(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    }
    var emptyList = {};
    function curryCons(head3) {
      return function(tail2) {
        return new Cons2(head3, tail2);
      };
    }
    function listToArray(list) {
      var result = [];
      var count = 0;
      var xs = list;
      while (xs !== emptyList) {
        result[count++] = xs.head;
        xs = xs.tail;
      }
      return result;
    }
    return function(foldr2, xs) {
      return listToArray(foldr2(curryCons)(emptyList)(xs));
    };
  }();
  var sortByImpl = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2, fromOrdering, xs) {
      var out;
      if (xs.length < 2)
        return xs;
      out = xs.slice(0);
      mergeFromTo(compare2, fromOrdering, out, xs.slice(0), 0, xs.length);
      return out;
    };
  }();

  // output/Data.Semigroup/foreign.js
  var concatArray = function(xs) {
    return function(ys) {
      if (xs.length === 0)
        return ys;
      if (ys.length === 0)
        return xs;
      return xs.concat(ys);
    };
  };

  // output/Data.Semigroup/index.js
  var semigroupArray = {
    append: concatArray
  };
  var append = function(dict) {
    return dict.append;
  };

  // output/Control.Bind/index.js
  var bind = function(dict) {
    return dict.bind;
  };

  // output/Control.Monad/index.js
  var ap = function(dictMonad) {
    var bind2 = bind(dictMonad.Bind1());
    var pure3 = pure(dictMonad.Applicative0());
    return function(f) {
      return function(a) {
        return bind2(f)(function(f$prime) {
          return bind2(a)(function(a$prime) {
            return pure3(f$prime(a$prime));
          });
        });
      };
    };
  };

  // output/Data.Bounded/foreign.js
  var topInt = 2147483647;
  var bottomInt = -2147483648;
  var topChar = String.fromCharCode(65535);
  var bottomChar = String.fromCharCode(0);
  var topNumber = Number.POSITIVE_INFINITY;
  var bottomNumber = Number.NEGATIVE_INFINITY;

  // output/Data.Ord/foreign.js
  var unsafeCompareImpl = function(lt) {
    return function(eq2) {
      return function(gt) {
        return function(x) {
          return function(y) {
            return x < y ? lt : x === y ? eq2 : gt;
          };
        };
      };
    };
  };
  var ordIntImpl = unsafeCompareImpl;

  // output/Data.Eq/foreign.js
  var refEq = function(r1) {
    return function(r2) {
      return r1 === r2;
    };
  };
  var eqIntImpl = refEq;

  // output/Data.Eq/index.js
  var eqInt = {
    eq: eqIntImpl
  };

  // output/Data.Ordering/index.js
  var LT = /* @__PURE__ */ function() {
    function LT2() {
    }
    ;
    LT2.value = new LT2();
    return LT2;
  }();
  var GT = /* @__PURE__ */ function() {
    function GT2() {
    }
    ;
    GT2.value = new GT2();
    return GT2;
  }();
  var EQ = /* @__PURE__ */ function() {
    function EQ2() {
    }
    ;
    EQ2.value = new EQ2();
    return EQ2;
  }();

  // output/Data.Ring/foreign.js
  var intSub = function(x) {
    return function(y) {
      return x - y | 0;
    };
  };

  // output/Data.Semiring/foreign.js
  var intAdd = function(x) {
    return function(y) {
      return x + y | 0;
    };
  };
  var intMul = function(x) {
    return function(y) {
      return x * y | 0;
    };
  };

  // output/Data.Semiring/index.js
  var semiringInt = {
    add: intAdd,
    zero: 0,
    mul: intMul,
    one: 1
  };

  // output/Data.Ring/index.js
  var ringInt = {
    sub: intSub,
    Semiring0: function() {
      return semiringInt;
    }
  };

  // output/Data.Ord/index.js
  var ordInt = /* @__PURE__ */ function() {
    return {
      compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
      Eq0: function() {
        return eqInt;
      }
    };
  }();

  // output/Data.Bounded/index.js
  var top = function(dict) {
    return dict.top;
  };
  var boundedInt = {
    top: topInt,
    bottom: bottomInt,
    Ord0: function() {
      return ordInt;
    }
  };
  var bottom = function(dict) {
    return dict.bottom;
  };

  // output/Data.Maybe/index.js
  var identity2 = /* @__PURE__ */ identity(categoryFn);
  var Nothing = /* @__PURE__ */ function() {
    function Nothing2() {
    }
    ;
    Nothing2.value = new Nothing2();
    return Nothing2;
  }();
  var Just = /* @__PURE__ */ function() {
    function Just2(value0) {
      this.value0 = value0;
    }
    ;
    Just2.create = function(value0) {
      return new Just2(value0);
    };
    return Just2;
  }();
  var maybe = function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v;
        }
        ;
        if (v2 instanceof Just) {
          return v1(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Maybe (line 237, column 1 - line 237, column 51): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  };
  var fromMaybe = function(a) {
    return maybe(a)(identity2);
  };

  // output/Data.Identity/index.js
  var Identity = function(x) {
    return x;
  };
  var functorIdentity = {
    map: function(f) {
      return function(m) {
        return f(m);
      };
    }
  };
  var applyIdentity = {
    apply: function(v) {
      return function(v1) {
        return v(v1);
      };
    },
    Functor0: function() {
      return functorIdentity;
    }
  };
  var bindIdentity = {
    bind: function(v) {
      return function(f) {
        return f(v);
      };
    },
    Apply0: function() {
      return applyIdentity;
    }
  };
  var applicativeIdentity = {
    pure: Identity,
    Apply0: function() {
      return applyIdentity;
    }
  };
  var monadIdentity = {
    Applicative0: function() {
      return applicativeIdentity;
    },
    Bind1: function() {
      return bindIdentity;
    }
  };

  // output/Data.EuclideanRing/foreign.js
  var intDegree = function(x) {
    return Math.min(Math.abs(x), 2147483647);
  };
  var intDiv = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      return y > 0 ? Math.floor(x / y) : -Math.floor(x / -y);
    };
  };
  var intMod = function(x) {
    return function(y) {
      if (y === 0)
        return 0;
      var yy = Math.abs(y);
      return (x % yy + yy) % yy;
    };
  };

  // output/Data.CommutativeRing/index.js
  var commutativeRingInt = {
    Ring0: function() {
      return ringInt;
    }
  };

  // output/Data.EuclideanRing/index.js
  var mod = function(dict) {
    return dict.mod;
  };
  var euclideanRingInt = {
    degree: intDegree,
    div: intDiv,
    mod: intMod,
    CommutativeRing0: function() {
      return commutativeRingInt;
    }
  };

  // output/Effect/foreign.js
  var pureE = function(a) {
    return function() {
      return a;
    };
  };
  var bindE = function(a) {
    return function(f) {
      return function() {
        return f(a())();
      };
    };
  };

  // output/Effect/index.js
  var $runtime_lazy = function(name2, moduleName, init2) {
    var state2 = 0;
    var val;
    return function(lineNumber) {
      if (state2 === 2)
        return val;
      if (state2 === 1)
        throw new ReferenceError(name2 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
      state2 = 1;
      val = init2();
      state2 = 2;
      return val;
    };
  };
  var monadEffect = {
    Applicative0: function() {
      return applicativeEffect;
    },
    Bind1: function() {
      return bindEffect;
    }
  };
  var bindEffect = {
    bind: bindE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var applicativeEffect = {
    pure: pureE,
    Apply0: function() {
      return $lazy_applyEffect(0);
    }
  };
  var $lazy_functorEffect = /* @__PURE__ */ $runtime_lazy("functorEffect", "Effect", function() {
    return {
      map: liftA1(applicativeEffect)
    };
  });
  var $lazy_applyEffect = /* @__PURE__ */ $runtime_lazy("applyEffect", "Effect", function() {
    return {
      apply: ap(monadEffect),
      Functor0: function() {
        return $lazy_functorEffect(0);
      }
    };
  });
  var functorEffect = /* @__PURE__ */ $lazy_functorEffect(20);

  // output/Data.Array.ST/foreign.js
  var sortByImpl2 = function() {
    function mergeFromTo(compare2, fromOrdering, xs1, xs2, from, to) {
      var mid;
      var i;
      var j;
      var k;
      var x;
      var y;
      var c;
      mid = from + (to - from >> 1);
      if (mid - from > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, from, mid);
      if (to - mid > 1)
        mergeFromTo(compare2, fromOrdering, xs2, xs1, mid, to);
      i = from;
      j = mid;
      k = from;
      while (i < mid && j < to) {
        x = xs2[i];
        y = xs2[j];
        c = fromOrdering(compare2(x)(y));
        if (c > 0) {
          xs1[k++] = y;
          ++j;
        } else {
          xs1[k++] = x;
          ++i;
        }
      }
      while (i < mid) {
        xs1[k++] = xs2[i++];
      }
      while (j < to) {
        xs1[k++] = xs2[j++];
      }
    }
    return function(compare2, fromOrdering, xs) {
      if (xs.length < 2)
        return xs;
      mergeFromTo(compare2, fromOrdering, xs, xs.slice(0), 0, xs.length);
      return xs;
    };
  }();

  // output/Data.Tuple/index.js
  var Tuple = /* @__PURE__ */ function() {
    function Tuple2(value0, value1) {
      this.value0 = value0;
      this.value1 = value1;
    }
    ;
    Tuple2.create = function(value0) {
      return function(value1) {
        return new Tuple2(value0, value1);
      };
    };
    return Tuple2;
  }();
  var uncurry = function(f) {
    return function(v) {
      return f(v.value0)(v.value1);
    };
  };
  var fst = function(v) {
    return v.value0;
  };

  // output/Unsafe.Coerce/foreign.js
  var unsafeCoerce2 = function(x) {
    return x;
  };

  // output/Safe.Coerce/index.js
  var coerce = function() {
    return unsafeCoerce2;
  };

  // output/Data.Newtype/index.js
  var coerce2 = /* @__PURE__ */ coerce();
  var unwrap = function() {
    return coerce2;
  };

  // output/Data.Traversable/foreign.js
  var traverseArrayImpl = function() {
    function array1(a) {
      return [a];
    }
    function array2(a) {
      return function(b) {
        return [a, b];
      };
    }
    function array3(a) {
      return function(b) {
        return function(c) {
          return [a, b, c];
        };
      };
    }
    function concat2(xs) {
      return function(ys) {
        return xs.concat(ys);
      };
    }
    return function(apply2) {
      return function(map5) {
        return function(pure3) {
          return function(f) {
            return function(array) {
              function go(bot, top3) {
                switch (top3 - bot) {
                  case 0:
                    return pure3([]);
                  case 1:
                    return map5(array1)(f(array[bot]));
                  case 2:
                    return apply2(map5(array2)(f(array[bot])))(f(array[bot + 1]));
                  case 3:
                    return apply2(apply2(map5(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                  default:
                    var pivot = bot + Math.floor((top3 - bot) / 4) * 2;
                    return apply2(map5(concat2)(go(bot, pivot)))(go(pivot, top3));
                }
              }
              return go(0, array.length);
            };
          };
        };
      };
    };
  }();

  // output/Data.Array/index.js
  var append2 = /* @__PURE__ */ append(semigroupArray);
  var cons = function(x) {
    return function(xs) {
      return append2([x])(xs);
    };
  };

  // output/GameModel/index.js
  var initialModel = {
    gameStepNumber: 0,
    gameTime: 0,
    inputKey: 0,
    wsBuffer: [],
    screenWidth: 150,
    screenHeight: 100
  };
  var initGame = /* @__PURE__ */ pure(applicativeEffect)(initialModel);

  // output/Constants/index.js
  var constants = {
    frameRateNumber: 20,
    websocketUrl: "ws://95.140.155.123:1234/ws"
  };

  // output/Signal/foreign.js
  function make(initial) {
    var subs = [];
    var val = initial;
    var sig = {
      subscribe: function(sub2) {
        subs.push(sub2);
        sub2(val);
      },
      get: function() {
        return val;
      },
      set: function(newval) {
        val = newval;
        subs.forEach(function(sub2) {
          sub2(newval);
        });
      }
    };
    return sig;
  }
  var constant = make;
  function mapSig(fun) {
    return function(sig) {
      var out = make(fun(sig.get()));
      sig.subscribe(function(val) {
        out.set(fun(val));
      });
      return out;
    };
  }
  function merge(sig1) {
    return function(sig2) {
      var out = make(sig1.get());
      sig2.subscribe(out.set);
      sig1.subscribe(out.set);
      return out;
    };
  }
  function foldp(fun) {
    return function(seed) {
      return function(sig) {
        var acc = seed;
        var out = make(acc);
        sig.subscribe(function(val) {
          acc = fun(val)(acc);
          out.set(acc);
        });
        return out;
      };
    };
  }
  function runSignal(sig) {
    return function() {
      sig.subscribe(function(val) {
        val();
      });
      return {};
    };
  }

  // output/Signal/index.js
  var semigroupSignal = {
    append: merge
  };
  var functorSignal = {
    map: mapSig
  };

  // output/Signal.DOM/foreign.js
  function keyPressedP(constant2) {
    return function(keyCode) {
      return function() {
        var out = constant2(false);
        window.addEventListener("keydown", function(e) {
          if (e.keyCode === keyCode)
            out.set(true);
        });
        window.addEventListener("keyup", function(e) {
          if (e.keyCode === keyCode)
            out.set(false);
        });
        return out;
      };
    };
  }

  // output/Signal.Time/foreign.js
  function now() {
    var perf = typeof performance !== "undefined" ? performance : null, proc = typeof process !== "undefined" ? process : null;
    return (perf && (perf.now || perf.webkitNow || perf.msNow || perf.oNow || perf.mozNow) || proc && proc.hrtime && function() {
      var t = proc.hrtime();
      return (t[0] * 1e9 + t[1]) / 1e6;
    } || Date.now).call(perf);
  }
  function everyP(constant2) {
    return function(t) {
      var out = constant2(now());
      setInterval(function() {
        out.set(now());
      }, t);
      return out;
    };
  }

  // output/Signal.Time/index.js
  var every = /* @__PURE__ */ everyP(constant);

  // output/Signal.DOM/index.js
  var keyPressed = /* @__PURE__ */ keyPressedP(constant);

  // output/Data.Int/foreign.js
  var fromNumberImpl = function(just) {
    return function(nothing) {
      return function(n) {
        return (n | 0) === n ? just(n) : nothing;
      };
    };
  };
  var toNumber = function(n) {
    return n;
  };

  // output/Data.Number/foreign.js
  var isFiniteImpl = isFinite;
  var floor = Math.floor;

  // output/Data.Int/index.js
  var top2 = /* @__PURE__ */ top(boundedInt);
  var bottom2 = /* @__PURE__ */ bottom(boundedInt);
  var fromNumber = /* @__PURE__ */ function() {
    return fromNumberImpl(Just.create)(Nothing.value);
  }();
  var unsafeClamp = function(x) {
    if (!isFiniteImpl(x)) {
      return 0;
    }
    ;
    if (x >= toNumber(top2)) {
      return top2;
    }
    ;
    if (x <= toNumber(bottom2)) {
      return bottom2;
    }
    ;
    if (otherwise) {
      return fromMaybe(0)(fromNumber(x));
    }
    ;
    throw new Error("Failed pattern match at Data.Int (line 72, column 1 - line 72, column 29): " + [x.constructor.name]);
  };
  var floor2 = function($39) {
    return unsafeClamp(floor($39));
  };

  // output/Effect.Random/foreign.js
  var random = Math.random;

  // output/Effect.Random/index.js
  var randomInt = function(low) {
    return function(high) {
      return function __do3() {
        var n = random();
        var asNumber = (toNumber(high) - toNumber(low) + 1) * n + toNumber(low);
        return floor2(asNumber);
      };
    };
  };

  // output/Random.LCG/index.js
  var mod2 = /* @__PURE__ */ mod(euclideanRingInt);
  var seedMin = 1;
  var lcgM = 2147483647;
  var seedMax = /* @__PURE__ */ function() {
    return lcgM - 1 | 0;
  }();
  var mkSeed = function(x) {
    var ensureBetween = function(min3) {
      return function(max3) {
        return function(n) {
          var rangeSize = max3 - min3 | 0;
          var n$prime = mod2(n)(rangeSize);
          var $25 = n$prime < min3;
          if ($25) {
            return n$prime + max3 | 0;
          }
          ;
          return n$prime;
        };
      };
    };
    return ensureBetween(seedMin)(seedMax)(x);
  };
  var randomSeed = /* @__PURE__ */ map(functorEffect)(mkSeed)(/* @__PURE__ */ randomInt(seedMin)(seedMax));

  // output/Control.Monad.State.Trans/index.js
  var functorStateT = function(dictFunctor) {
    var map5 = map(dictFunctor);
    return {
      map: function(f) {
        return function(v) {
          return function(s) {
            return map5(function(v1) {
              return new Tuple(f(v1.value0), v1.value1);
            })(v(s));
          };
        };
      }
    };
  };
  var monadStateT = function(dictMonad) {
    return {
      Applicative0: function() {
        return applicativeStateT(dictMonad);
      },
      Bind1: function() {
        return bindStateT(dictMonad);
      }
    };
  };
  var bindStateT = function(dictMonad) {
    var bind2 = bind(dictMonad.Bind1());
    return {
      bind: function(v) {
        return function(f) {
          return function(s) {
            return bind2(v(s))(function(v1) {
              var v3 = f(v1.value0);
              return v3(v1.value1);
            });
          };
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };
  var applyStateT = function(dictMonad) {
    var functorStateT1 = functorStateT(dictMonad.Bind1().Apply0().Functor0());
    return {
      apply: ap(monadStateT(dictMonad)),
      Functor0: function() {
        return functorStateT1;
      }
    };
  };
  var applicativeStateT = function(dictMonad) {
    var pure3 = pure(dictMonad.Applicative0());
    return {
      pure: function(a) {
        return function(s) {
          return pure3(new Tuple(a, s));
        };
      },
      Apply0: function() {
        return applyStateT(dictMonad);
      }
    };
  };

  // output/Control.Monad.State/index.js
  var unwrap3 = /* @__PURE__ */ unwrap();
  var runState = function(v) {
    return function($18) {
      return unwrap3(v($18));
    };
  };

  // output/Data.Array.NonEmpty.Internal/foreign.js
  var traverse1Impl = function() {
    function Cont(fn) {
      this.fn = fn;
    }
    var emptyList = {};
    var ConsCell = function(head3, tail2) {
      this.head = head3;
      this.tail = tail2;
    };
    function finalCell(head3) {
      return new ConsCell(head3, emptyList);
    }
    function consList(x) {
      return function(xs) {
        return new ConsCell(x, xs);
      };
    }
    function listToArray(list) {
      var arr = [];
      var xs = list;
      while (xs !== emptyList) {
        arr.push(xs.head);
        xs = xs.tail;
      }
      return arr;
    }
    return function(apply2, map5, f) {
      var buildFrom = function(x, ys) {
        return apply2(map5(consList)(f(x)))(ys);
      };
      var go = function(acc, currentLen, xs) {
        if (currentLen === 0) {
          return acc;
        } else {
          var last2 = xs[currentLen - 1];
          return new Cont(function() {
            var built = go(buildFrom(last2, acc), currentLen - 1, xs);
            return built;
          });
        }
      };
      return function(array) {
        var acc = map5(finalCell)(f(array[array.length - 1]));
        var result = go(acc, array.length - 1, array);
        while (result instanceof Cont) {
          result = result.fn();
        }
        return map5(listToArray)(result);
      };
    };
  }();

  // output/Test.QuickCheck.Gen/index.js
  var unGen = function(v) {
    return v;
  };
  var runGen = function($103) {
    return runState(unGen($103));
  };
  var applicativeGen = /* @__PURE__ */ applicativeStateT(monadIdentity);

  // output/SignalM/index.js
  var map2 = /* @__PURE__ */ map(functorSignal);
  var foldpM = function(run3) {
    return function(st$prime) {
      return function(f) {
        return function(st) {
          return function(sig) {
            return map2(fst)(foldp(function(xa) {
              return function(v) {
                return uncurry(run3)(new Tuple(f(xa)(v.value0), v.value1));
              };
            })(new Tuple(st, st$prime))(sig));
          };
        };
      };
    };
  };
  var foldpR$prime = /* @__PURE__ */ foldpM(runGen);
  var foldpR = function(f) {
    return function(st) {
      return function(sig) {
        return function __do3() {
          var seed = randomSeed();
          return foldpR$prime({
            newSeed: seed,
            size: 536870911
          })(f)(st)(sig);
        };
      };
    };
  };

  // output/WebSocket.WSSignalChan/foreign.js
  var _wsocket = function(url) {
    return function() {
      return new WebSocket(url);
    };
  };
  var _addEventListenerConnectionIsOpen = function(socket) {
    return function() {
      socket.addEventListener("open", (event) => {
        console.log("WebSocket connection opened:", event);
      });
    };
  };
  var _addEventListenerMessageRecieved = function(socket) {
    return function(signalFunc) {
      return function() {
        socket.addEventListener("message", (event) => {
          signalFunc(event.data)();
        });
      };
    };
  };
  var _addEventListenerConnectionIsClose = function(socket) {
    return function() {
      socket.addEventListener("close", (event) => {
        console.log("WebSocket connection closed:", event);
      });
    };
  };

  // output/Signal.Channel/foreign.js
  function channelP(constant2) {
    return function(v) {
      return function() {
        return constant2(v);
      };
    };
  }
  function sendP(chan) {
    return function(v) {
      return function() {
        chan.set(v);
      };
    };
  }
  function subscribe(chan) {
    return chan;
  }

  // output/Signal.Channel/index.js
  var send = sendP;
  var channel = /* @__PURE__ */ channelP(constant);

  // output/WebSocket.WSSignalChan/index.js
  var addListenerWSMessageToSignal = function(socket) {
    return function __do3() {
      var chan = channel("")();
      _addEventListenerMessageRecieved(socket)(function(msg) {
        return send(chan)(msg);
      })();
      return subscribe(chan);
    };
  };
  var initWSSignal = function(url) {
    return function __do3() {
      var socket = _wsocket(url)();
      _addEventListenerConnectionIsOpen(socket)();
      _addEventListenerConnectionIsClose(socket)();
      return addListenerWSMessageToSignal(socket)();
    };
  };

  // output/GetInput/index.js
  var map3 = /* @__PURE__ */ map(functorEffect);
  var map1 = /* @__PURE__ */ map(functorSignal);
  var append1 = /* @__PURE__ */ append(semigroupSignal);
  var FrameRateSignal = /* @__PURE__ */ function() {
    function FrameRateSignal2(value0) {
      this.value0 = value0;
    }
    ;
    FrameRateSignal2.create = function(value0) {
      return new FrameRateSignal2(value0);
    };
    return FrameRateSignal2;
  }();
  var UserInputSignal = /* @__PURE__ */ function() {
    function UserInputSignal2(value0) {
      this.value0 = value0;
    }
    ;
    UserInputSignal2.create = function(value0) {
      return new UserInputSignal2(value0);
    };
    return UserInputSignal2;
  }();
  var WebSocketSignal = /* @__PURE__ */ function() {
    function WebSocketSignal2(value0) {
      this.value0 = value0;
    }
    ;
    WebSocketSignal2.create = function(value0) {
      return new WebSocketSignal2(value0);
    };
    return WebSocketSignal2;
  }();
  var wsSignal = /* @__PURE__ */ function() {
    return map3(map1(WebSocketSignal.create))(initWSSignal(constants.websocketUrl));
  }();
  var getUserInput = /* @__PURE__ */ map3(/* @__PURE__ */ map1(function(k) {
    var n = function() {
      if (k) {
        return 32;
      }
      ;
      return 0;
    }();
    return new UserInputSignal({
      key: n
    });
  }))(/* @__PURE__ */ keyPressed(32));
  var frameRate = /* @__PURE__ */ function() {
    return every(constants.frameRateNumber);
  }();
  var getAllInput = /* @__PURE__ */ function() {
    var frSig = map1(FrameRateSignal.create)(frameRate);
    return function __do3() {
      var uInputSig = getUserInput();
      var wsSig = wsSignal();
      return append1(frSig)(append1(uInputSig)(wsSig));
    };
  }();

  // output/Graphics.Canvas/foreign.js
  function getCanvasElementByIdImpl(id, Just2, Nothing2) {
    return function() {
      var el = document.getElementById(id);
      if (el && el instanceof HTMLCanvasElement) {
        return Just2(el);
      } else {
        return Nothing2;
      }
    };
  }
  function getContext2D(c) {
    return function() {
      return c.getContext("2d");
    };
  }
  function getCanvasWidth(canvas) {
    return function() {
      return canvas.width;
    };
  }
  function getCanvasHeight(canvas) {
    return function() {
      return canvas.height;
    };
  }
  function beginPath(ctx) {
    return function() {
      ctx.beginPath();
    };
  }
  function fill(ctx) {
    return function() {
      ctx.fill();
    };
  }
  function rect(ctx) {
    return function(r) {
      return function() {
        ctx.rect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function clearRect(ctx) {
    return function(r) {
      return function() {
        ctx.clearRect(r.x, r.y, r.width, r.height);
      };
    };
  }
  function save(ctx) {
    return function() {
      ctx.save();
    };
  }
  function restore(ctx) {
    return function() {
      ctx.restore();
    };
  }

  // output/Graphics.Canvas/index.js
  var getCanvasElementById = function(elId) {
    return getCanvasElementByIdImpl(elId, Just.create, Nothing.value);
  };
  var getCanvasDimensions = function(ce) {
    return function __do3() {
      var w = getCanvasWidth(ce)();
      var h = getCanvasHeight(ce)();
      return {
        width: w,
        height: h
      };
    };
  };
  var fillPath = function(ctx) {
    return function(path) {
      return function __do3() {
        beginPath(ctx)();
        var a = path();
        fill(ctx)();
        return a;
      };
    };
  };

  // output/Render/index.js
  var show2 = /* @__PURE__ */ show(/* @__PURE__ */ showRecord()()(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "gameStepNumber";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "gameTime";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "inputKey";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "screenHeight";
    }
  })(/* @__PURE__ */ showRecordFieldsCons({
    reflectSymbol: function() {
      return "screenWidth";
    }
  })(/* @__PURE__ */ showRecordFieldsConsNil({
    reflectSymbol: function() {
      return "wsBuffer";
    }
  })(/* @__PURE__ */ showArray(showString)))(showNumber))(showNumber))(showInt))(showNumber))(showInt)));
  var render = function(m) {
    return function __do3() {
      log(show2(m))();
      var v = getCanvasElementById("gameBoard")();
      if (v instanceof Just) {
        var ctx = getContext2D(v.value0)();
        var canvasDim = getCanvasDimensions(v.value0)();
        save(ctx)();
        clearRect(ctx)({
          x: 0,
          y: 0,
          width: canvasDim.width,
          height: canvasDim.width
        })();
        fillPath(ctx)(rect(ctx)({
          x: 0,
          y: 0,
          width: canvasDim.width,
          height: canvasDim.height
        }))();
        clearRect(ctx)({
          x: 0,
          y: 0,
          width: 300,
          height: 200
        })();
        return restore(ctx)();
      }
      ;
      throw new Error("Failed pattern match at Render (line 25, column 9 - line 25, column 56): " + [v.constructor.name]);
    };
  };

  // output/RunGame/index.js
  var pure2 = /* @__PURE__ */ pure(applicativeGen);
  var map4 = /* @__PURE__ */ map(functorSignal);
  var gameStep = function(v) {
    return function(v1) {
      if (v instanceof UserInputSignal) {
        return pure2({
          gameStepNumber: v1.gameStepNumber + 1 | 0,
          inputKey: v.value0.key,
          gameTime: v1.gameTime,
          screenHeight: v1.screenHeight,
          screenWidth: v1.screenWidth,
          wsBuffer: v1.wsBuffer
        });
      }
      ;
      if (v instanceof WebSocketSignal) {
        return pure2({
          gameStepNumber: v1.gameStepNumber + 1 | 0,
          wsBuffer: cons(v.value0)(v1.wsBuffer),
          gameTime: v1.gameTime,
          inputKey: v1.inputKey,
          screenHeight: v1.screenHeight,
          screenWidth: v1.screenWidth
        });
      }
      ;
      if (v instanceof FrameRateSignal) {
        return pure2({
          gameStepNumber: v1.gameStepNumber + 1 | 0,
          gameTime: v.value0,
          wsBuffer: [],
          inputKey: v1.inputKey,
          screenHeight: v1.screenHeight,
          screenWidth: v1.screenWidth
        });
      }
      ;
      throw new Error("Failed pattern match at RunGame (line 15, column 1 - line 15, column 45): " + [v.constructor.name, v1.constructor.name]);
    };
  };
  var runGame = function __do() {
    var initialGameModel = initGame();
    render(initialGameModel)();
    var inputSignal = getAllInput();
    var game = foldpR(gameStep)(initialGameModel)(inputSignal)();
    return runSignal(map4(render)(game))();
  };

  // output/Main/index.js
  var main = function __do2() {
    log("\u{1F35D}")();
    return runGame();
  };

  // <stdin>
  main();
})();
