// output/Cardano.Wallet.Cip30/foreign.js
var _getWalletApi = (walletName) => (extensions) => () => window.cardano[walletName].enable({ extensions });
var _getBalance = (api) => () => api.getBalance();
var isWalletAvailable = (walletName) => () => typeof window.cardano != "undefined" && typeof window.cardano[walletName] != "undefined" && typeof window.cardano[walletName].apiVersion != "undefined" && typeof window.cardano[walletName].enable == "function";
var allWalletTags = () => typeof window.cardano != "undefined" ? Object.keys(window.cardano).filter(
  (tag) => typeof window.cardano[tag] == "object"
) : [];

// output/Control.Promise/foreign.js
function thenImpl(promise2) {
  return function(errCB) {
    return function(succCB) {
      return function() {
        promise2.then(succCB, errCB);
      };
    };
  };
}

// output/Data.Functor/foreign.js
var arrayMap = function(f) {
  return function(arr) {
    var l = arr.length;
    var result = new Array(l);
    for (var i2 = 0; i2 < l; i2++) {
      result[i2] = f(arr[i2]);
    }
    return result;
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

// output/Data.Function/index.js
var flip = function(f) {
  return function(b2) {
    return function(a2) {
      return f(a2)(b2);
    };
  };
};
var $$const = function(a2) {
  return function(v) {
    return a2;
  };
};

// output/Data.Unit/foreign.js
var unit = void 0;

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var $$void = function(dictFunctor) {
  return map(dictFunctor)($$const(unit));
};
var voidLeft = function(dictFunctor) {
  var map110 = map(dictFunctor);
  return function(f) {
    return function(x) {
      return map110($$const(x))(f);
    };
  };
};
var voidRight = function(dictFunctor) {
  var map110 = map(dictFunctor);
  return function(x) {
    return map110($$const(x));
  };
};
var functorArray = {
  map: arrayMap
};

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

// output/Control.Alt/index.js
var alt = function(dict) {
  return dict.alt;
};

// output/Control.Bind/foreign.js
var arrayBind = function(arr) {
  return function(f) {
    var result = [];
    for (var i2 = 0, l = arr.length; i2 < l; i2++) {
      Array.prototype.push.apply(result, f(arr[i2]));
    }
    return result;
  };
};

// output/Control.Apply/foreign.js
var arrayApply = function(fs) {
  return function(xs) {
    var l = fs.length;
    var k = xs.length;
    var result = new Array(l * k);
    var n = 0;
    for (var i2 = 0; i2 < l; i2++) {
      var f = fs[i2];
      for (var j = 0; j < k; j++) {
        result[n++] = f(xs[j]);
      }
    }
    return result;
  };
};

// output/Control.Apply/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
var applyArray = {
  apply: arrayApply,
  Functor0: function() {
    return functorArray;
  }
};
var apply = function(dict) {
  return dict.apply;
};
var applySecond = function(dictApply) {
  var apply1 = apply(dictApply);
  var map20 = map(dictApply.Functor0());
  return function(a2) {
    return function(b2) {
      return apply1(map20($$const(identity2))(a2))(b2);
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var unless = function(dictApplicative) {
  var pure13 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (!v) {
        return v1;
      }
      ;
      if (v) {
        return pure13(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var when = function(dictApplicative) {
  var pure13 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (v) {
        return v1;
      }
      ;
      if (!v) {
        return pure13(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var liftA1 = function(dictApplicative) {
  var apply3 = apply(dictApplicative.Apply0());
  var pure13 = pure(dictApplicative);
  return function(f) {
    return function(a2) {
      return apply3(pure13(f))(a2);
    };
  };
};

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
};
var bindArray = {
  bind: arrayBind,
  Apply0: function() {
    return applyArray;
  }
};
var bind = function(dict) {
  return dict.bind;
};
var bindFlipped = function(dictBind) {
  return flip(bind(dictBind));
};
var composeKleisliFlipped = function(dictBind) {
  var bindFlipped12 = bindFlipped(dictBind);
  return function(f) {
    return function(g) {
      return function(a2) {
        return bindFlipped12(f)(g(a2));
      };
    };
  };
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
  }
};

// output/Data.Bounded/foreign.js
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
var ordStringImpl = unsafeCompareImpl;

// output/Data.Eq/foreign.js
var refEq = function(r1) {
  return function(r2) {
    return r1 === r2;
  };
};
var eqIntImpl = refEq;
var eqStringImpl = refEq;

// output/Data.Eq/index.js
var eqString = {
  eq: eqStringImpl
};
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

// output/Data.Ord/index.js
var ordString = /* @__PURE__ */ function() {
  return {
    compare: ordStringImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqString;
    }
  };
}();
var ordInt = /* @__PURE__ */ function() {
  return {
    compare: ordIntImpl(LT.value)(EQ.value)(GT.value),
    Eq0: function() {
      return eqInt;
    }
  };
}();
var compare = function(dict) {
  return dict.compare;
};

// output/Data.Show/foreign.js
var showStringImpl = function(s) {
  var l = s.length;
  return '"' + s.replace(
    /[\0-\x1F\x7F"\\]/g,
    // eslint-disable-line no-control-regex
    function(c, i2) {
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
      var k = i2 + 1;
      var empty7 = k < l && s[k] >= "0" && s[k] <= "9" ? "\\&" : "";
      return "\\" + c.charCodeAt(0).toString(10) + empty7;
    }
  ) + '"';
};

// output/Data.Show/index.js
var showString = {
  show: showStringImpl
};
var show = function(dict) {
  return dict.show;
};

// output/Data.Maybe/index.js
var identity3 = /* @__PURE__ */ identity(categoryFn);
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
var isNothing = /* @__PURE__ */ maybe(true)(/* @__PURE__ */ $$const(false));
var isJust = /* @__PURE__ */ maybe(false)(/* @__PURE__ */ $$const(true));
var functorMaybe = {
  map: function(v) {
    return function(v1) {
      if (v1 instanceof Just) {
        return new Just(v(v1.value0));
      }
      ;
      return Nothing.value;
    };
  }
};
var map2 = /* @__PURE__ */ map(functorMaybe);
var fromMaybe = function(a2) {
  return maybe(a2)(identity3);
};
var fromJust = function() {
  return function(v) {
    if (v instanceof Just) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Maybe (line 288, column 1 - line 288, column 46): " + [v.constructor.name]);
  };
};
var applyMaybe = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return map2(v.value0)(v1);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 67, column 1 - line 69, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorMaybe;
  }
};
var bindMaybe = {
  bind: function(v) {
    return function(v1) {
      if (v instanceof Just) {
        return v1(v.value0);
      }
      ;
      if (v instanceof Nothing) {
        return Nothing.value;
      }
      ;
      throw new Error("Failed pattern match at Data.Maybe (line 125, column 1 - line 127, column 28): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Apply0: function() {
    return applyMaybe;
  }
};

// output/Data.Either/index.js
var Left = /* @__PURE__ */ function() {
  function Left2(value0) {
    this.value0 = value0;
  }
  ;
  Left2.create = function(value0) {
    return new Left2(value0);
  };
  return Left2;
}();
var Right = /* @__PURE__ */ function() {
  function Right2(value0) {
    this.value0 = value0;
  }
  ;
  Right2.create = function(value0) {
    return new Right2(value0);
  };
  return Right2;
}();
var functorEither = {
  map: function(f) {
    return function(m) {
      if (m instanceof Left) {
        return new Left(m.value0);
      }
      ;
      if (m instanceof Right) {
        return new Right(f(m.value0));
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 0, column 0 - line 0, column 0): " + [m.constructor.name]);
    };
  }
};
var either = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2 instanceof Left) {
        return v(v2.value0);
      }
      ;
      if (v2 instanceof Right) {
        return v1(v2.value0);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 208, column 1 - line 208, column 64): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
    };
  };
};
var hush = /* @__PURE__ */ function() {
  return either($$const(Nothing.value))(Just.create);
}();

// output/Effect/foreign.js
var pureE = function(a2) {
  return function() {
    return a2;
  };
};
var bindE = function(a2) {
  return function(f) {
    return function() {
      return f(a2())();
    };
  };
};

// output/Control.Monad/index.js
var unlessM = function(dictMonad) {
  var bind8 = bind(dictMonad.Bind1());
  var unless2 = unless(dictMonad.Applicative0());
  return function(mb) {
    return function(m) {
      return bind8(mb)(function(b2) {
        return unless2(b2)(m);
      });
    };
  };
};
var ap = function(dictMonad) {
  var bind8 = bind(dictMonad.Bind1());
  var pure9 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a2) {
      return bind8(f)(function(f$prime) {
        return bind8(a2)(function(a$prime) {
          return pure9(f$prime(a$prime));
        });
      });
    };
  };
};

// output/Data.Monoid/index.js
var mempty = function(dict) {
  return dict.mempty;
};

// output/Effect/index.js
var $runtime_lazy = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
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

// output/Effect.Exception/foreign.js
function error(msg) {
  return new Error(msg);
}
function throwException(e) {
  return function() {
    throw e;
  };
}

// output/Effect.Exception/index.js
var $$throw = function($4) {
  return throwException(error($4));
};

// output/Control.Monad.Error.Class/index.js
var throwError = function(dict) {
  return dict.throwError;
};
var catchError = function(dict) {
  return dict.catchError;
};
var $$try = function(dictMonadError) {
  var catchError1 = catchError(dictMonadError);
  var Monad0 = dictMonadError.MonadThrow0().Monad0();
  var map20 = map(Monad0.Bind1().Apply0().Functor0());
  var pure9 = pure(Monad0.Applicative0());
  return function(a2) {
    return catchError1(map20(Right.create)(a2))(function($52) {
      return pure9(Left.create($52));
    });
  };
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

// output/Effect.Ref/foreign.js
var _new = function(val) {
  return function() {
    return { value: val };
  };
};
var read = function(ref2) {
  return function() {
    return ref2.value;
  };
};
var modifyImpl = function(f) {
  return function(ref2) {
    return function() {
      var t = f(ref2.value);
      ref2.value = t.state;
      return t.value;
    };
  };
};
var write = function(val) {
  return function(ref2) {
    return function() {
      ref2.value = val;
    };
  };
};

// output/Effect.Ref/index.js
var $$void2 = /* @__PURE__ */ $$void(functorEffect);
var $$new = _new;
var modify$prime = modifyImpl;
var modify = function(f) {
  return modify$prime(function(s) {
    var s$prime = f(s);
    return {
      state: s$prime,
      value: s$prime
    };
  });
};
var modify_ = function(f) {
  return function(s) {
    return $$void2(modify(f)(s));
  };
};

// output/Control.Monad.Rec.Class/index.js
var bindFlipped2 = /* @__PURE__ */ bindFlipped(bindEffect);
var map3 = /* @__PURE__ */ map(functorEffect);
var Loop = /* @__PURE__ */ function() {
  function Loop2(value0) {
    this.value0 = value0;
  }
  ;
  Loop2.create = function(value0) {
    return new Loop2(value0);
  };
  return Loop2;
}();
var Done = /* @__PURE__ */ function() {
  function Done2(value0) {
    this.value0 = value0;
  }
  ;
  Done2.create = function(value0) {
    return new Done2(value0);
  };
  return Done2;
}();
var tailRecM = function(dict) {
  return dict.tailRecM;
};
var monadRecEffect = {
  tailRecM: function(f) {
    return function(a2) {
      var fromDone = function(v) {
        if (v instanceof Done) {
          return v.value0;
        }
        ;
        throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 137, column 30 - line 137, column 44): " + [v.constructor.name]);
      };
      return function __do4() {
        var r = bindFlipped2($$new)(f(a2))();
        (function() {
          while (!function __do5() {
            var v = read(r)();
            if (v instanceof Loop) {
              var e = f(v.value0)();
              write(e)(r)();
              return false;
            }
            ;
            if (v instanceof Done) {
              return true;
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Rec.Class (line 128, column 22 - line 133, column 28): " + [v.constructor.name]);
          }()) {
          }
          ;
          return {};
        })();
        return map3(fromDone)(read(r))();
      };
    };
  },
  Monad0: function() {
    return monadEffect;
  }
};

// output/Data.HeytingAlgebra/foreign.js
var boolConj = function(b1) {
  return function(b2) {
    return b1 && b2;
  };
};
var boolDisj = function(b1) {
  return function(b2) {
    return b1 || b2;
  };
};
var boolNot = function(b2) {
  return !b2;
};

// output/Data.HeytingAlgebra/index.js
var tt = function(dict) {
  return dict.tt;
};
var not = function(dict) {
  return dict.not;
};
var implies = function(dict) {
  return dict.implies;
};
var ff = function(dict) {
  return dict.ff;
};
var disj = function(dict) {
  return dict.disj;
};
var heytingAlgebraBoolean = {
  ff: false,
  tt: true,
  implies: function(a2) {
    return function(b2) {
      return disj(heytingAlgebraBoolean)(not(heytingAlgebraBoolean)(a2))(b2);
    };
  },
  conj: boolConj,
  disj: boolDisj,
  not: boolNot
};
var conj = function(dict) {
  return dict.conj;
};
var heytingAlgebraFunction = function(dictHeytingAlgebra) {
  var ff1 = ff(dictHeytingAlgebra);
  var tt1 = tt(dictHeytingAlgebra);
  var implies1 = implies(dictHeytingAlgebra);
  var conj1 = conj(dictHeytingAlgebra);
  var disj1 = disj(dictHeytingAlgebra);
  var not1 = not(dictHeytingAlgebra);
  return {
    ff: function(v) {
      return ff1;
    },
    tt: function(v) {
      return tt1;
    },
    implies: function(f) {
      return function(g) {
        return function(a2) {
          return implies1(f(a2))(g(a2));
        };
      };
    },
    conj: function(f) {
      return function(g) {
        return function(a2) {
          return conj1(f(a2))(g(a2));
        };
      };
    },
    disj: function(f) {
      return function(g) {
        return function(a2) {
          return disj1(f(a2))(g(a2));
        };
      };
    },
    not: function(f) {
      return function(a2) {
        return not1(f(a2));
      };
    }
  };
};

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
var snd = function(v) {
  return v.value1;
};
var functorTuple = {
  map: function(f) {
    return function(m) {
      return new Tuple(m.value0, f(m.value1));
    };
  }
};
var fst = function(v) {
  return v.value0;
};

// output/Control.Monad.State.Class/index.js
var state = function(dict) {
  return dict.state;
};
var modify_2 = function(dictMonadState) {
  var state1 = state(dictMonadState);
  return function(f) {
    return state1(function(s) {
      return new Tuple(unit, f(s));
    });
  };
};

// output/Effect.Class/index.js
var monadEffectEffect = {
  liftEffect: /* @__PURE__ */ identity(categoryFn),
  Monad0: function() {
    return monadEffect;
  }
};
var liftEffect = function(dict) {
  return dict.liftEffect;
};

// output/Control.Monad.Except.Trans/index.js
var map4 = /* @__PURE__ */ map(functorEither);
var ExceptT = function(x) {
  return x;
};
var runExceptT = function(v) {
  return v;
};
var mapExceptT = function(f) {
  return function(v) {
    return f(v);
  };
};
var functorExceptT = function(dictFunctor) {
  var map110 = map(dictFunctor);
  return {
    map: function(f) {
      return mapExceptT(map110(map4(f)));
    }
  };
};
var monadExceptT = function(dictMonad) {
  return {
    Applicative0: function() {
      return applicativeExceptT(dictMonad);
    },
    Bind1: function() {
      return bindExceptT(dictMonad);
    }
  };
};
var bindExceptT = function(dictMonad) {
  var bind8 = bind(dictMonad.Bind1());
  var pure9 = pure(dictMonad.Applicative0());
  return {
    bind: function(v) {
      return function(k) {
        return bind8(v)(either(function($187) {
          return pure9(Left.create($187));
        })(function(a2) {
          var v1 = k(a2);
          return v1;
        }));
      };
    },
    Apply0: function() {
      return applyExceptT(dictMonad);
    }
  };
};
var applyExceptT = function(dictMonad) {
  var functorExceptT1 = functorExceptT(dictMonad.Bind1().Apply0().Functor0());
  return {
    apply: ap(monadExceptT(dictMonad)),
    Functor0: function() {
      return functorExceptT1;
    }
  };
};
var applicativeExceptT = function(dictMonad) {
  return {
    pure: function() {
      var $188 = pure(dictMonad.Applicative0());
      return function($189) {
        return ExceptT($188(Right.create($189)));
      };
    }(),
    Apply0: function() {
      return applyExceptT(dictMonad);
    }
  };
};
var monadThrowExceptT = function(dictMonad) {
  var monadExceptT1 = monadExceptT(dictMonad);
  return {
    throwError: function() {
      var $198 = pure(dictMonad.Applicative0());
      return function($199) {
        return ExceptT($198(Left.create($199)));
      };
    }(),
    Monad0: function() {
      return monadExceptT1;
    }
  };
};
var altExceptT = function(dictSemigroup) {
  var append5 = append(dictSemigroup);
  return function(dictMonad) {
    var Bind1 = dictMonad.Bind1();
    var bind8 = bind(Bind1);
    var pure9 = pure(dictMonad.Applicative0());
    var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
    return {
      alt: function(v) {
        return function(v1) {
          return bind8(v)(function(rm) {
            if (rm instanceof Right) {
              return pure9(new Right(rm.value0));
            }
            ;
            if (rm instanceof Left) {
              return bind8(v1)(function(rn) {
                if (rn instanceof Right) {
                  return pure9(new Right(rn.value0));
                }
                ;
                if (rn instanceof Left) {
                  return pure9(new Left(append5(rm.value0)(rn.value0)));
                }
                ;
                throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 86, column 9 - line 88, column 49): " + [rn.constructor.name]);
              });
            }
            ;
            throw new Error("Failed pattern match at Control.Monad.Except.Trans (line 82, column 5 - line 88, column 49): " + [rm.constructor.name]);
          });
        };
      },
      Functor0: function() {
        return functorExceptT1;
      }
    };
  };
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

// output/Control.Monad.Except/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var runExcept = function($3) {
  return unwrap2(runExceptT($3));
};

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init2) {
    return function(xs) {
      var acc = init2;
      var len = xs.length;
      for (var i2 = len - 1; i2 >= 0; i2--) {
        acc = f(xs[i2])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init2) {
    return function(xs) {
      var acc = init2;
      var len = xs.length;
      for (var i2 = 0; i2 < len; i2++) {
        acc = f(acc)(xs[i2]);
      }
      return acc;
    };
  };
};

// output/Control.Plus/index.js
var empty = function(dict) {
  return dict.empty;
};

// output/Data.Bifunctor/index.js
var bimap = function(dict) {
  return dict.bimap;
};

// output/Data.Foldable/index.js
var foldr = function(dict) {
  return dict.foldr;
};
var traverse_ = function(dictApplicative) {
  var applySecond2 = applySecond(dictApplicative.Apply0());
  var pure9 = pure(dictApplicative);
  return function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(f) {
      return foldr22(function($454) {
        return applySecond2(f($454));
      })(pure9(unit));
    };
  };
};
var for_ = function(dictApplicative) {
  var traverse_14 = traverse_(dictApplicative);
  return function(dictFoldable) {
    return flip(traverse_14(dictFoldable));
  };
};
var foldl = function(dict) {
  return dict.foldl;
};
var foldableMaybe = {
  foldr: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v1;
        }
        ;
        if (v2 instanceof Just) {
          return v(v2.value0)(v1);
        }
        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldl: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Nothing) {
          return v1;
        }
        ;
        if (v2 instanceof Just) {
          return v(v1)(v2.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  },
  foldMap: function(dictMonoid) {
    var mempty3 = mempty(dictMonoid);
    return function(v) {
      return function(v1) {
        if (v1 instanceof Nothing) {
          return mempty3;
        }
        ;
        if (v1 instanceof Just) {
          return v(v1.value0);
        }
        ;
        throw new Error("Failed pattern match at Data.Foldable (line 138, column 1 - line 144, column 27): " + [v.constructor.name, v1.constructor.name]);
      };
    };
  }
};
var foldMapDefaultR = function(dictFoldable) {
  var foldr22 = foldr(dictFoldable);
  return function(dictMonoid) {
    var append5 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return foldr22(function(x) {
        return function(acc) {
          return append5(f(x))(acc);
        };
      })(mempty3);
    };
  };
};
var foldableArray = {
  foldr: foldrArray,
  foldl: foldlArray,
  foldMap: function(dictMonoid) {
    return foldMapDefaultR(foldableArray)(dictMonoid);
  }
};

// output/Data.Traversable/foreign.js
var traverseArrayImpl = /* @__PURE__ */ function() {
  function array1(a2) {
    return [a2];
  }
  function array2(a2) {
    return function(b2) {
      return [a2, b2];
    };
  }
  function array3(a2) {
    return function(b2) {
      return function(c) {
        return [a2, b2, c];
      };
    };
  }
  function concat2(xs) {
    return function(ys) {
      return xs.concat(ys);
    };
  }
  return function(apply3) {
    return function(map20) {
      return function(pure9) {
        return function(f) {
          return function(array) {
            function go2(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure9([]);
                case 1:
                  return map20(array1)(f(array[bot]));
                case 2:
                  return apply3(map20(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply3(apply3(map20(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply3(map20(concat2)(go2(bot, pivot)))(go2(pivot, top2));
              }
            }
            return go2(0, array.length);
          };
        };
      };
    };
  };
}();

// output/Data.Traversable/index.js
var identity4 = /* @__PURE__ */ identity(categoryFn);
var traverse = function(dict) {
  return dict.traverse;
};
var sequenceDefault = function(dictTraversable) {
  var traverse22 = traverse(dictTraversable);
  return function(dictApplicative) {
    return traverse22(dictApplicative)(identity4);
  };
};
var traversableArray = {
  traverse: function(dictApplicative) {
    var Apply0 = dictApplicative.Apply0();
    return traverseArrayImpl(apply(Apply0))(map(Apply0.Functor0()))(pure(dictApplicative));
  },
  sequence: function(dictApplicative) {
    return sequenceDefault(traversableArray)(dictApplicative);
  },
  Functor0: function() {
    return functorArray;
  },
  Foldable1: function() {
    return foldableArray;
  }
};

// output/Data.NonEmpty/index.js
var NonEmpty = /* @__PURE__ */ function() {
  function NonEmpty2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  NonEmpty2.create = function(value0) {
    return function(value1) {
      return new NonEmpty2(value0, value1);
    };
  };
  return NonEmpty2;
}();
var singleton2 = function(dictPlus) {
  var empty7 = empty(dictPlus);
  return function(a2) {
    return new NonEmpty(a2, empty7);
  };
};

// output/Data.List.Types/index.js
var Nil = /* @__PURE__ */ function() {
  function Nil2() {
  }
  ;
  Nil2.value = new Nil2();
  return Nil2;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons2.create = function(value0) {
    return function(value1) {
      return new Cons2(value0, value1);
    };
  };
  return Cons2;
}();
var NonEmptyList = function(x) {
  return x;
};
var toList = function(v) {
  return new Cons(v.value0, v.value1);
};
var listMap = function(f) {
  var chunkedRevMap = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Cons && (v1.value1 instanceof Cons && v1.value1.value1 instanceof Cons)) {
          $tco_var_v = new Cons(v1, v);
          $copy_v1 = v1.value1.value1.value1;
          return;
        }
        ;
        var unrolledMap = function(v2) {
          if (v2 instanceof Cons && (v2.value1 instanceof Cons && v2.value1.value1 instanceof Nil)) {
            return new Cons(f(v2.value0), new Cons(f(v2.value1.value0), Nil.value));
          }
          ;
          if (v2 instanceof Cons && v2.value1 instanceof Nil) {
            return new Cons(f(v2.value0), Nil.value);
          }
          ;
          return Nil.value;
        };
        var reverseUnrolledMap = function($copy_v2) {
          return function($copy_v3) {
            var $tco_var_v2 = $copy_v2;
            var $tco_done1 = false;
            var $tco_result2;
            function $tco_loop2(v2, v3) {
              if (v2 instanceof Cons && (v2.value0 instanceof Cons && (v2.value0.value1 instanceof Cons && v2.value0.value1.value1 instanceof Cons))) {
                $tco_var_v2 = v2.value1;
                $copy_v3 = new Cons(f(v2.value0.value0), new Cons(f(v2.value0.value1.value0), new Cons(f(v2.value0.value1.value1.value0), v3)));
                return;
              }
              ;
              $tco_done1 = true;
              return v3;
            }
            ;
            while (!$tco_done1) {
              $tco_result2 = $tco_loop2($tco_var_v2, $copy_v3);
            }
            ;
            return $tco_result2;
          };
        };
        $tco_done = true;
        return reverseUnrolledMap(v)(unrolledMap(v1));
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return chunkedRevMap(Nil.value);
};
var functorList = {
  map: listMap
};
var foldableList = {
  foldr: function(f) {
    return function(b2) {
      var rev3 = function() {
        var go2 = function($copy_v) {
          return function($copy_v1) {
            var $tco_var_v = $copy_v;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1) {
              if (v1 instanceof Nil) {
                $tco_done = true;
                return v;
              }
              ;
              if (v1 instanceof Cons) {
                $tco_var_v = new Cons(v1.value0, v);
                $copy_v1 = v1.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.List.Types (line 107, column 7 - line 107, column 23): " + [v.constructor.name, v1.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $copy_v1);
            }
            ;
            return $tco_result;
          };
        };
        return go2(Nil.value);
      }();
      var $284 = foldl(foldableList)(flip(f))(b2);
      return function($285) {
        return $284(rev3($285));
      };
    };
  },
  foldl: function(f) {
    var go2 = function($copy_b) {
      return function($copy_v) {
        var $tco_var_b = $copy_b;
        var $tco_done1 = false;
        var $tco_result;
        function $tco_loop(b2, v) {
          if (v instanceof Nil) {
            $tco_done1 = true;
            return b2;
          }
          ;
          if (v instanceof Cons) {
            $tco_var_b = f(b2)(v.value0);
            $copy_v = v.value1;
            return;
          }
          ;
          throw new Error("Failed pattern match at Data.List.Types (line 111, column 12 - line 113, column 30): " + [v.constructor.name]);
        }
        ;
        while (!$tco_done1) {
          $tco_result = $tco_loop($tco_var_b, $copy_v);
        }
        ;
        return $tco_result;
      };
    };
    return go2;
  },
  foldMap: function(dictMonoid) {
    var append22 = append(dictMonoid.Semigroup0());
    var mempty3 = mempty(dictMonoid);
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $286 = append22(acc);
        return function($287) {
          return $286(f($287));
        };
      })(mempty3);
    };
  }
};
var foldr2 = /* @__PURE__ */ foldr(foldableList);
var semigroupList = {
  append: function(xs) {
    return function(ys) {
      return foldr2(Cons.create)(ys)(xs);
    };
  }
};
var append1 = /* @__PURE__ */ append(semigroupList);
var semigroupNonEmptyList = {
  append: function(v) {
    return function(as$prime) {
      return new NonEmpty(v.value0, append1(v.value1)(toList(as$prime)));
    };
  }
};
var altList = {
  alt: append1,
  Functor0: function() {
    return functorList;
  }
};
var plusList = /* @__PURE__ */ function() {
  return {
    empty: Nil.value,
    Alt0: function() {
      return altList;
    }
  };
}();

// output/Effect.Aff/foreign.js
var Aff = function() {
  var EMPTY = {};
  var PURE = "Pure";
  var THROW = "Throw";
  var CATCH = "Catch";
  var SYNC = "Sync";
  var ASYNC = "Async";
  var BIND = "Bind";
  var BRACKET = "Bracket";
  var FORK = "Fork";
  var SEQ = "Sequential";
  var MAP = "Map";
  var APPLY = "Apply";
  var ALT = "Alt";
  var CONS = "Cons";
  var RESUME = "Resume";
  var RELEASE = "Release";
  var FINALIZER = "Finalizer";
  var FINALIZED = "Finalized";
  var FORKED = "Forked";
  var FIBER = "Fiber";
  var THUNK = "Thunk";
  function Aff2(tag, _1, _2, _3) {
    this.tag = tag;
    this._1 = _1;
    this._2 = _2;
    this._3 = _3;
  }
  function AffCtr(tag) {
    var fn = function(_1, _2, _3) {
      return new Aff2(tag, _1, _2, _3);
    };
    fn.tag = tag;
    return fn;
  }
  function nonCanceler2(error4) {
    return new Aff2(PURE, void 0);
  }
  function runEff(eff) {
    try {
      eff();
    } catch (error4) {
      setTimeout(function() {
        throw error4;
      }, 0);
    }
  }
  function runSync(left, right, eff) {
    try {
      return right(eff());
    } catch (error4) {
      return left(error4);
    }
  }
  function runAsync(left, eff, k) {
    try {
      return eff(k)();
    } catch (error4) {
      k(left(error4))();
      return nonCanceler2;
    }
  }
  var Scheduler = function() {
    var limit = 1024;
    var size4 = 0;
    var ix = 0;
    var queue = new Array(limit);
    var draining = false;
    function drain() {
      var thunk;
      draining = true;
      while (size4 !== 0) {
        size4--;
        thunk = queue[ix];
        queue[ix] = void 0;
        ix = (ix + 1) % limit;
        thunk();
      }
      draining = false;
    }
    return {
      isDraining: function() {
        return draining;
      },
      enqueue: function(cb) {
        var i2, tmp;
        if (size4 === limit) {
          tmp = draining;
          drain();
          draining = tmp;
        }
        queue[(ix + size4) % limit] = cb;
        size4++;
        if (!draining) {
          drain();
        }
      }
    };
  }();
  function Supervisor(util) {
    var fibers = {};
    var fiberId = 0;
    var count = 0;
    return {
      register: function(fiber) {
        var fid = fiberId++;
        fiber.onComplete({
          rethrow: true,
          handler: function(result) {
            return function() {
              count--;
              delete fibers[fid];
            };
          }
        })();
        fibers[fid] = fiber;
        count++;
      },
      isEmpty: function() {
        return count === 0;
      },
      killAll: function(killError, cb) {
        return function() {
          if (count === 0) {
            return cb();
          }
          var killCount = 0;
          var kills = {};
          function kill2(fid) {
            kills[fid] = fibers[fid].kill(killError, function(result) {
              return function() {
                delete kills[fid];
                killCount--;
                if (util.isLeft(result) && util.fromLeft(result)) {
                  setTimeout(function() {
                    throw util.fromLeft(result);
                  }, 0);
                }
                if (killCount === 0) {
                  cb();
                }
              };
            })();
          }
          for (var k in fibers) {
            if (fibers.hasOwnProperty(k)) {
              killCount++;
              kill2(k);
            }
          }
          fibers = {};
          fiberId = 0;
          count = 0;
          return function(error4) {
            return new Aff2(SYNC, function() {
              for (var k2 in kills) {
                if (kills.hasOwnProperty(k2)) {
                  kills[k2]();
                }
              }
            });
          };
        };
      }
    };
  }
  var SUSPENDED = 0;
  var CONTINUE = 1;
  var STEP_BIND = 2;
  var STEP_RESULT = 3;
  var PENDING = 4;
  var RETURN = 5;
  var COMPLETED = 6;
  function Fiber(util, supervisor, aff) {
    var runTick = 0;
    var status = SUSPENDED;
    var step3 = aff;
    var fail2 = null;
    var interrupt = null;
    var bhead = null;
    var btail = null;
    var attempts = null;
    var bracketCount = 0;
    var joinId = 0;
    var joins = null;
    var rethrow = true;
    function run3(localRunTick) {
      var tmp, result, attempt;
      while (true) {
        tmp = null;
        result = null;
        attempt = null;
        switch (status) {
          case STEP_BIND:
            status = CONTINUE;
            try {
              step3 = bhead(step3);
              if (btail === null) {
                bhead = null;
              } else {
                bhead = btail._1;
                btail = btail._2;
              }
            } catch (e) {
              status = RETURN;
              fail2 = util.left(e);
              step3 = null;
            }
            break;
          case STEP_RESULT:
            if (util.isLeft(step3)) {
              status = RETURN;
              fail2 = step3;
              step3 = null;
            } else if (bhead === null) {
              status = RETURN;
            } else {
              status = STEP_BIND;
              step3 = util.fromRight(step3);
            }
            break;
          case CONTINUE:
            switch (step3.tag) {
              case BIND:
                if (bhead) {
                  btail = new Aff2(CONS, bhead, btail);
                }
                bhead = step3._2;
                status = CONTINUE;
                step3 = step3._1;
                break;
              case PURE:
                if (bhead === null) {
                  status = RETURN;
                  step3 = util.right(step3._1);
                } else {
                  status = STEP_BIND;
                  step3 = step3._1;
                }
                break;
              case SYNC:
                status = STEP_RESULT;
                step3 = runSync(util.left, util.right, step3._1);
                break;
              case ASYNC:
                status = PENDING;
                step3 = runAsync(util.left, step3._1, function(result2) {
                  return function() {
                    if (runTick !== localRunTick) {
                      return;
                    }
                    runTick++;
                    Scheduler.enqueue(function() {
                      if (runTick !== localRunTick + 1) {
                        return;
                      }
                      status = STEP_RESULT;
                      step3 = result2;
                      run3(runTick);
                    });
                  };
                });
                return;
              case THROW:
                status = RETURN;
                fail2 = util.left(step3._1);
                step3 = null;
                break;
              case CATCH:
                if (bhead === null) {
                  attempts = new Aff2(CONS, step3, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status = CONTINUE;
                step3 = step3._1;
                break;
              case BRACKET:
                bracketCount++;
                if (bhead === null) {
                  attempts = new Aff2(CONS, step3, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step3, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status = CONTINUE;
                step3 = step3._1;
                break;
              case FORK:
                status = STEP_RESULT;
                tmp = Fiber(util, supervisor, step3._2);
                if (supervisor) {
                  supervisor.register(tmp);
                }
                if (step3._1) {
                  tmp.run();
                }
                step3 = util.right(tmp);
                break;
              case SEQ:
                status = CONTINUE;
                step3 = sequential3(util, supervisor, step3._1);
                break;
            }
            break;
          case RETURN:
            bhead = null;
            btail = null;
            if (attempts === null) {
              status = COMPLETED;
              step3 = interrupt || fail2 || step3;
            } else {
              tmp = attempts._3;
              attempt = attempts._1;
              attempts = attempts._2;
              switch (attempt.tag) {
                case CATCH:
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    status = RETURN;
                  } else if (fail2) {
                    status = CONTINUE;
                    step3 = attempt._2(util.fromLeft(fail2));
                    fail2 = null;
                  }
                  break;
                case RESUME:
                  if (interrupt && interrupt !== tmp && bracketCount === 0 || fail2) {
                    status = RETURN;
                  } else {
                    bhead = attempt._1;
                    btail = attempt._2;
                    status = STEP_BIND;
                    step3 = util.fromRight(step3);
                  }
                  break;
                case BRACKET:
                  bracketCount--;
                  if (fail2 === null) {
                    result = util.fromRight(step3);
                    attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                    if (interrupt === tmp || bracketCount > 0) {
                      status = CONTINUE;
                      step3 = attempt._3(result);
                    }
                  }
                  break;
                case RELEASE:
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                  status = CONTINUE;
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    step3 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                  } else if (fail2) {
                    step3 = attempt._1.failed(util.fromLeft(fail2))(attempt._2);
                  } else {
                    step3 = attempt._1.completed(util.fromRight(step3))(attempt._2);
                  }
                  fail2 = null;
                  bracketCount++;
                  break;
                case FINALIZER:
                  bracketCount++;
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step3, fail2), attempts, interrupt);
                  status = CONTINUE;
                  step3 = attempt._1;
                  break;
                case FINALIZED:
                  bracketCount--;
                  status = RETURN;
                  step3 = attempt._1;
                  fail2 = attempt._2;
                  break;
              }
            }
            break;
          case COMPLETED:
            for (var k in joins) {
              if (joins.hasOwnProperty(k)) {
                rethrow = rethrow && joins[k].rethrow;
                runEff(joins[k].handler(step3));
              }
            }
            joins = null;
            if (interrupt && fail2) {
              setTimeout(function() {
                throw util.fromLeft(fail2);
              }, 0);
            } else if (util.isLeft(step3) && rethrow) {
              setTimeout(function() {
                if (rethrow) {
                  throw util.fromLeft(step3);
                }
              }, 0);
            }
            return;
          case SUSPENDED:
            status = CONTINUE;
            break;
          case PENDING:
            return;
        }
      }
    }
    function onComplete(join4) {
      return function() {
        if (status === COMPLETED) {
          rethrow = rethrow && join4.rethrow;
          join4.handler(step3)();
          return function() {
          };
        }
        var jid = joinId++;
        joins = joins || {};
        joins[jid] = join4;
        return function() {
          if (joins !== null) {
            delete joins[jid];
          }
        };
      };
    }
    function kill2(error4, cb) {
      return function() {
        if (status === COMPLETED) {
          cb(util.right(void 0))();
          return function() {
          };
        }
        var canceler = onComplete({
          rethrow: false,
          handler: function() {
            return cb(util.right(void 0));
          }
        })();
        switch (status) {
          case SUSPENDED:
            interrupt = util.left(error4);
            status = COMPLETED;
            step3 = interrupt;
            run3(runTick);
            break;
          case PENDING:
            if (interrupt === null) {
              interrupt = util.left(error4);
            }
            if (bracketCount === 0) {
              if (status === PENDING) {
                attempts = new Aff2(CONS, new Aff2(FINALIZER, step3(error4)), attempts, interrupt);
              }
              status = RETURN;
              step3 = null;
              fail2 = null;
              run3(++runTick);
            }
            break;
          default:
            if (interrupt === null) {
              interrupt = util.left(error4);
            }
            if (bracketCount === 0) {
              status = RETURN;
              step3 = null;
              fail2 = null;
            }
        }
        return canceler;
      };
    }
    function join3(cb) {
      return function() {
        var canceler = onComplete({
          rethrow: false,
          handler: cb
        })();
        if (status === SUSPENDED) {
          run3(runTick);
        }
        return canceler;
      };
    }
    return {
      kill: kill2,
      join: join3,
      onComplete,
      isSuspended: function() {
        return status === SUSPENDED;
      },
      run: function() {
        if (status === SUSPENDED) {
          if (!Scheduler.isDraining()) {
            Scheduler.enqueue(function() {
              run3(runTick);
            });
          } else {
            run3(runTick);
          }
        }
      }
    };
  }
  function runPar(util, supervisor, par, cb) {
    var fiberId = 0;
    var fibers = {};
    var killId = 0;
    var kills = {};
    var early = new Error("[ParAff] Early exit");
    var interrupt = null;
    var root = EMPTY;
    function kill2(error4, par2, cb2) {
      var step3 = par2;
      var head2 = null;
      var tail = null;
      var count = 0;
      var kills2 = {};
      var tmp, kid;
      loop:
        while (true) {
          tmp = null;
          switch (step3.tag) {
            case FORKED:
              if (step3._3 === EMPTY) {
                tmp = fibers[step3._1];
                kills2[count++] = tmp.kill(error4, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head2 === null) {
                break loop;
              }
              step3 = head2._2;
              if (tail === null) {
                head2 = null;
              } else {
                head2 = tail._1;
                tail = tail._2;
              }
              break;
            case MAP:
              step3 = step3._2;
              break;
            case APPLY:
            case ALT:
              if (head2) {
                tail = new Aff2(CONS, head2, tail);
              }
              head2 = step3;
              step3 = step3._1;
              break;
          }
        }
      if (count === 0) {
        cb2(util.right(void 0))();
      } else {
        kid = 0;
        tmp = count;
        for (; kid < tmp; kid++) {
          kills2[kid] = kills2[kid]();
        }
      }
      return kills2;
    }
    function join3(result, head2, tail) {
      var fail2, step3, lhs, rhs, tmp, kid;
      if (util.isLeft(result)) {
        fail2 = result;
        step3 = null;
      } else {
        step3 = result;
        fail2 = null;
      }
      loop:
        while (true) {
          lhs = null;
          rhs = null;
          tmp = null;
          kid = null;
          if (interrupt !== null) {
            return;
          }
          if (head2 === null) {
            cb(fail2 || step3)();
            return;
          }
          if (head2._3 !== EMPTY) {
            return;
          }
          switch (head2.tag) {
            case MAP:
              if (fail2 === null) {
                head2._3 = util.right(head2._1(util.fromRight(step3)));
                step3 = head2._3;
              } else {
                head2._3 = fail2;
              }
              break;
            case APPLY:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (fail2) {
                head2._3 = fail2;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail2 === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(fail2, null, null);
                    } else {
                      join3(fail2, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              } else if (lhs === EMPTY || rhs === EMPTY) {
                return;
              } else {
                step3 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head2._3 = step3;
              }
              break;
            case ALT:
              lhs = head2._1._3;
              rhs = head2._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail2 = step3 === lhs ? rhs : lhs;
                step3 = null;
                head2._3 = fail2;
              } else {
                head2._3 = step3;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step3 === lhs ? head2._2 : head2._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail === null) {
                      join3(step3, null, null);
                    } else {
                      join3(step3, tail._1, tail._2);
                    }
                  };
                });
                if (tmp) {
                  tmp = false;
                  return;
                }
              }
              break;
          }
          if (tail === null) {
            head2 = null;
          } else {
            head2 = tail._1;
            tail = tail._2;
          }
        }
    }
    function resolve(fiber) {
      return function(result) {
        return function() {
          delete fibers[fiber._1];
          fiber._3 = result;
          join3(result, fiber._2._1, fiber._2._2);
        };
      };
    }
    function run3() {
      var status = CONTINUE;
      var step3 = par;
      var head2 = null;
      var tail = null;
      var tmp, fid;
      loop:
        while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step3.tag) {
                case MAP:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(MAP, step3._1, EMPTY, EMPTY);
                  step3 = step3._2;
                  break;
                case APPLY:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(APPLY, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                case ALT:
                  if (head2) {
                    tail = new Aff2(CONS, head2, tail);
                  }
                  head2 = new Aff2(ALT, EMPTY, step3._2, EMPTY);
                  step3 = step3._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step3;
                  step3 = new Aff2(FORKED, fid, new Aff2(CONS, head2, tail), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step3)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head2 === null) {
                break loop;
              }
              if (head2._1 === EMPTY) {
                head2._1 = step3;
                status = CONTINUE;
                step3 = head2._2;
                head2._2 = EMPTY;
              } else {
                head2._2 = step3;
                step3 = head2;
                if (tail === null) {
                  head2 = null;
                } else {
                  head2 = tail._1;
                  tail = tail._2;
                }
              }
          }
        }
      root = step3;
      for (fid = 0; fid < fiberId; fid++) {
        fibers[fid].run();
      }
    }
    function cancel(error4, cb2) {
      interrupt = util.left(error4);
      var innerKills;
      for (var kid in kills) {
        if (kills.hasOwnProperty(kid)) {
          innerKills = kills[kid];
          for (kid in innerKills) {
            if (innerKills.hasOwnProperty(kid)) {
              innerKills[kid]();
            }
          }
        }
      }
      kills = null;
      var newKills = kill2(error4, root, cb2);
      return function(killError) {
        return new Aff2(ASYNC, function(killCb) {
          return function() {
            for (var kid2 in newKills) {
              if (newKills.hasOwnProperty(kid2)) {
                newKills[kid2]();
              }
            }
            return nonCanceler2;
          };
        });
      };
    }
    run3();
    return function(killError) {
      return new Aff2(ASYNC, function(killCb) {
        return function() {
          return cancel(killError, killCb);
        };
      });
    };
  }
  function sequential3(util, supervisor, par) {
    return new Aff2(ASYNC, function(cb) {
      return function() {
        return runPar(util, supervisor, par, cb);
      };
    });
  }
  Aff2.EMPTY = EMPTY;
  Aff2.Pure = AffCtr(PURE);
  Aff2.Throw = AffCtr(THROW);
  Aff2.Catch = AffCtr(CATCH);
  Aff2.Sync = AffCtr(SYNC);
  Aff2.Async = AffCtr(ASYNC);
  Aff2.Bind = AffCtr(BIND);
  Aff2.Bracket = AffCtr(BRACKET);
  Aff2.Fork = AffCtr(FORK);
  Aff2.Seq = AffCtr(SEQ);
  Aff2.ParMap = AffCtr(MAP);
  Aff2.ParApply = AffCtr(APPLY);
  Aff2.ParAlt = AffCtr(ALT);
  Aff2.Fiber = Fiber;
  Aff2.Supervisor = Supervisor;
  Aff2.Scheduler = Scheduler;
  Aff2.nonCanceler = nonCanceler2;
  return Aff2;
}();
var _pure = Aff.Pure;
var _throwError = Aff.Throw;
function _catchError(aff) {
  return function(k) {
    return Aff.Catch(aff, k);
  };
}
function _map(f) {
  return function(aff) {
    if (aff.tag === Aff.Pure.tag) {
      return Aff.Pure(f(aff._1));
    } else {
      return Aff.Bind(aff, function(value13) {
        return Aff.Pure(f(value13));
      });
    }
  };
}
function _bind(aff) {
  return function(k) {
    return Aff.Bind(aff, k);
  };
}
function _fork(immediate) {
  return function(aff) {
    return Aff.Fork(immediate, aff);
  };
}
var _liftEffect = Aff.Sync;
function _parAffMap(f) {
  return function(aff) {
    return Aff.ParMap(f, aff);
  };
}
function _parAffApply(aff1) {
  return function(aff2) {
    return Aff.ParApply(aff1, aff2);
  };
}
var makeAff = Aff.Async;
function generalBracket(acquire) {
  return function(options2) {
    return function(k) {
      return Aff.Bracket(acquire, options2, k);
    };
  };
}
function _makeFiber(util, aff) {
  return function() {
    return Aff.Fiber(util, null, aff);
  };
}
var _sequential = Aff.Seq;

// output/Control.Parallel.Class/index.js
var sequential = function(dict) {
  return dict.sequential;
};
var parallel = function(dict) {
  return dict.parallel;
};

// output/Control.Parallel/index.js
var identity5 = /* @__PURE__ */ identity(categoryFn);
var parTraverse_ = function(dictParallel) {
  var sequential3 = sequential(dictParallel);
  var parallel4 = parallel(dictParallel);
  return function(dictApplicative) {
    var traverse_7 = traverse_(dictApplicative);
    return function(dictFoldable) {
      var traverse_14 = traverse_7(dictFoldable);
      return function(f) {
        var $51 = traverse_14(function($53) {
          return parallel4(f($53));
        });
        return function($52) {
          return sequential3($51($52));
        };
      };
    };
  };
};
var parSequence_ = function(dictParallel) {
  var parTraverse_1 = parTraverse_(dictParallel);
  return function(dictApplicative) {
    var parTraverse_2 = parTraverse_1(dictApplicative);
    return function(dictFoldable) {
      return parTraverse_2(dictFoldable)(identity5);
    };
  };
};

// output/Effect.Unsafe/foreign.js
var unsafePerformEffect = function(f) {
  return f();
};

// output/Partial.Unsafe/foreign.js
var _unsafePartial = function(f) {
  return f();
};

// output/Partial/foreign.js
var _crashWith = function(msg) {
  throw new Error(msg);
};

// output/Partial/index.js
var crashWith = function() {
  return _crashWith;
};

// output/Partial.Unsafe/index.js
var crashWith2 = /* @__PURE__ */ crashWith();
var unsafePartial = _unsafePartial;
var unsafeCrashWith = function(msg) {
  return unsafePartial(function() {
    return crashWith2(msg);
  });
};

// output/Effect.Aff/index.js
var $runtime_lazy2 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var pure2 = /* @__PURE__ */ pure(applicativeEffect);
var $$void3 = /* @__PURE__ */ $$void(functorEffect);
var map5 = /* @__PURE__ */ map(functorEffect);
var Canceler = function(x) {
  return x;
};
var suspendAff = /* @__PURE__ */ _fork(false);
var functorParAff = {
  map: _parAffMap
};
var functorAff = {
  map: _map
};
var map1 = /* @__PURE__ */ map(functorAff);
var forkAff = /* @__PURE__ */ _fork(true);
var ffiUtil = /* @__PURE__ */ function() {
  var unsafeFromRight = function(v) {
    if (v instanceof Right) {
      return v.value0;
    }
    ;
    if (v instanceof Left) {
      return unsafeCrashWith("unsafeFromRight: Left");
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 412, column 21 - line 414, column 54): " + [v.constructor.name]);
  };
  var unsafeFromLeft = function(v) {
    if (v instanceof Left) {
      return v.value0;
    }
    ;
    if (v instanceof Right) {
      return unsafeCrashWith("unsafeFromLeft: Right");
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 407, column 20 - line 409, column 55): " + [v.constructor.name]);
  };
  var isLeft = function(v) {
    if (v instanceof Left) {
      return true;
    }
    ;
    if (v instanceof Right) {
      return false;
    }
    ;
    throw new Error("Failed pattern match at Effect.Aff (line 402, column 12 - line 404, column 21): " + [v.constructor.name]);
  };
  return {
    isLeft,
    fromLeft: unsafeFromLeft,
    fromRight: unsafeFromRight,
    left: Left.create,
    right: Right.create
  };
}();
var makeFiber = function(aff) {
  return _makeFiber(ffiUtil, aff);
};
var launchAff = function(aff) {
  return function __do4() {
    var fiber = makeFiber(aff)();
    fiber.run();
    return fiber;
  };
};
var bracket = function(acquire) {
  return function(completed) {
    return generalBracket(acquire)({
      killed: $$const(completed),
      failed: $$const(completed),
      completed: $$const(completed)
    });
  };
};
var applyParAff = {
  apply: _parAffApply,
  Functor0: function() {
    return functorParAff;
  }
};
var monadAff = {
  Applicative0: function() {
    return applicativeAff;
  },
  Bind1: function() {
    return bindAff;
  }
};
var bindAff = {
  bind: _bind,
  Apply0: function() {
    return $lazy_applyAff(0);
  }
};
var applicativeAff = {
  pure: _pure,
  Apply0: function() {
    return $lazy_applyAff(0);
  }
};
var $lazy_applyAff = /* @__PURE__ */ $runtime_lazy2("applyAff", "Effect.Aff", function() {
  return {
    apply: ap(monadAff),
    Functor0: function() {
      return functorAff;
    }
  };
});
var applyAff = /* @__PURE__ */ $lazy_applyAff(73);
var pure22 = /* @__PURE__ */ pure(applicativeAff);
var bind1 = /* @__PURE__ */ bind(bindAff);
var bindFlipped3 = /* @__PURE__ */ bindFlipped(bindAff);
var $$finally = function(fin) {
  return function(a2) {
    return bracket(pure22(unit))($$const(fin))($$const(a2));
  };
};
var parallelAff = {
  parallel: unsafeCoerce2,
  sequential: _sequential,
  Apply0: function() {
    return applyAff;
  },
  Apply1: function() {
    return applyParAff;
  }
};
var parallel2 = /* @__PURE__ */ parallel(parallelAff);
var applicativeParAff = {
  pure: function($76) {
    return parallel2(pure22($76));
  },
  Apply0: function() {
    return applyParAff;
  }
};
var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableArray);
var semigroupCanceler = {
  append: function(v) {
    return function(v1) {
      return function(err) {
        return parSequence_2([v(err), v1(err)]);
      };
    };
  }
};
var monadEffectAff = {
  liftEffect: _liftEffect,
  Monad0: function() {
    return monadAff;
  }
};
var liftEffect2 = /* @__PURE__ */ liftEffect(monadEffectAff);
var effectCanceler = function($77) {
  return Canceler($$const(liftEffect2($77)));
};
var joinFiber = function(v) {
  return makeAff(function(k) {
    return map5(effectCanceler)(v.join(k));
  });
};
var functorFiber = {
  map: function(f) {
    return function(t) {
      return unsafePerformEffect(makeFiber(map1(f)(joinFiber(t))));
    };
  }
};
var killFiber = function(e) {
  return function(v) {
    return bind1(liftEffect2(v.isSuspended))(function(suspended) {
      if (suspended) {
        return liftEffect2($$void3(v.kill(e, $$const(pure2(unit)))));
      }
      ;
      return makeAff(function(k) {
        return map5(effectCanceler)(v.kill(e, k));
      });
    });
  };
};
var monadThrowAff = {
  throwError: _throwError,
  Monad0: function() {
    return monadAff;
  }
};
var monadErrorAff = {
  catchError: _catchError,
  MonadThrow0: function() {
    return monadThrowAff;
  }
};
var $$try2 = /* @__PURE__ */ $$try(monadErrorAff);
var runAff = function(k) {
  return function(aff) {
    return launchAff(bindFlipped3(function($83) {
      return liftEffect2(k($83));
    })($$try2(aff)));
  };
};
var runAff_ = function(k) {
  return function(aff) {
    return $$void3(runAff(k)(aff));
  };
};
var monadRecAff = {
  tailRecM: function(k) {
    var go2 = function(a2) {
      return bind1(k(a2))(function(res) {
        if (res instanceof Done) {
          return pure22(res.value0);
        }
        ;
        if (res instanceof Loop) {
          return go2(res.value0);
        }
        ;
        throw new Error("Failed pattern match at Effect.Aff (line 104, column 7 - line 106, column 23): " + [res.constructor.name]);
      });
    };
    return go2;
  },
  Monad0: function() {
    return monadAff;
  }
};
var nonCanceler = /* @__PURE__ */ $$const(/* @__PURE__ */ pure22(unit));
var monoidCanceler = {
  mempty: nonCanceler,
  Semigroup0: function() {
    return semigroupCanceler;
  }
};

// output/Foreign/foreign.js
function typeOf(value13) {
  return typeof value13;
}
function tagOf(value13) {
  return Object.prototype.toString.call(value13).slice(8, -1);
}
var isArray = Array.isArray || function(value13) {
  return Object.prototype.toString.call(value13) === "[object Array]";
};

// output/Data.List/index.js
var reverse = /* @__PURE__ */ function() {
  var go2 = function($copy_v) {
    return function($copy_v1) {
      var $tco_var_v = $copy_v;
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v, v1) {
        if (v1 instanceof Nil) {
          $tco_done = true;
          return v;
        }
        ;
        if (v1 instanceof Cons) {
          $tco_var_v = new Cons(v1.value0, v);
          $copy_v1 = v1.value1;
          return;
        }
        ;
        throw new Error("Failed pattern match at Data.List (line 368, column 3 - line 368, column 19): " + [v.constructor.name, v1.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($tco_var_v, $copy_v1);
      }
      ;
      return $tco_result;
    };
  };
  return go2(Nil.value);
}();
var $$null = function(v) {
  if (v instanceof Nil) {
    return true;
  }
  ;
  return false;
};

// output/Data.List.NonEmpty/index.js
var singleton3 = /* @__PURE__ */ function() {
  var $200 = singleton2(plusList);
  return function($201) {
    return NonEmptyList($200($201));
  };
}();
var cons = function(y) {
  return function(v) {
    return new NonEmpty(y, new Cons(v.value0, v.value1));
  };
};

// output/Foreign/index.js
var TypeMismatch = /* @__PURE__ */ function() {
  function TypeMismatch2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  TypeMismatch2.create = function(value0) {
    return function(value1) {
      return new TypeMismatch2(value0, value1);
    };
  };
  return TypeMismatch2;
}();
var unsafeFromForeign = unsafeCoerce2;
var fail = function(dictMonad) {
  var $153 = throwError(monadThrowExceptT(dictMonad));
  return function($154) {
    return $153(singleton3($154));
  };
};
var unsafeReadTagged = function(dictMonad) {
  var pure13 = pure(applicativeExceptT(dictMonad));
  var fail1 = fail(dictMonad);
  return function(tag) {
    return function(value13) {
      if (tagOf(value13) === tag) {
        return pure13(unsafeFromForeign(value13));
      }
      ;
      if (otherwise) {
        return fail1(new TypeMismatch(tag, tagOf(value13)));
      }
      ;
      throw new Error("Failed pattern match at Foreign (line 123, column 1 - line 123, column 104): " + [tag.constructor.name, value13.constructor.name]);
    };
  };
};
var readString = function(dictMonad) {
  return unsafeReadTagged(dictMonad)("String");
};

// output/Control.Promise/index.js
var voidRight2 = /* @__PURE__ */ voidRight(functorEffect);
var mempty2 = /* @__PURE__ */ mempty(monoidCanceler);
var identity6 = /* @__PURE__ */ identity(categoryFn);
var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
var map6 = /* @__PURE__ */ map(/* @__PURE__ */ functorExceptT(functorIdentity));
var readString2 = /* @__PURE__ */ readString(monadIdentity);
var bind2 = /* @__PURE__ */ bind(bindAff);
var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
var toAff$prime = function(customCoerce) {
  return function(p2) {
    return makeAff(function(cb) {
      return voidRight2(mempty2)(thenImpl(p2)(function($14) {
        return cb(Left.create(customCoerce($14)))();
      })(function($15) {
        return cb(Right.create($15))();
      }));
    });
  };
};
var coerce3 = function(fn) {
  return either(function(v) {
    return error("Promise failed, couldn't extract JS Error or String");
  })(identity6)(runExcept(alt2(unsafeReadTagged2("Error")(fn))(map6(error)(readString2(fn)))));
};
var toAff = /* @__PURE__ */ toAff$prime(coerce3);
var toAffE = function(f) {
  return bind2(liftEffect3(f))(toAff);
};

// output/Data.Array/foreign.js
var replicateFill = function(count, value13) {
  if (count < 1) {
    return [];
  }
  var result = new Array(count);
  return result.fill(value13);
};
var replicatePolyfill = function(count, value13) {
  var result = [];
  var n = 0;
  for (var i2 = 0; i2 < count; i2++) {
    result[n++] = value13;
  }
  return result;
};
var replicateImpl = typeof Array.prototype.fill === "function" ? replicateFill : replicatePolyfill;
var length3 = function(xs) {
  return xs.length;
};
var findIndexImpl = function(just, nothing, f, xs) {
  for (var i2 = 0, l = xs.length; i2 < l; i2++) {
    if (f(xs[i2]))
      return just(i2);
  }
  return nothing;
};
var _deleteAt = function(just, nothing, i2, l) {
  if (i2 < 0 || i2 >= l.length)
    return nothing;
  var l1 = l.slice();
  l1.splice(i2, 1);
  return just(l1);
};

// output/Data.Function.Uncurried/foreign.js
var runFn4 = function(fn) {
  return function(a2) {
    return function(b2) {
      return function(c) {
        return function(d) {
          return fn(a2, b2, c, d);
        };
      };
    };
  };
};

// output/Data.Array/index.js
var fromJust2 = /* @__PURE__ */ fromJust();
var traverse2 = /* @__PURE__ */ traverse(traversableArray);
var singleton5 = function(a2) {
  return [a2];
};
var findIndex2 = /* @__PURE__ */ function() {
  return runFn4(findIndexImpl)(Just.create)(Nothing.value);
}();
var deleteAt = /* @__PURE__ */ function() {
  return runFn4(_deleteAt)(Just.create)(Nothing.value);
}();
var deleteBy = function(v) {
  return function(v1) {
    return function(v2) {
      if (v2.length === 0) {
        return [];
      }
      ;
      return maybe(v2)(function(i2) {
        return fromJust2(deleteAt(i2)(v2));
      })(findIndex2(v(v1))(v2));
    };
  };
};
var concatMap = /* @__PURE__ */ flip(/* @__PURE__ */ bind(bindArray));
var mapMaybe2 = function(f) {
  return concatMap(function() {
    var $189 = maybe([])(singleton5);
    return function($190) {
      return $189(f($190));
    };
  }());
};
var filterA = function(dictApplicative) {
  var traverse12 = traverse2(dictApplicative);
  var map32 = map(dictApplicative.Apply0().Functor0());
  return function(p2) {
    var $191 = map32(mapMaybe2(function(v) {
      if (v.value1) {
        return new Just(v.value0);
      }
      ;
      return Nothing.value;
    }));
    var $192 = traverse12(function(x) {
      return map32(Tuple.create(x))(p2(x));
    });
    return function($193) {
      return $191($192($193));
    };
  };
};

// output/Data.Nullable/foreign.js
var nullImpl = null;
function nullable(a2, r, f) {
  return a2 == null ? r : f(a2);
}
function notNull(x) {
  return x;
}

// output/Data.Nullable/index.js
var toNullable = /* @__PURE__ */ maybe(nullImpl)(notNull);
var toMaybe = function(n) {
  return nullable(n, Nothing.value, Just.create);
};

// output/Foreign.Object/foreign.js
function _lookup(no, yes, k, m) {
  return k in m ? yes(m[k]) : no;
}
function toArrayWithKey(f) {
  return function(m) {
    var r = [];
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r.push(f(k)(m[k]));
      }
    }
    return r;
  };
}
var keys = Object.keys || toArrayWithKey(function(k) {
  return function() {
    return k;
  };
});

// output/Foreign.Object.ST/foreign.js
var newImpl = function() {
  return {};
};

// output/Foreign.Object/index.js
var lookup = /* @__PURE__ */ function() {
  return runFn4(_lookup)(Nothing.value)(Just.create);
}();

// output/Cardano.Wallet.Cip30/index.js
var filterA2 = /* @__PURE__ */ filterA(applicativeEffect);
var getBalance = function(api) {
  return toAffE(_getBalance(api));
};
var enable = function(walletName) {
  return function(exts) {
    return toAffE(_getWalletApi(walletName)(exts));
  };
};
var allWallets = allWalletTags;
var getAvailableWallets = function __do() {
  var wallets = allWallets();
  return filterA2(isWalletAvailable)(wallets)();
};

// output/Csl/foreign.js
import * as CSL from "@emurgo/cardano-serialization-lib-browser";
function errorableToPurs(f, ...vars) {
  return (left) => (right) => {
    try {
      return right(f(...vars));
    } catch (err) {
      if (typeof err === "string")
        return left(err);
      else
        return left(err.message);
    }
  };
}
var bigInt_one = CSL.BigInt.one();
var bigNum_free = (self) => () => self.free();
var bigNum_toBytes = (self) => self.to_bytes();
var bigNum_fromBytes = (bytes) => errorableToPurs(CSL.BigNum.from_bytes, bytes);
var bigNum_toHex = (self) => self.to_hex();
var bigNum_fromHex = (hex_str) => errorableToPurs(CSL.BigNum.from_hex, hex_str);
var bigNum_toJson = (self) => self.to_json();
var bigNum_toJsValue = (self) => self.to_js_value();
var bigNum_fromJson = (json) => errorableToPurs(CSL.BigNum.from_json, json);
var bigNum_fromStr = (string) => errorableToPurs(CSL.BigNum.from_str, string);
var bigNum_toStr = (self) => self.to_str();
var bigNum_zero = CSL.BigNum.zero();
var bigNum_one = CSL.BigNum.one();
var bigNum_isZero = (self) => self.is_zero();
var bigNum_divFloor = (self) => (other) => self.div_floor(other);
var bigNum_checkedMul = (self) => (other) => self.checked_mul(other);
var bigNum_checkedAdd = (self) => (other) => self.checked_add(other);
var bigNum_checkedSub = (self) => (other) => self.checked_sub(other);
var bigNum_clampedSub = (self) => (other) => self.clamped_sub(other);
var bigNum_compare = (self) => (rhs_value) => self.compare(rhs_value);
var bigNum_lessThan = (self) => (rhs_value) => self.less_than(rhs_value);
var bigNum_max = (a2) => (b2) => CSL.BigNum.max(a2, b2);
var bip32PrivateKey_generateEd25519Bip32 = CSL.Bip32PrivateKey.generate_ed25519_bip32();
var ed25519KeyHashes_new = CSL.Ed25519KeyHashes.new();
var language_newPlutusV1 = CSL.Language.new_plutus_v1();
var language_newPlutusV2 = CSL.Language.new_plutus_v2();
var networkId_testnet = CSL.NetworkId.testnet();
var networkId_mainnet = CSL.NetworkId.mainnet();
var networkInfo_testnet = CSL.NetworkInfo.testnet();
var networkInfo_mainnet = CSL.NetworkInfo.mainnet();
var nonce_newIdentity = CSL.Nonce.new_identity();
var privateKey_generateEd25519 = CSL.PrivateKey.generate_ed25519();
var privateKey_generateEd25519extended = CSL.PrivateKey.generate_ed25519extended();
var protocolParamUpdate_new = CSL.ProtocolParamUpdate.new();
var redeemerTag_newSpend = CSL.RedeemerTag.new_spend();
var redeemerTag_newMint = CSL.RedeemerTag.new_mint();
var redeemerTag_newCert = CSL.RedeemerTag.new_cert();
var redeemerTag_newReward = CSL.RedeemerTag.new_reward();
var txBuilderConfigBuilder_new = CSL.TransactionBuilderConfigBuilder.new();
var txOutBuilder_new = CSL.TransactionOutputBuilder.new();
var txBuilderConstants_plutusDefaultCostModels = CSL.TxBuilderConstants.plutus_default_cost_models();
var txBuilderConstants_plutusAlonzoCostModels = CSL.TxBuilderConstants.plutus_alonzo_cost_models();
var txBuilderConstants_plutusVasilCostModels = CSL.TxBuilderConstants.plutus_vasil_cost_models();
var value_free = (self) => () => self.free();
var value_toBytes = (self) => self.to_bytes();
var value_fromBytes = (bytes) => errorableToPurs(CSL.Value.from_bytes, bytes);
var value_toHex = (self) => self.to_hex();
var value_fromHex = (hex_str) => errorableToPurs(CSL.Value.from_hex, hex_str);
var value_toJson = (self) => self.to_json();
var value_toJsValue = (self) => self.to_js_value();
var value_fromJson = (json) => errorableToPurs(CSL.Value.from_json, json);
var value_new = (coin) => CSL.Value.new(coin);
var value_newFromAssets = (multiasset) => CSL.Value.new_from_assets(multiasset);
var value_newWithAssets = (coin) => (multiasset) => CSL.Value.new_with_assets(coin, multiasset);
var value_zero = CSL.Value.zero();
var value_isZero = (self) => self.is_zero();
var value_coin = (self) => self.coin();
var value_setCoin = (self) => (coin) => () => self.set_coin(coin);
var value_multiasset = (self) => self.multiasset();
var value_setMultiasset = (self) => (multiasset) => () => self.set_multiasset(multiasset);
var value_checkedAdd = (self) => (rhs) => self.checked_add(rhs);
var value_checkedSub = (self) => (rhs_value) => self.checked_sub(rhs_value);
var value_clampedSub = (self) => (rhs_value) => self.clamped_sub(rhs_value);
var value_compare = (self) => (rhs_value) => self.compare(rhs_value);

// output/Csl/index.js
var runForeignErrorable = function(f) {
  return f(Left.create)(Right.create);
};
var runForeignMaybe = function($753) {
  return hush(runForeignErrorable($753));
};
var value = {
  free: value_free,
  toBytes: value_toBytes,
  fromBytes: function(a1) {
    return runForeignMaybe(value_fromBytes(a1));
  },
  toHex: value_toHex,
  fromHex: function(a1) {
    return runForeignMaybe(value_fromHex(a1));
  },
  toJson: value_toJson,
  toJsValue: value_toJsValue,
  fromJson: function(a1) {
    return runForeignMaybe(value_fromJson(a1));
  },
  "new": value_new,
  newFromAssets: value_newFromAssets,
  newWithAssets: value_newWithAssets,
  zero: value_zero,
  isZero: value_isZero,
  coin: value_coin,
  setCoin: value_setCoin,
  multiasset: function(a1) {
    return toMaybe(value_multiasset(a1));
  },
  setMultiasset: value_setMultiasset,
  checkedAdd: value_checkedAdd,
  checkedSub: value_checkedSub,
  clampedSub: value_clampedSub,
  compare: function(a1) {
    return function(a2) {
      return toMaybe(value_compare(a1)(a2));
    };
  }
};
var bigNum = {
  free: bigNum_free,
  toBytes: bigNum_toBytes,
  fromBytes: function(a1) {
    return runForeignMaybe(bigNum_fromBytes(a1));
  },
  toHex: bigNum_toHex,
  fromHex: function(a1) {
    return runForeignMaybe(bigNum_fromHex(a1));
  },
  toJson: bigNum_toJson,
  toJsValue: bigNum_toJsValue,
  fromJson: function(a1) {
    return runForeignMaybe(bigNum_fromJson(a1));
  },
  fromStr: function(a1) {
    return runForeignMaybe(bigNum_fromStr(a1));
  },
  toStr: bigNum_toStr,
  zero: bigNum_zero,
  one: bigNum_one,
  isZero: bigNum_isZero,
  divFloor: bigNum_divFloor,
  checkedMul: bigNum_checkedMul,
  checkedAdd: bigNum_checkedAdd,
  checkedSub: bigNum_checkedSub,
  clampedSub: bigNum_clampedSub,
  compare: bigNum_compare,
  lessThan: bigNum_lessThan,
  max: bigNum_max
};

// output/Effect.Aff.Class/index.js
var monadAffAff = {
  liftAff: /* @__PURE__ */ identity(categoryFn),
  MonadEffect0: function() {
    return monadEffectAff;
  }
};
var liftAff = function(dict) {
  return dict.liftAff;
};

// output/Data.Exists/index.js
var runExists = unsafeCoerce2;
var mkExists = unsafeCoerce2;

// output/Data.Coyoneda/index.js
var CoyonedaF = /* @__PURE__ */ function() {
  function CoyonedaF2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  CoyonedaF2.create = function(value0) {
    return function(value1) {
      return new CoyonedaF2(value0, value1);
    };
  };
  return CoyonedaF2;
}();
var unCoyoneda = function(f) {
  return function(v) {
    return runExists(function(v1) {
      return f(v1.value0)(v1.value1);
    })(v);
  };
};
var coyoneda = function(k) {
  return function(fi) {
    return mkExists(new CoyonedaF(k, fi));
  };
};
var functorCoyoneda = {
  map: function(f) {
    return function(v) {
      return runExists(function(v1) {
        return coyoneda(function($180) {
          return f(v1.value0($180));
        })(v1.value1);
      })(v);
    };
  }
};
var liftCoyoneda = /* @__PURE__ */ coyoneda(/* @__PURE__ */ identity(categoryFn));

// output/Data.Map.Internal/index.js
var $runtime_lazy3 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var Leaf = /* @__PURE__ */ function() {
  function Leaf2() {
  }
  ;
  Leaf2.value = new Leaf2();
  return Leaf2;
}();
var Node = /* @__PURE__ */ function() {
  function Node2(value0, value1, value22, value32, value42, value52) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
    this.value3 = value32;
    this.value4 = value42;
    this.value5 = value52;
  }
  ;
  Node2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return function(value32) {
          return function(value42) {
            return function(value52) {
              return new Node2(value0, value1, value22, value32, value42, value52);
            };
          };
        };
      };
    };
  };
  return Node2;
}();
var Split = /* @__PURE__ */ function() {
  function Split2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  Split2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new Split2(value0, value1, value22);
      };
    };
  };
  return Split2;
}();
var SplitLast = /* @__PURE__ */ function() {
  function SplitLast2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  SplitLast2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new SplitLast2(value0, value1, value22);
      };
    };
  };
  return SplitLast2;
}();
var unsafeNode = function(k, v, l, r) {
  if (l instanceof Leaf) {
    if (r instanceof Leaf) {
      return new Node(1, 1, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + r.value0 | 0, 1 + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 702, column 5 - line 706, column 39): " + [r.constructor.name]);
  }
  ;
  if (l instanceof Node) {
    if (r instanceof Leaf) {
      return new Node(1 + l.value0 | 0, 1 + l.value1 | 0, k, v, l, r);
    }
    ;
    if (r instanceof Node) {
      return new Node(1 + function() {
        var $280 = l.value0 > r.value0;
        if ($280) {
          return l.value0;
        }
        ;
        return r.value0;
      }() | 0, (1 + l.value1 | 0) + r.value1 | 0, k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 708, column 5 - line 712, column 68): " + [r.constructor.name]);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 700, column 32 - line 712, column 68): " + [l.constructor.name]);
};
var singleton6 = function(k) {
  return function(v) {
    return new Node(1, 1, k, v, Leaf.value, Leaf.value);
  };
};
var unsafeBalancedNode = /* @__PURE__ */ function() {
  var height8 = function(v) {
    if (v instanceof Leaf) {
      return 0;
    }
    ;
    if (v instanceof Node) {
      return v.value0;
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 757, column 12 - line 759, column 26): " + [v.constructor.name]);
  };
  var rotateLeft = function(k, v, l, rk, rv, rl, rr) {
    if (rl instanceof Node && rl.value0 > height8(rr)) {
      return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
    }
    ;
    return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
  };
  var rotateRight = function(k, v, lk, lv, ll, lr, r) {
    if (lr instanceof Node && height8(ll) <= lr.value0) {
      return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
    }
    ;
    return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
  };
  return function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return singleton6(k)(v);
      }
      ;
      if (r instanceof Node && r.value0 > 1) {
        return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    if (l instanceof Node) {
      if (r instanceof Node) {
        if (r.value0 > (l.value0 + 1 | 0)) {
          return rotateLeft(k, v, l, r.value2, r.value3, r.value4, r.value5);
        }
        ;
        if (l.value0 > (r.value0 + 1 | 0)) {
          return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
        }
        ;
      }
      ;
      if (r instanceof Leaf && l.value0 > 1) {
        return rotateRight(k, v, l.value2, l.value3, l.value4, l.value5, r);
      }
      ;
      return unsafeNode(k, v, l, r);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 717, column 40 - line 738, column 34): " + [l.constructor.name]);
  };
}();
var $lazy_unsafeSplit = /* @__PURE__ */ $runtime_lazy3("unsafeSplit", "Data.Map.Internal", function() {
  return function(comp, k, m) {
    if (m instanceof Leaf) {
      return new Split(Nothing.value, Leaf.value, Leaf.value);
    }
    ;
    if (m instanceof Node) {
      var v = comp(k)(m.value2);
      if (v instanceof LT) {
        var v1 = $lazy_unsafeSplit(793)(comp, k, m.value4);
        return new Split(v1.value0, v1.value1, unsafeBalancedNode(m.value2, m.value3, v1.value2, m.value5));
      }
      ;
      if (v instanceof GT) {
        var v1 = $lazy_unsafeSplit(796)(comp, k, m.value5);
        return new Split(v1.value0, unsafeBalancedNode(m.value2, m.value3, m.value4, v1.value1), v1.value2);
      }
      ;
      if (v instanceof EQ) {
        return new Split(new Just(m.value3), m.value4, m.value5);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 791, column 5 - line 799, column 30): " + [v.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 787, column 34 - line 799, column 30): " + [m.constructor.name]);
  };
});
var unsafeSplit = /* @__PURE__ */ $lazy_unsafeSplit(786);
var $lazy_unsafeSplitLast = /* @__PURE__ */ $runtime_lazy3("unsafeSplitLast", "Data.Map.Internal", function() {
  return function(k, v, l, r) {
    if (r instanceof Leaf) {
      return new SplitLast(k, v, l);
    }
    ;
    if (r instanceof Node) {
      var v1 = $lazy_unsafeSplitLast(779)(r.value2, r.value3, r.value4, r.value5);
      return new SplitLast(v1.value0, v1.value1, unsafeBalancedNode(k, v, l, v1.value2));
    }
    ;
    throw new Error("Failed pattern match at Data.Map.Internal (line 776, column 37 - line 780, column 57): " + [r.constructor.name]);
  };
});
var unsafeSplitLast = /* @__PURE__ */ $lazy_unsafeSplitLast(775);
var unsafeJoinNodes = function(v, v1) {
  if (v instanceof Leaf) {
    return v1;
  }
  ;
  if (v instanceof Node) {
    var v2 = unsafeSplitLast(v.value2, v.value3, v.value4, v.value5);
    return unsafeBalancedNode(v2.value0, v2.value1, v2.value2, v1);
  }
  ;
  throw new Error("Failed pattern match at Data.Map.Internal (line 764, column 25 - line 768, column 38): " + [v.constructor.name, v1.constructor.name]);
};
var lookup2 = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    var go2 = function($copy_v) {
      var $tco_done = false;
      var $tco_result;
      function $tco_loop(v) {
        if (v instanceof Leaf) {
          $tco_done = true;
          return Nothing.value;
        }
        ;
        if (v instanceof Node) {
          var v1 = compare2(k)(v.value2);
          if (v1 instanceof LT) {
            $copy_v = v.value4;
            return;
          }
          ;
          if (v1 instanceof GT) {
            $copy_v = v.value5;
            return;
          }
          ;
          if (v1 instanceof EQ) {
            $tco_done = true;
            return new Just(v.value3);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 283, column 7 - line 286, column 22): " + [v1.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 280, column 8 - line 286, column 22): " + [v.constructor.name]);
      }
      ;
      while (!$tco_done) {
        $tco_result = $tco_loop($copy_v);
      }
      ;
      return $tco_result;
    };
    return go2;
  };
};
var insert = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go2 = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton6(k)(v);
        }
        ;
        if (v1 instanceof Node) {
          var v2 = compare2(k)(v1.value2);
          if (v2 instanceof LT) {
            return unsafeBalancedNode(v1.value2, v1.value3, go2(v1.value4), v1.value5);
          }
          ;
          if (v2 instanceof GT) {
            return unsafeBalancedNode(v1.value2, v1.value3, v1.value4, go2(v1.value5));
          }
          ;
          if (v2 instanceof EQ) {
            return new Node(v1.value0, v1.value1, k, v, v1.value4, v1.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 471, column 7 - line 474, column 35): " + [v2.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 468, column 8 - line 474, column 35): " + [v1.constructor.name]);
      };
      return go2;
    };
  };
};
var foldableMap = {
  foldr: function(f) {
    return function(z) {
      var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
        return function(m$prime, z$prime) {
          if (m$prime instanceof Leaf) {
            return z$prime;
          }
          ;
          if (m$prime instanceof Node) {
            return $lazy_go(172)(m$prime.value4, f(m$prime.value3)($lazy_go(172)(m$prime.value5, z$prime)));
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 169, column 26 - line 172, column 43): " + [m$prime.constructor.name]);
        };
      });
      var go2 = $lazy_go(169);
      return function(m) {
        return go2(m, z);
      };
    };
  },
  foldl: function(f) {
    return function(z) {
      var $lazy_go = $runtime_lazy3("go", "Data.Map.Internal", function() {
        return function(z$prime, m$prime) {
          if (m$prime instanceof Leaf) {
            return z$prime;
          }
          ;
          if (m$prime instanceof Node) {
            return $lazy_go(178)(f($lazy_go(178)(z$prime, m$prime.value4))(m$prime.value3), m$prime.value5);
          }
          ;
          throw new Error("Failed pattern match at Data.Map.Internal (line 175, column 26 - line 178, column 43): " + [m$prime.constructor.name]);
        };
      });
      var go2 = $lazy_go(175);
      return function(m) {
        return go2(z, m);
      };
    };
  },
  foldMap: function(dictMonoid) {
    var mempty3 = mempty(dictMonoid);
    var append12 = append(dictMonoid.Semigroup0());
    return function(f) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return mempty3;
        }
        ;
        if (v instanceof Node) {
          return append12(go2(v.value4))(append12(f(v.value3))(go2(v.value5)));
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 181, column 10 - line 184, column 28): " + [v.constructor.name]);
      };
      return go2;
    };
  }
};
var empty3 = /* @__PURE__ */ function() {
  return Leaf.value;
}();
var $$delete = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    var go2 = function(v) {
      if (v instanceof Leaf) {
        return Leaf.value;
      }
      ;
      if (v instanceof Node) {
        var v1 = compare2(k)(v.value2);
        if (v1 instanceof LT) {
          return unsafeBalancedNode(v.value2, v.value3, go2(v.value4), v.value5);
        }
        ;
        if (v1 instanceof GT) {
          return unsafeBalancedNode(v.value2, v.value3, v.value4, go2(v.value5));
        }
        ;
        if (v1 instanceof EQ) {
          return unsafeJoinNodes(v.value4, v.value5);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 498, column 7 - line 501, column 43): " + [v1.constructor.name]);
      }
      ;
      throw new Error("Failed pattern match at Data.Map.Internal (line 495, column 8 - line 501, column 43): " + [v.constructor.name]);
    };
    return go2;
  };
};
var alter = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(f) {
    return function(k) {
      return function(m) {
        var v = unsafeSplit(compare2, k, m);
        var v2 = f(v.value0);
        if (v2 instanceof Nothing) {
          return unsafeJoinNodes(v.value1, v.value2);
        }
        ;
        if (v2 instanceof Just) {
          return unsafeBalancedNode(k, v2.value0, v.value1, v.value2);
        }
        ;
        throw new Error("Failed pattern match at Data.Map.Internal (line 514, column 3 - line 518, column 41): " + [v2.constructor.name]);
      };
    };
  };
};

// output/Halogen.Data.Slot/index.js
var foreachSlot = function(dictApplicative) {
  var traverse_7 = traverse_(dictApplicative)(foldableMap);
  return function(v) {
    return function(k) {
      return traverse_7(function($54) {
        return k($54);
      })(v);
    };
  };
};
var empty4 = empty3;

// output/Data.String.Common/foreign.js
var joinWith = function(s) {
  return function(xs) {
    return xs.join(s);
  };
};

// output/Halogen.Query.Input/index.js
var RefUpdate = /* @__PURE__ */ function() {
  function RefUpdate2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  RefUpdate2.create = function(value0) {
    return function(value1) {
      return new RefUpdate2(value0, value1);
    };
  };
  return RefUpdate2;
}();
var Action = /* @__PURE__ */ function() {
  function Action3(value0) {
    this.value0 = value0;
  }
  ;
  Action3.create = function(value0) {
    return new Action3(value0);
  };
  return Action3;
}();

// output/Halogen.VDom.Machine/index.js
var Step = /* @__PURE__ */ function() {
  function Step3(value0, value1, value22, value32) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
    this.value3 = value32;
  }
  ;
  Step3.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return function(value32) {
          return new Step3(value0, value1, value22, value32);
        };
      };
    };
  };
  return Step3;
}();
var unStep = unsafeCoerce2;
var step = function(v, a2) {
  return v.value2(v.value1, a2);
};
var mkStep = unsafeCoerce2;
var halt = function(v) {
  return v.value3(v.value1);
};
var extract2 = /* @__PURE__ */ unStep(function(v) {
  return v.value0;
});

// output/Halogen.VDom.Types/index.js
var map7 = /* @__PURE__ */ map(functorArray);
var map12 = /* @__PURE__ */ map(functorTuple);
var Text = /* @__PURE__ */ function() {
  function Text2(value0) {
    this.value0 = value0;
  }
  ;
  Text2.create = function(value0) {
    return new Text2(value0);
  };
  return Text2;
}();
var Elem = /* @__PURE__ */ function() {
  function Elem2(value0, value1, value22, value32) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
    this.value3 = value32;
  }
  ;
  Elem2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return function(value32) {
          return new Elem2(value0, value1, value22, value32);
        };
      };
    };
  };
  return Elem2;
}();
var Keyed = /* @__PURE__ */ function() {
  function Keyed2(value0, value1, value22, value32) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
    this.value3 = value32;
  }
  ;
  Keyed2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return function(value32) {
          return new Keyed2(value0, value1, value22, value32);
        };
      };
    };
  };
  return Keyed2;
}();
var Widget = /* @__PURE__ */ function() {
  function Widget2(value0) {
    this.value0 = value0;
  }
  ;
  Widget2.create = function(value0) {
    return new Widget2(value0);
  };
  return Widget2;
}();
var Grafted = /* @__PURE__ */ function() {
  function Grafted2(value0) {
    this.value0 = value0;
  }
  ;
  Grafted2.create = function(value0) {
    return new Grafted2(value0);
  };
  return Grafted2;
}();
var Graft = /* @__PURE__ */ function() {
  function Graft2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  Graft2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new Graft2(value0, value1, value22);
      };
    };
  };
  return Graft2;
}();
var unGraft = function(f) {
  return function($61) {
    return f($61);
  };
};
var graft = unsafeCoerce2;
var bifunctorGraft = {
  bimap: function(f) {
    return function(g) {
      return unGraft(function(v) {
        return graft(new Graft(function($63) {
          return f(v.value0($63));
        }, function($64) {
          return g(v.value1($64));
        }, v.value2));
      });
    };
  }
};
var bimap2 = /* @__PURE__ */ bimap(bifunctorGraft);
var runGraft = /* @__PURE__ */ unGraft(function(v) {
  var go2 = function(v2) {
    if (v2 instanceof Text) {
      return new Text(v2.value0);
    }
    ;
    if (v2 instanceof Elem) {
      return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map7(go2)(v2.value3));
    }
    ;
    if (v2 instanceof Keyed) {
      return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map7(map12(go2))(v2.value3));
    }
    ;
    if (v2 instanceof Widget) {
      return new Widget(v.value1(v2.value0));
    }
    ;
    if (v2 instanceof Grafted) {
      return new Grafted(bimap2(v.value0)(v.value1)(v2.value0));
    }
    ;
    throw new Error("Failed pattern match at Halogen.VDom.Types (line 86, column 7 - line 86, column 27): " + [v2.constructor.name]);
  };
  return go2(v.value2);
});

// output/Halogen.VDom.Util/foreign.js
function unsafeGetAny(key, obj) {
  return obj[key];
}
function unsafeHasAny(key, obj) {
  return obj.hasOwnProperty(key);
}
function unsafeSetAny(key, val, obj) {
  obj[key] = val;
}
function forE2(a2, f) {
  var b2 = [];
  for (var i2 = 0; i2 < a2.length; i2++) {
    b2.push(f(i2, a2[i2]));
  }
  return b2;
}
function forEachE(a2, f) {
  for (var i2 = 0; i2 < a2.length; i2++) {
    f(a2[i2]);
  }
}
function forInE(o, f) {
  var ks = Object.keys(o);
  for (var i2 = 0; i2 < ks.length; i2++) {
    var k = ks[i2];
    f(k, o[k]);
  }
}
function diffWithIxE(a1, a2, f1, f2, f3) {
  var a3 = [];
  var l1 = a1.length;
  var l2 = a2.length;
  var i2 = 0;
  while (1) {
    if (i2 < l1) {
      if (i2 < l2) {
        a3.push(f1(i2, a1[i2], a2[i2]));
      } else {
        f2(i2, a1[i2]);
      }
    } else if (i2 < l2) {
      a3.push(f3(i2, a2[i2]));
    } else {
      break;
    }
    i2++;
  }
  return a3;
}
function strMapWithIxE(as, fk, f) {
  var o = {};
  for (var i2 = 0; i2 < as.length; i2++) {
    var a2 = as[i2];
    var k = fk(a2);
    o[k] = f(k, i2, a2);
  }
  return o;
}
function diffWithKeyAndIxE(o1, as, fk, f1, f2, f3) {
  var o2 = {};
  for (var i2 = 0; i2 < as.length; i2++) {
    var a2 = as[i2];
    var k = fk(a2);
    if (o1.hasOwnProperty(k)) {
      o2[k] = f1(k, i2, o1[k], a2);
    } else {
      o2[k] = f3(k, i2, a2);
    }
  }
  for (var k in o1) {
    if (k in o2) {
      continue;
    }
    f2(k, o1[k]);
  }
  return o2;
}
function refEq2(a2, b2) {
  return a2 === b2;
}
function createTextNode(s, doc) {
  return doc.createTextNode(s);
}
function setTextContent(s, n) {
  n.textContent = s;
}
function createElement(ns, name15, doc) {
  if (ns != null) {
    return doc.createElementNS(ns, name15);
  } else {
    return doc.createElement(name15);
  }
}
function insertChildIx(i2, a2, b2) {
  var n = b2.childNodes.item(i2) || null;
  if (n !== a2) {
    b2.insertBefore(a2, n);
  }
}
function removeChild(a2, b2) {
  if (b2 && a2.parentNode === b2) {
    b2.removeChild(a2);
  }
}
function parentNode(a2) {
  return a2.parentNode;
}
function setAttribute(ns, attr3, val, el) {
  if (ns != null) {
    el.setAttributeNS(ns, attr3, val);
  } else {
    el.setAttribute(attr3, val);
  }
}
function removeAttribute(ns, attr3, el) {
  if (ns != null) {
    el.removeAttributeNS(ns, attr3);
  } else {
    el.removeAttribute(attr3);
  }
}
function hasAttribute(ns, attr3, el) {
  if (ns != null) {
    return el.hasAttributeNS(ns, attr3);
  } else {
    return el.hasAttribute(attr3);
  }
}
function addEventListener(ev, listener, el) {
  el.addEventListener(ev, listener, false);
}
function removeEventListener(ev, listener, el) {
  el.removeEventListener(ev, listener, false);
}
var jsUndefined = void 0;

// output/Halogen.VDom.Util/index.js
var unsafeLookup = unsafeGetAny;
var unsafeFreeze2 = unsafeCoerce2;
var pokeMutMap = unsafeSetAny;
var newMutMap = newImpl;

// output/Web.DOM.Element/foreign.js
var getProp = function(name15) {
  return function(doctype) {
    return doctype[name15];
  };
};
var _namespaceURI = getProp("namespaceURI");
var _prefix = getProp("prefix");
var localName = getProp("localName");
var tagName = getProp("tagName");

// output/Web.DOM.ParentNode/foreign.js
var getEffProp = function(name15) {
  return function(node) {
    return function() {
      return node[name15];
    };
  };
};
var children = getEffProp("children");
var _firstElementChild = getEffProp("firstElementChild");
var _lastElementChild = getEffProp("lastElementChild");
var childElementCount = getEffProp("childElementCount");
function _querySelector(selector) {
  return function(node) {
    return function() {
      return node.querySelector(selector);
    };
  };
}

// output/Web.DOM.ParentNode/index.js
var map8 = /* @__PURE__ */ map(functorEffect);
var querySelector = function(qs) {
  var $2 = map8(toMaybe);
  var $3 = _querySelector(qs);
  return function($4) {
    return $2($3($4));
  };
};

// output/Web.DOM.Element/index.js
var toNode = unsafeCoerce2;

// output/Halogen.VDom.DOM/index.js
var $runtime_lazy4 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var haltWidget = function(v) {
  return halt(v.widget);
};
var $lazy_patchWidget = /* @__PURE__ */ $runtime_lazy4("patchWidget", "Halogen.VDom.DOM", function() {
  return function(state3, vdom) {
    if (vdom instanceof Grafted) {
      return $lazy_patchWidget(291)(state3, runGraft(vdom.value0));
    }
    ;
    if (vdom instanceof Widget) {
      var res = step(state3.widget, vdom.value0);
      var res$prime = unStep(function(v) {
        return mkStep(new Step(v.value0, {
          build: state3.build,
          widget: res
        }, $lazy_patchWidget(296), haltWidget));
      })(res);
      return res$prime;
    }
    ;
    haltWidget(state3);
    return state3.build(vdom);
  };
});
var patchWidget = /* @__PURE__ */ $lazy_patchWidget(286);
var haltText = function(v) {
  var parent2 = parentNode(v.node);
  return removeChild(v.node, parent2);
};
var $lazy_patchText = /* @__PURE__ */ $runtime_lazy4("patchText", "Halogen.VDom.DOM", function() {
  return function(state3, vdom) {
    if (vdom instanceof Grafted) {
      return $lazy_patchText(82)(state3, runGraft(vdom.value0));
    }
    ;
    if (vdom instanceof Text) {
      if (state3.value === vdom.value0) {
        return mkStep(new Step(state3.node, state3, $lazy_patchText(85), haltText));
      }
      ;
      if (otherwise) {
        var nextState = {
          build: state3.build,
          node: state3.node,
          value: vdom.value0
        };
        setTextContent(vdom.value0, state3.node);
        return mkStep(new Step(state3.node, nextState, $lazy_patchText(89), haltText));
      }
      ;
    }
    ;
    haltText(state3);
    return state3.build(vdom);
  };
});
var patchText = /* @__PURE__ */ $lazy_patchText(77);
var haltKeyed = function(v) {
  var parent2 = parentNode(v.node);
  removeChild(v.node, parent2);
  forInE(v.children, function(v1, s) {
    return halt(s);
  });
  return halt(v.attrs);
};
var haltElem = function(v) {
  var parent2 = parentNode(v.node);
  removeChild(v.node, parent2);
  forEachE(v.children, halt);
  return halt(v.attrs);
};
var eqElemSpec = function(ns1, v, ns2, v1) {
  var $63 = v === v1;
  if ($63) {
    if (ns1 instanceof Just && (ns2 instanceof Just && ns1.value0 === ns2.value0)) {
      return true;
    }
    ;
    if (ns1 instanceof Nothing && ns2 instanceof Nothing) {
      return true;
    }
    ;
    return false;
  }
  ;
  return false;
};
var $lazy_patchElem = /* @__PURE__ */ $runtime_lazy4("patchElem", "Halogen.VDom.DOM", function() {
  return function(state3, vdom) {
    if (vdom instanceof Grafted) {
      return $lazy_patchElem(135)(state3, runGraft(vdom.value0));
    }
    ;
    if (vdom instanceof Elem && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
      var v = length3(vdom.value3);
      var v1 = length3(state3.children);
      if (v1 === 0 && v === 0) {
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state3.children
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchElem(149), haltElem));
      }
      ;
      var onThis = function(v2, s) {
        return halt(s);
      };
      var onThese = function(ix, s, v2) {
        var res = step(s, v2);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var onThat = function(ix, v2) {
        var res = state3.build(v2);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
      var attrs2 = step(state3.attrs, vdom.value2);
      var nextState = {
        build: state3.build,
        node: state3.node,
        attrs: attrs2,
        ns: vdom.value0,
        name: vdom.value1,
        children: children2
      };
      return mkStep(new Step(state3.node, nextState, $lazy_patchElem(172), haltElem));
    }
    ;
    haltElem(state3);
    return state3.build(vdom);
  };
});
var patchElem = /* @__PURE__ */ $lazy_patchElem(130);
var $lazy_patchKeyed = /* @__PURE__ */ $runtime_lazy4("patchKeyed", "Halogen.VDom.DOM", function() {
  return function(state3, vdom) {
    if (vdom instanceof Grafted) {
      return $lazy_patchKeyed(222)(state3, runGraft(vdom.value0));
    }
    ;
    if (vdom instanceof Keyed && eqElemSpec(state3.ns, state3.name, vdom.value0, vdom.value1)) {
      var v = length3(vdom.value3);
      if (state3.length === 0 && v === 0) {
        var attrs2 = step(state3.attrs, vdom.value2);
        var nextState = {
          build: state3.build,
          node: state3.node,
          attrs: attrs2,
          ns: vdom.value0,
          name: vdom.value1,
          children: state3.children,
          length: 0
        };
        return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(237), haltKeyed));
      }
      ;
      var onThis = function(v2, s) {
        return halt(s);
      };
      var onThese = function(v2, ix$prime, s, v3) {
        var res = step(s, v3.value1);
        insertChildIx(ix$prime, extract2(res), state3.node);
        return res;
      };
      var onThat = function(v2, ix, v3) {
        var res = state3.build(v3.value1);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
      var attrs2 = step(state3.attrs, vdom.value2);
      var nextState = {
        build: state3.build,
        node: state3.node,
        attrs: attrs2,
        ns: vdom.value0,
        name: vdom.value1,
        children: children2,
        length: v
      };
      return mkStep(new Step(state3.node, nextState, $lazy_patchKeyed(261), haltKeyed));
    }
    ;
    haltKeyed(state3);
    return state3.build(vdom);
  };
});
var patchKeyed = /* @__PURE__ */ $lazy_patchKeyed(217);
var buildWidget = function(v, build, w) {
  var res = v.buildWidget(v)(w);
  var res$prime = unStep(function(v1) {
    return mkStep(new Step(v1.value0, {
      build,
      widget: res
    }, patchWidget, haltWidget));
  })(res);
  return res$prime;
};
var buildText = function(v, build, s) {
  var node = createTextNode(s, v.document);
  var state3 = {
    build,
    node,
    value: s
  };
  return mkStep(new Step(node, state3, patchText, haltText));
};
var buildKeyed = function(v, build, ns1, name1, as1, ch1) {
  var el = createElement(toNullable(ns1), name1, v.document);
  var node = toNode(el);
  var onChild = function(v1, ix, v2) {
    var res = build(v2.value1);
    insertChildIx(ix, extract2(res), node);
    return res;
  };
  var children2 = strMapWithIxE(ch1, fst, onChild);
  var attrs = v.buildAttributes(el)(as1);
  var state3 = {
    build,
    node,
    attrs,
    ns: ns1,
    name: name1,
    children: children2,
    length: length3(ch1)
  };
  return mkStep(new Step(node, state3, patchKeyed, haltKeyed));
};
var buildElem = function(v, build, ns1, name1, as1, ch1) {
  var el = createElement(toNullable(ns1), name1, v.document);
  var node = toNode(el);
  var onChild = function(ix, child) {
    var res = build(child);
    insertChildIx(ix, extract2(res), node);
    return res;
  };
  var children2 = forE2(ch1, onChild);
  var attrs = v.buildAttributes(el)(as1);
  var state3 = {
    build,
    node,
    attrs,
    ns: ns1,
    name: name1,
    children: children2
  };
  return mkStep(new Step(node, state3, patchElem, haltElem));
};
var buildVDom = function(spec) {
  var $lazy_build = $runtime_lazy4("build", "Halogen.VDom.DOM", function() {
    return function(v) {
      if (v instanceof Text) {
        return buildText(spec, $lazy_build(59), v.value0);
      }
      ;
      if (v instanceof Elem) {
        return buildElem(spec, $lazy_build(60), v.value0, v.value1, v.value2, v.value3);
      }
      ;
      if (v instanceof Keyed) {
        return buildKeyed(spec, $lazy_build(61), v.value0, v.value1, v.value2, v.value3);
      }
      ;
      if (v instanceof Widget) {
        return buildWidget(spec, $lazy_build(62), v.value0);
      }
      ;
      if (v instanceof Grafted) {
        return $lazy_build(63)(runGraft(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Halogen.VDom.DOM (line 58, column 27 - line 63, column 52): " + [v.constructor.name]);
    };
  });
  var build = $lazy_build(58);
  return build;
};

// output/Web.Event.EventTarget/foreign.js
function eventListener(fn) {
  return function() {
    return function(event) {
      return fn(event)();
    };
  };
}
function addEventListener2(type) {
  return function(listener) {
    return function(useCapture) {
      return function(target6) {
        return function() {
          return target6.addEventListener(type, listener, useCapture);
        };
      };
    };
  };
}
function removeEventListener2(type) {
  return function(listener) {
    return function(useCapture) {
      return function(target6) {
        return function() {
          return target6.removeEventListener(type, listener, useCapture);
        };
      };
    };
  };
}

// output/Halogen.VDom.DOM.Prop/index.js
var $runtime_lazy5 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var Created = /* @__PURE__ */ function() {
  function Created2(value0) {
    this.value0 = value0;
  }
  ;
  Created2.create = function(value0) {
    return new Created2(value0);
  };
  return Created2;
}();
var Removed = /* @__PURE__ */ function() {
  function Removed2(value0) {
    this.value0 = value0;
  }
  ;
  Removed2.create = function(value0) {
    return new Removed2(value0);
  };
  return Removed2;
}();
var Attribute = /* @__PURE__ */ function() {
  function Attribute2(value0, value1, value22) {
    this.value0 = value0;
    this.value1 = value1;
    this.value2 = value22;
  }
  ;
  Attribute2.create = function(value0) {
    return function(value1) {
      return function(value22) {
        return new Attribute2(value0, value1, value22);
      };
    };
  };
  return Attribute2;
}();
var Property = /* @__PURE__ */ function() {
  function Property2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Property2.create = function(value0) {
    return function(value1) {
      return new Property2(value0, value1);
    };
  };
  return Property2;
}();
var Handler = /* @__PURE__ */ function() {
  function Handler2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Handler2.create = function(value0) {
    return function(value1) {
      return new Handler2(value0, value1);
    };
  };
  return Handler2;
}();
var Ref = /* @__PURE__ */ function() {
  function Ref2(value0) {
    this.value0 = value0;
  }
  ;
  Ref2.create = function(value0) {
    return new Ref2(value0);
  };
  return Ref2;
}();
var unsafeGetProperty = unsafeGetAny;
var setProperty = unsafeSetAny;
var removeProperty = function(key, el) {
  var v = hasAttribute(nullImpl, key, el);
  if (v) {
    return removeAttribute(nullImpl, key, el);
  }
  ;
  var v1 = typeOf(unsafeGetAny(key, el));
  if (v1 === "string") {
    return unsafeSetAny(key, "", el);
  }
  ;
  if (key === "rowSpan") {
    return unsafeSetAny(key, 1, el);
  }
  ;
  if (key === "colSpan") {
    return unsafeSetAny(key, 1, el);
  }
  ;
  return unsafeSetAny(key, jsUndefined, el);
};
var propToStrKey = function(v) {
  if (v instanceof Attribute && v.value0 instanceof Just) {
    return "attr/" + (v.value0.value0 + (":" + v.value1));
  }
  ;
  if (v instanceof Attribute) {
    return "attr/:" + v.value1;
  }
  ;
  if (v instanceof Property) {
    return "prop/" + v.value0;
  }
  ;
  if (v instanceof Handler) {
    return "handler/" + v.value0;
  }
  ;
  if (v instanceof Ref) {
    return "ref";
  }
  ;
  throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 182, column 16 - line 187, column 16): " + [v.constructor.name]);
};
var propFromString = unsafeCoerce2;
var propFromInt = unsafeCoerce2;
var buildProp = function(emit) {
  return function(el) {
    var removeProp = function(prevEvents) {
      return function(v, v1) {
        if (v1 instanceof Attribute) {
          return removeAttribute(toNullable(v1.value0), v1.value1, el);
        }
        ;
        if (v1 instanceof Property) {
          return removeProperty(v1.value0, el);
        }
        ;
        if (v1 instanceof Handler) {
          var handler3 = unsafeLookup(v1.value0, prevEvents);
          return removeEventListener(v1.value0, fst(handler3), el);
        }
        ;
        if (v1 instanceof Ref) {
          return unit;
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 169, column 5 - line 179, column 18): " + [v1.constructor.name]);
      };
    };
    var mbEmit = function(v) {
      if (v instanceof Just) {
        return emit(v.value0)();
      }
      ;
      return unit;
    };
    var haltProp = function(state3) {
      var v = lookup("ref")(state3.props);
      if (v instanceof Just && v.value0 instanceof Ref) {
        return mbEmit(v.value0.value0(new Removed(el)));
      }
      ;
      return unit;
    };
    var diffProp = function(prevEvents, events) {
      return function(v, v1, v11, v2) {
        if (v11 instanceof Attribute && v2 instanceof Attribute) {
          var $66 = v11.value2 === v2.value2;
          if ($66) {
            return v2;
          }
          ;
          setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
          return v2;
        }
        ;
        if (v11 instanceof Property && v2 instanceof Property) {
          var v4 = refEq2(v11.value1, v2.value1);
          if (v4) {
            return v2;
          }
          ;
          if (v2.value0 === "value") {
            var elVal = unsafeGetProperty("value", el);
            var $75 = refEq2(elVal, v2.value1);
            if ($75) {
              return v2;
            }
            ;
            setProperty(v2.value0, v2.value1, el);
            return v2;
          }
          ;
          setProperty(v2.value0, v2.value1, el);
          return v2;
        }
        ;
        if (v11 instanceof Handler && v2 instanceof Handler) {
          var handler3 = unsafeLookup(v2.value0, prevEvents);
          write(v2.value1)(snd(handler3))();
          pokeMutMap(v2.value0, handler3, events);
          return v2;
        }
        ;
        return v2;
      };
    };
    var applyProp = function(events) {
      return function(v, v1, v2) {
        if (v2 instanceof Attribute) {
          setAttribute(toNullable(v2.value0), v2.value1, v2.value2, el);
          return v2;
        }
        ;
        if (v2 instanceof Property) {
          setProperty(v2.value0, v2.value1, el);
          return v2;
        }
        ;
        if (v2 instanceof Handler) {
          var v3 = unsafeGetAny(v2.value0, events);
          if (unsafeHasAny(v2.value0, events)) {
            write(v2.value1)(snd(v3))();
            return v2;
          }
          ;
          var ref2 = $$new(v2.value1)();
          var listener = eventListener(function(ev) {
            return function __do4() {
              var f$prime = read(ref2)();
              return mbEmit(f$prime(ev));
            };
          })();
          pokeMutMap(v2.value0, new Tuple(listener, ref2), events);
          addEventListener(v2.value0, listener, el);
          return v2;
        }
        ;
        if (v2 instanceof Ref) {
          mbEmit(v2.value0(new Created(el)));
          return v2;
        }
        ;
        throw new Error("Failed pattern match at Halogen.VDom.DOM.Prop (line 113, column 5 - line 135, column 15): " + [v2.constructor.name]);
      };
    };
    var $lazy_patchProp = $runtime_lazy5("patchProp", "Halogen.VDom.DOM.Prop", function() {
      return function(state3, ps2) {
        var events = newMutMap();
        var onThis = removeProp(state3.events);
        var onThese = diffProp(state3.events, events);
        var onThat = applyProp(events);
        var props = diffWithKeyAndIxE(state3.props, ps2, propToStrKey, onThese, onThis, onThat);
        var nextState = {
          events: unsafeFreeze2(events),
          props
        };
        return mkStep(new Step(unit, nextState, $lazy_patchProp(100), haltProp));
      };
    });
    var patchProp = $lazy_patchProp(87);
    var renderProp = function(ps1) {
      var events = newMutMap();
      var ps1$prime = strMapWithIxE(ps1, propToStrKey, applyProp(events));
      var state3 = {
        events: unsafeFreeze2(events),
        props: ps1$prime
      };
      return mkStep(new Step(unit, state3, patchProp, haltProp));
    };
    return renderProp;
  };
};

// output/Halogen.HTML.Core/index.js
var HTML = function(x) {
  return x;
};
var toPropValue = function(dict) {
  return dict.toPropValue;
};
var text = function($29) {
  return HTML(Text.create($29));
};
var prop = function(dictIsProp) {
  var toPropValue1 = toPropValue(dictIsProp);
  return function(v) {
    var $31 = Property.create(v);
    return function($32) {
      return $31(toPropValue1($32));
    };
  };
};
var isPropString = {
  toPropValue: propFromString
};
var isPropInt = {
  toPropValue: propFromInt
};
var handler = /* @__PURE__ */ function() {
  return Handler.create;
}();
var element = function(ns) {
  return function(name15) {
    return function(props) {
      return function(children2) {
        return new Elem(ns, name15, props, children2);
      };
    };
  };
};
var attr = function(ns) {
  return function(v) {
    return Attribute.create(ns)(v);
  };
};

// output/Control.Applicative.Free/index.js
var identity7 = /* @__PURE__ */ identity(categoryFn);
var Pure = /* @__PURE__ */ function() {
  function Pure2(value0) {
    this.value0 = value0;
  }
  ;
  Pure2.create = function(value0) {
    return new Pure2(value0);
  };
  return Pure2;
}();
var Lift = /* @__PURE__ */ function() {
  function Lift3(value0) {
    this.value0 = value0;
  }
  ;
  Lift3.create = function(value0) {
    return new Lift3(value0);
  };
  return Lift3;
}();
var Ap = /* @__PURE__ */ function() {
  function Ap2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Ap2.create = function(value0) {
    return function(value1) {
      return new Ap2(value0, value1);
    };
  };
  return Ap2;
}();
var mkAp = function(fba) {
  return function(fb) {
    return new Ap(fba, fb);
  };
};
var liftFreeAp = /* @__PURE__ */ function() {
  return Lift.create;
}();
var goLeft = function(dictApplicative) {
  var pure9 = pure(dictApplicative);
  return function(fStack) {
    return function(valStack) {
      return function(nat) {
        return function(func) {
          return function(count) {
            if (func instanceof Pure) {
              return new Tuple(new Cons({
                func: pure9(func.value0),
                count
              }, fStack), valStack);
            }
            ;
            if (func instanceof Lift) {
              return new Tuple(new Cons({
                func: nat(func.value0),
                count
              }, fStack), valStack);
            }
            ;
            if (func instanceof Ap) {
              return goLeft(dictApplicative)(fStack)(cons(func.value1)(valStack))(nat)(func.value0)(count + 1 | 0);
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 102, column 41 - line 105, column 81): " + [func.constructor.name]);
          };
        };
      };
    };
  };
};
var goApply = function(dictApplicative) {
  var apply3 = apply(dictApplicative.Apply0());
  return function(fStack) {
    return function(vals) {
      return function(gVal) {
        if (fStack instanceof Nil) {
          return new Left(gVal);
        }
        ;
        if (fStack instanceof Cons) {
          var gRes = apply3(fStack.value0.func)(gVal);
          var $31 = fStack.value0.count === 1;
          if ($31) {
            if (fStack.value1 instanceof Nil) {
              return new Left(gRes);
            }
            ;
            return goApply(dictApplicative)(fStack.value1)(vals)(gRes);
          }
          ;
          if (vals instanceof Nil) {
            return new Left(gRes);
          }
          ;
          if (vals instanceof Cons) {
            return new Right(new Tuple(new Cons({
              func: gRes,
              count: fStack.value0.count - 1 | 0
            }, fStack.value1), new NonEmpty(vals.value0, vals.value1)));
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 83, column 11 - line 88, column 50): " + [vals.constructor.name]);
        }
        ;
        throw new Error("Failed pattern match at Control.Applicative.Free (line 72, column 3 - line 88, column 50): " + [fStack.constructor.name]);
      };
    };
  };
};
var functorFreeAp = {
  map: function(f) {
    return function(x) {
      return mkAp(new Pure(f))(x);
    };
  }
};
var foldFreeAp = function(dictApplicative) {
  var goApply1 = goApply(dictApplicative);
  var pure9 = pure(dictApplicative);
  var goLeft1 = goLeft(dictApplicative);
  return function(nat) {
    return function(z) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v.value1.value0 instanceof Pure) {
            var v1 = goApply1(v.value0)(v.value1.value1)(pure9(v.value1.value0.value0));
            if (v1 instanceof Left) {
              $tco_done = true;
              return v1.value0;
            }
            ;
            if (v1 instanceof Right) {
              $copy_v = v1.value0;
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 54, column 17 - line 56, column 24): " + [v1.constructor.name]);
          }
          ;
          if (v.value1.value0 instanceof Lift) {
            var v1 = goApply1(v.value0)(v.value1.value1)(nat(v.value1.value0.value0));
            if (v1 instanceof Left) {
              $tco_done = true;
              return v1.value0;
            }
            ;
            if (v1 instanceof Right) {
              $copy_v = v1.value0;
              return;
            }
            ;
            throw new Error("Failed pattern match at Control.Applicative.Free (line 57, column 17 - line 59, column 24): " + [v1.constructor.name]);
          }
          ;
          if (v.value1.value0 instanceof Ap) {
            var nextVals = new NonEmpty(v.value1.value0.value1, v.value1.value1);
            $copy_v = goLeft1(v.value0)(nextVals)(nat)(v.value1.value0.value0)(1);
            return;
          }
          ;
          throw new Error("Failed pattern match at Control.Applicative.Free (line 53, column 5 - line 62, column 47): " + [v.value1.value0.constructor.name]);
        }
        ;
        while (!$tco_done) {
          $tco_result = $tco_loop($copy_v);
        }
        ;
        return $tco_result;
      };
      return go2(new Tuple(Nil.value, singleton3(z)));
    };
  };
};
var retractFreeAp = function(dictApplicative) {
  return foldFreeAp(dictApplicative)(identity7);
};
var applyFreeAp = {
  apply: function(fba) {
    return function(fb) {
      return mkAp(fba)(fb);
    };
  },
  Functor0: function() {
    return functorFreeAp;
  }
};
var applicativeFreeAp = /* @__PURE__ */ function() {
  return {
    pure: Pure.create,
    Apply0: function() {
      return applyFreeAp;
    }
  };
}();
var foldFreeAp1 = /* @__PURE__ */ foldFreeAp(applicativeFreeAp);
var hoistFreeAp = function(f) {
  return foldFreeAp1(function($54) {
    return liftFreeAp(f($54));
  });
};

// output/Data.CatQueue/index.js
var CatQueue = /* @__PURE__ */ function() {
  function CatQueue2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  CatQueue2.create = function(value0) {
    return function(value1) {
      return new CatQueue2(value0, value1);
    };
  };
  return CatQueue2;
}();
var uncons2 = function($copy_v) {
  var $tco_done = false;
  var $tco_result;
  function $tco_loop(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      $tco_done = true;
      return Nothing.value;
    }
    ;
    if (v.value0 instanceof Nil) {
      $copy_v = new CatQueue(reverse(v.value1), Nil.value);
      return;
    }
    ;
    if (v.value0 instanceof Cons) {
      $tco_done = true;
      return new Just(new Tuple(v.value0.value0, new CatQueue(v.value0.value1, v.value1)));
    }
    ;
    throw new Error("Failed pattern match at Data.CatQueue (line 82, column 1 - line 82, column 63): " + [v.constructor.name]);
  }
  ;
  while (!$tco_done) {
    $tco_result = $tco_loop($copy_v);
  }
  ;
  return $tco_result;
};
var snoc2 = function(v) {
  return function(a2) {
    return new CatQueue(v.value0, new Cons(a2, v.value1));
  };
};
var $$null2 = function(v) {
  if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
    return true;
  }
  ;
  return false;
};
var empty5 = /* @__PURE__ */ function() {
  return new CatQueue(Nil.value, Nil.value);
}();

// output/Data.CatList/index.js
var CatNil = /* @__PURE__ */ function() {
  function CatNil2() {
  }
  ;
  CatNil2.value = new CatNil2();
  return CatNil2;
}();
var CatCons = /* @__PURE__ */ function() {
  function CatCons2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  CatCons2.create = function(value0) {
    return function(value1) {
      return new CatCons2(value0, value1);
    };
  };
  return CatCons2;
}();
var link = function(v) {
  return function(v1) {
    if (v instanceof CatNil) {
      return v1;
    }
    ;
    if (v1 instanceof CatNil) {
      return v;
    }
    ;
    if (v instanceof CatCons) {
      return new CatCons(v.value0, snoc2(v.value1)(v1));
    }
    ;
    throw new Error("Failed pattern match at Data.CatList (line 108, column 1 - line 108, column 54): " + [v.constructor.name, v1.constructor.name]);
  };
};
var foldr3 = function(k) {
  return function(b2) {
    return function(q2) {
      var foldl2 = function($copy_v) {
        return function($copy_v1) {
          return function($copy_v2) {
            var $tco_var_v = $copy_v;
            var $tco_var_v1 = $copy_v1;
            var $tco_done = false;
            var $tco_result;
            function $tco_loop(v, v1, v2) {
              if (v2 instanceof Nil) {
                $tco_done = true;
                return v1;
              }
              ;
              if (v2 instanceof Cons) {
                $tco_var_v = v;
                $tco_var_v1 = v(v1)(v2.value0);
                $copy_v2 = v2.value1;
                return;
              }
              ;
              throw new Error("Failed pattern match at Data.CatList (line 124, column 3 - line 124, column 59): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
            }
            ;
            while (!$tco_done) {
              $tco_result = $tco_loop($tco_var_v, $tco_var_v1, $copy_v2);
            }
            ;
            return $tco_result;
          };
        };
      };
      var go2 = function($copy_xs) {
        return function($copy_ys) {
          var $tco_var_xs = $copy_xs;
          var $tco_done1 = false;
          var $tco_result;
          function $tco_loop(xs, ys) {
            var v = uncons2(xs);
            if (v instanceof Nothing) {
              $tco_done1 = true;
              return foldl2(function(x) {
                return function(i2) {
                  return i2(x);
                };
              })(b2)(ys);
            }
            ;
            if (v instanceof Just) {
              $tco_var_xs = v.value0.value1;
              $copy_ys = new Cons(k(v.value0.value0), ys);
              return;
            }
            ;
            throw new Error("Failed pattern match at Data.CatList (line 120, column 14 - line 122, column 67): " + [v.constructor.name]);
          }
          ;
          while (!$tco_done1) {
            $tco_result = $tco_loop($tco_var_xs, $copy_ys);
          }
          ;
          return $tco_result;
        };
      };
      return go2(q2)(Nil.value);
    };
  };
};
var uncons3 = function(v) {
  if (v instanceof CatNil) {
    return Nothing.value;
  }
  ;
  if (v instanceof CatCons) {
    return new Just(new Tuple(v.value0, function() {
      var $66 = $$null2(v.value1);
      if ($66) {
        return CatNil.value;
      }
      ;
      return foldr3(link)(CatNil.value)(v.value1);
    }()));
  }
  ;
  throw new Error("Failed pattern match at Data.CatList (line 99, column 1 - line 99, column 61): " + [v.constructor.name]);
};
var empty6 = /* @__PURE__ */ function() {
  return CatNil.value;
}();
var append2 = link;
var semigroupCatList = {
  append: append2
};
var snoc3 = function(cat) {
  return function(a2) {
    return append2(cat)(new CatCons(a2, empty5));
  };
};

// output/Control.Monad.Free/index.js
var $runtime_lazy6 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var append3 = /* @__PURE__ */ append(semigroupCatList);
var Free = /* @__PURE__ */ function() {
  function Free2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Free2.create = function(value0) {
    return function(value1) {
      return new Free2(value0, value1);
    };
  };
  return Free2;
}();
var Return = /* @__PURE__ */ function() {
  function Return2(value0) {
    this.value0 = value0;
  }
  ;
  Return2.create = function(value0) {
    return new Return2(value0);
  };
  return Return2;
}();
var Bind = /* @__PURE__ */ function() {
  function Bind2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Bind2.create = function(value0) {
    return function(value1) {
      return new Bind2(value0, value1);
    };
  };
  return Bind2;
}();
var toView = function($copy_v) {
  var $tco_done = false;
  var $tco_result;
  function $tco_loop(v) {
    var runExpF = function(v22) {
      return v22;
    };
    var concatF = function(v22) {
      return function(r) {
        return new Free(v22.value0, append3(v22.value1)(r));
      };
    };
    if (v.value0 instanceof Return) {
      var v2 = uncons3(v.value1);
      if (v2 instanceof Nothing) {
        $tco_done = true;
        return new Return(v.value0.value0);
      }
      ;
      if (v2 instanceof Just) {
        $copy_v = concatF(runExpF(v2.value0.value0)(v.value0.value0))(v2.value0.value1);
        return;
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 227, column 7 - line 231, column 64): " + [v2.constructor.name]);
    }
    ;
    if (v.value0 instanceof Bind) {
      $tco_done = true;
      return new Bind(v.value0.value0, function(a2) {
        return concatF(v.value0.value1(a2))(v.value1);
      });
    }
    ;
    throw new Error("Failed pattern match at Control.Monad.Free (line 225, column 3 - line 233, column 56): " + [v.value0.constructor.name]);
  }
  ;
  while (!$tco_done) {
    $tco_result = $tco_loop($copy_v);
  }
  ;
  return $tco_result;
};
var fromView = function(f) {
  return new Free(f, empty6);
};
var freeMonad = {
  Applicative0: function() {
    return freeApplicative;
  },
  Bind1: function() {
    return freeBind;
  }
};
var freeFunctor = {
  map: function(k) {
    return function(f) {
      return bindFlipped(freeBind)(function() {
        var $189 = pure(freeApplicative);
        return function($190) {
          return $189(k($190));
        };
      }())(f);
    };
  }
};
var freeBind = {
  bind: function(v) {
    return function(k) {
      return new Free(v.value0, snoc3(v.value1)(k));
    };
  },
  Apply0: function() {
    return $lazy_freeApply(0);
  }
};
var freeApplicative = {
  pure: function($191) {
    return fromView(Return.create($191));
  },
  Apply0: function() {
    return $lazy_freeApply(0);
  }
};
var $lazy_freeApply = /* @__PURE__ */ $runtime_lazy6("freeApply", "Control.Monad.Free", function() {
  return {
    apply: ap(freeMonad),
    Functor0: function() {
      return freeFunctor;
    }
  };
});
var pure3 = /* @__PURE__ */ pure(freeApplicative);
var liftF = function(f) {
  return fromView(new Bind(f, function($192) {
    return pure3($192);
  }));
};
var foldFree = function(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var map110 = map(Monad0.Bind1().Apply0().Functor0());
  var pure13 = pure(Monad0.Applicative0());
  var tailRecM4 = tailRecM(dictMonadRec);
  return function(k) {
    var go2 = function(f) {
      var v = toView(f);
      if (v instanceof Return) {
        return map110(Done.create)(pure13(v.value0));
      }
      ;
      if (v instanceof Bind) {
        return map110(function($199) {
          return Loop.create(v.value1($199));
        })(k(v.value0));
      }
      ;
      throw new Error("Failed pattern match at Control.Monad.Free (line 158, column 10 - line 160, column 37): " + [v.constructor.name]);
    };
    return tailRecM4(go2);
  };
};

// output/Halogen.Query.ChildQuery/index.js
var unChildQueryBox = unsafeCoerce2;

// output/Unsafe.Reference/foreign.js
function reallyUnsafeRefEq(a2) {
  return function(b2) {
    return a2 === b2;
  };
}

// output/Unsafe.Reference/index.js
var unsafeRefEq = reallyUnsafeRefEq;

// output/Halogen.Subscription/index.js
var $$void4 = /* @__PURE__ */ $$void(functorEffect);
var bind3 = /* @__PURE__ */ bind(bindEffect);
var append4 = /* @__PURE__ */ append(semigroupArray);
var traverse_2 = /* @__PURE__ */ traverse_(applicativeEffect);
var traverse_1 = /* @__PURE__ */ traverse_2(foldableArray);
var unsubscribe = function(v) {
  return v;
};
var subscribe = function(v) {
  return function(k) {
    return v(function($76) {
      return $$void4(k($76));
    });
  };
};
var notify = function(v) {
  return function(a2) {
    return v(a2);
  };
};
var create = function __do2() {
  var subscribers = $$new([])();
  return {
    emitter: function(k) {
      return function __do4() {
        modify_(function(v) {
          return append4(v)([k]);
        })(subscribers)();
        return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
      };
    },
    listener: function(a2) {
      return bind3(read(subscribers))(traverse_1(function(k) {
        return k(a2);
      }));
    }
  };
};

// output/Halogen.Query.HalogenM/index.js
var SubscriptionId = function(x) {
  return x;
};
var ForkId = function(x) {
  return x;
};
var State = /* @__PURE__ */ function() {
  function State2(value0) {
    this.value0 = value0;
  }
  ;
  State2.create = function(value0) {
    return new State2(value0);
  };
  return State2;
}();
var Subscribe = /* @__PURE__ */ function() {
  function Subscribe2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Subscribe2.create = function(value0) {
    return function(value1) {
      return new Subscribe2(value0, value1);
    };
  };
  return Subscribe2;
}();
var Unsubscribe = /* @__PURE__ */ function() {
  function Unsubscribe2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Unsubscribe2.create = function(value0) {
    return function(value1) {
      return new Unsubscribe2(value0, value1);
    };
  };
  return Unsubscribe2;
}();
var Lift2 = /* @__PURE__ */ function() {
  function Lift3(value0) {
    this.value0 = value0;
  }
  ;
  Lift3.create = function(value0) {
    return new Lift3(value0);
  };
  return Lift3;
}();
var ChildQuery2 = /* @__PURE__ */ function() {
  function ChildQuery3(value0) {
    this.value0 = value0;
  }
  ;
  ChildQuery3.create = function(value0) {
    return new ChildQuery3(value0);
  };
  return ChildQuery3;
}();
var Raise = /* @__PURE__ */ function() {
  function Raise2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Raise2.create = function(value0) {
    return function(value1) {
      return new Raise2(value0, value1);
    };
  };
  return Raise2;
}();
var Par = /* @__PURE__ */ function() {
  function Par2(value0) {
    this.value0 = value0;
  }
  ;
  Par2.create = function(value0) {
    return new Par2(value0);
  };
  return Par2;
}();
var Fork = /* @__PURE__ */ function() {
  function Fork2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Fork2.create = function(value0) {
    return function(value1) {
      return new Fork2(value0, value1);
    };
  };
  return Fork2;
}();
var Join = /* @__PURE__ */ function() {
  function Join2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Join2.create = function(value0) {
    return function(value1) {
      return new Join2(value0, value1);
    };
  };
  return Join2;
}();
var Kill = /* @__PURE__ */ function() {
  function Kill2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Kill2.create = function(value0) {
    return function(value1) {
      return new Kill2(value0, value1);
    };
  };
  return Kill2;
}();
var GetRef = /* @__PURE__ */ function() {
  function GetRef2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  GetRef2.create = function(value0) {
    return function(value1) {
      return new GetRef2(value0, value1);
    };
  };
  return GetRef2;
}();
var HalogenM = function(x) {
  return x;
};
var ordSubscriptionId = ordInt;
var ordForkId = ordInt;
var monadHalogenM = freeMonad;
var monadStateHalogenM = {
  state: function($181) {
    return HalogenM(liftF(State.create($181)));
  },
  Monad0: function() {
    return monadHalogenM;
  }
};
var monadEffectHalogenM = function(dictMonadEffect) {
  return {
    liftEffect: function() {
      var $186 = liftEffect(dictMonadEffect);
      return function($187) {
        return HalogenM(liftF(Lift2.create($186($187))));
      };
    }(),
    Monad0: function() {
      return monadHalogenM;
    }
  };
};
var monadAffHalogenM = function(dictMonadAff) {
  var monadEffectHalogenM1 = monadEffectHalogenM(dictMonadAff.MonadEffect0());
  return {
    liftAff: function() {
      var $188 = liftAff(dictMonadAff);
      return function($189) {
        return HalogenM(liftF(Lift2.create($188($189))));
      };
    }(),
    MonadEffect0: function() {
      return monadEffectHalogenM1;
    }
  };
};
var functorHalogenM = freeFunctor;
var bindHalogenM = freeBind;
var applicativeHalogenM = freeApplicative;

// output/Halogen.Query.HalogenQ/index.js
var Initialize = /* @__PURE__ */ function() {
  function Initialize2(value0) {
    this.value0 = value0;
  }
  ;
  Initialize2.create = function(value0) {
    return new Initialize2(value0);
  };
  return Initialize2;
}();
var Finalize = /* @__PURE__ */ function() {
  function Finalize2(value0) {
    this.value0 = value0;
  }
  ;
  Finalize2.create = function(value0) {
    return new Finalize2(value0);
  };
  return Finalize2;
}();
var Receive = /* @__PURE__ */ function() {
  function Receive2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Receive2.create = function(value0) {
    return function(value1) {
      return new Receive2(value0, value1);
    };
  };
  return Receive2;
}();
var Action2 = /* @__PURE__ */ function() {
  function Action3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Action3.create = function(value0) {
    return function(value1) {
      return new Action3(value0, value1);
    };
  };
  return Action3;
}();
var Query = /* @__PURE__ */ function() {
  function Query2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Query2.create = function(value0) {
    return function(value1) {
      return new Query2(value0, value1);
    };
  };
  return Query2;
}();

// output/Halogen.VDom.Thunk/index.js
var $runtime_lazy7 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var unsafeEqThunk = function(v, v1) {
  return refEq2(v.value0, v1.value0) && (refEq2(v.value1, v1.value1) && v.value1(v.value3, v1.value3));
};
var runThunk = function(v) {
  return v.value2(v.value3);
};
var buildThunk = function(toVDom) {
  var haltThunk = function(state3) {
    return halt(state3.vdom);
  };
  var $lazy_patchThunk = $runtime_lazy7("patchThunk", "Halogen.VDom.Thunk", function() {
    return function(state3, t2) {
      var $48 = unsafeEqThunk(state3.thunk, t2);
      if ($48) {
        return mkStep(new Step(extract2(state3.vdom), state3, $lazy_patchThunk(112), haltThunk));
      }
      ;
      var vdom = step(state3.vdom, toVDom(runThunk(t2)));
      return mkStep(new Step(extract2(vdom), {
        vdom,
        thunk: t2
      }, $lazy_patchThunk(115), haltThunk));
    };
  });
  var patchThunk = $lazy_patchThunk(108);
  var renderThunk = function(spec) {
    return function(t) {
      var vdom = buildVDom(spec)(toVDom(runThunk(t)));
      return mkStep(new Step(extract2(vdom), {
        thunk: t,
        vdom
      }, patchThunk, haltThunk));
    };
  };
  return renderThunk;
};

// output/Halogen.Component/index.js
var voidLeft2 = /* @__PURE__ */ voidLeft(functorHalogenM);
var traverse_3 = /* @__PURE__ */ traverse_(applicativeHalogenM)(foldableMaybe);
var map9 = /* @__PURE__ */ map(functorHalogenM);
var pure4 = /* @__PURE__ */ pure(applicativeHalogenM);
var ComponentSlot = /* @__PURE__ */ function() {
  function ComponentSlot2(value0) {
    this.value0 = value0;
  }
  ;
  ComponentSlot2.create = function(value0) {
    return new ComponentSlot2(value0);
  };
  return ComponentSlot2;
}();
var ThunkSlot = /* @__PURE__ */ function() {
  function ThunkSlot2(value0) {
    this.value0 = value0;
  }
  ;
  ThunkSlot2.create = function(value0) {
    return new ThunkSlot2(value0);
  };
  return ThunkSlot2;
}();
var unComponentSlot = unsafeCoerce2;
var unComponent = unsafeCoerce2;
var mkEval = function(args) {
  return function(v) {
    if (v instanceof Initialize) {
      return voidLeft2(traverse_3(args.handleAction)(args.initialize))(v.value0);
    }
    ;
    if (v instanceof Finalize) {
      return voidLeft2(traverse_3(args.handleAction)(args.finalize))(v.value0);
    }
    ;
    if (v instanceof Receive) {
      return voidLeft2(traverse_3(args.handleAction)(args.receive(v.value0)))(v.value1);
    }
    ;
    if (v instanceof Action2) {
      return voidLeft2(args.handleAction(v.value0))(v.value1);
    }
    ;
    if (v instanceof Query) {
      return unCoyoneda(function(g) {
        var $45 = map9(maybe(v.value1(unit))(g));
        return function($46) {
          return $45(args.handleQuery($46));
        };
      })(v.value0);
    }
    ;
    throw new Error("Failed pattern match at Halogen.Component (line 182, column 15 - line 192, column 71): " + [v.constructor.name]);
  };
};
var mkComponent = unsafeCoerce2;
var defaultEval = /* @__PURE__ */ function() {
  return {
    handleAction: $$const(pure4(unit)),
    handleQuery: $$const(pure4(Nothing.value)),
    receive: $$const(Nothing.value),
    initialize: Nothing.value,
    finalize: Nothing.value
  };
}();

// output/Halogen.HTML.Elements/index.js
var element2 = /* @__PURE__ */ function() {
  return element(Nothing.value);
}();
var h3 = /* @__PURE__ */ element2("h3");
var li = /* @__PURE__ */ element2("li");
var p = /* @__PURE__ */ element2("p");
var p_ = /* @__PURE__ */ p([]);
var ul = /* @__PURE__ */ element2("ul");
var div2 = /* @__PURE__ */ element2("div");
var div_ = /* @__PURE__ */ div2([]);
var a = /* @__PURE__ */ element2("a");
var a_ = /* @__PURE__ */ a([]);

// output/Web.HTML.Event.EventTypes/index.js
var domcontentloaded = "DOMContentLoaded";

// output/Web.UIEvent.MouseEvent.EventTypes/index.js
var click = "click";

// output/Halogen.HTML.Events/index.js
var mouseHandler = unsafeCoerce2;
var handler2 = function(et) {
  return function(f) {
    return handler(et)(function(ev) {
      return new Just(new Action(f(ev)));
    });
  };
};
var onClick = /* @__PURE__ */ function() {
  var $15 = handler2(click);
  return function($16) {
    return $15(mouseHandler($16));
  };
}();

// output/Halogen.HTML.Properties/index.js
var unwrap3 = /* @__PURE__ */ unwrap();
var prop2 = function(dictIsProp) {
  return prop(dictIsProp);
};
var prop22 = /* @__PURE__ */ prop2(isPropString);
var prop3 = /* @__PURE__ */ prop2(isPropInt);
var tabIndex = /* @__PURE__ */ prop3("tabIndex");
var classes = /* @__PURE__ */ function() {
  var $32 = prop22("className");
  var $33 = joinWith(" ");
  var $34 = map(functorArray)(unwrap3);
  return function($35) {
    return $32($33($34($35)));
  };
}();
var attr2 = /* @__PURE__ */ function() {
  return attr(Nothing.value);
}();

// output/Halogen.HTML.Properties.ARIA/index.js
var role = /* @__PURE__ */ attr2("role");

// output/Button/index.js
var map10 = /* @__PURE__ */ map(functorArray);
var show2 = /* @__PURE__ */ show(showString);
var bind4 = /* @__PURE__ */ bind(bindHalogenM);
var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
var ConnectWallet = /* @__PURE__ */ function() {
  function ConnectWallet2(value0) {
    this.value0 = value0;
  }
  ;
  ConnectWallet2.create = function(value0) {
    return new ConnectWallet2(value0);
  };
  return ConnectWallet2;
}();
var renderWalletListItem = function(s) {
  return li([onClick(function(v) {
    return new ConnectWallet(s);
  })])([a_([text(s)])]);
};
var renderWalletsList = function(ws) {
  if (ws.length === 0) {
    return div2([classes(["dropdown-content z-[1] card card-compact w-64 p-2 shadow bg-primary text-primary-content"])])([div2([classes(["card-body"])])([h3([classes(["card-title"])])([text("no wallets")]), p_([text("no wallets")])])]);
  }
  ;
  return ul([tabIndex(0), classes(["dropdown-content z-[1] menu p-2 shadow bg-base-100 rounded-box w-52"])])(map10(renderWalletListItem)(ws));
};
var printConnectedWallet = function(mw) {
  if (mw instanceof Nothing) {
    return "Connect";
  }
  ;
  if (mw instanceof Just) {
    return "Connected" + (" " + mw.value0.name);
  }
  ;
  throw new Error("Failed pattern match at Button (line 84, column 27 - line 86, column 41): " + [mw.constructor.name]);
};
var renderWalletConnect = function(v) {
  return div2([classes(["flex justify-end", "dropdown dropdown-bottom dropdown-end"])])([div2([tabIndex(0), role("button"), classes(["btn m-1"])])([text(printConnectedWallet(v.connectedWallet))]), renderWalletsList(v.availableWalletExtensions)]);
};
var render = function(state3) {
  return div_([renderWalletConnect(state3), function() {
    if (state3.connectedWallet instanceof Nothing) {
      return div_([]);
    }
    ;
    if (state3.connectedWallet instanceof Just) {
      return text(show2(state3.connectedWallet.value0.balance));
    }
    ;
    throw new Error("Failed pattern match at Button (line 34, column 7 - line 36, column 45): " + [state3.connectedWallet.constructor.name]);
  }()]);
};
var handleAction = function(dictMonadAff) {
  var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
  return function(v) {
    return bind4(liftAff2(enable(v.value0)([{
      cip: 30
    }])))(function(api) {
      return bind4(liftAff2(getBalance(api)))(function(b2) {
        var b22 = fromMaybe(value.zero)(value.fromHex(b2));
        return modify_3(function(st) {
          var $24 = {};
          for (var $25 in st) {
            if ({}.hasOwnProperty.call(st, $25)) {
              $24[$25] = st[$25];
            }
            ;
          }
          ;
          $24.connectedWallet = new Just({
            api,
            name: v.value0,
            balance: bigNum.toStr(value.coin(b22))
          });
          return $24;
        });
      });
    });
  };
};
var component = function(dictMonadAff) {
  return mkComponent({
    initialState: function(ws) {
      return {
        availableWalletExtensions: ws,
        connectedWallet: Nothing.value
      };
    },
    render,
    "eval": mkEval({
      handleQuery: defaultEval.handleQuery,
      receive: defaultEval.receive,
      initialize: defaultEval.initialize,
      finalize: defaultEval.finalize,
      handleAction: handleAction(dictMonadAff)
    })
  });
};

// output/Web.HTML/foreign.js
var windowImpl = function() {
  return window;
};

// output/Web.HTML.HTMLDocument/foreign.js
function _readyState(doc) {
  return doc.readyState;
}

// output/Web.HTML.HTMLDocument.ReadyState/index.js
var Loading = /* @__PURE__ */ function() {
  function Loading2() {
  }
  ;
  Loading2.value = new Loading2();
  return Loading2;
}();
var Interactive = /* @__PURE__ */ function() {
  function Interactive2() {
  }
  ;
  Interactive2.value = new Interactive2();
  return Interactive2;
}();
var Complete = /* @__PURE__ */ function() {
  function Complete2() {
  }
  ;
  Complete2.value = new Complete2();
  return Complete2;
}();
var parse = function(v) {
  if (v === "loading") {
    return new Just(Loading.value);
  }
  ;
  if (v === "interactive") {
    return new Just(Interactive.value);
  }
  ;
  if (v === "complete") {
    return new Just(Complete.value);
  }
  ;
  return Nothing.value;
};

// output/Web.HTML.HTMLDocument/index.js
var map11 = /* @__PURE__ */ map(functorEffect);
var toParentNode = unsafeCoerce2;
var toDocument = unsafeCoerce2;
var readyState = function(doc) {
  return map11(function() {
    var $4 = fromMaybe(Loading.value);
    return function($5) {
      return $4(parse($5));
    };
  }())(function() {
    return _readyState(doc);
  });
};

// output/Web.HTML.HTMLElement/foreign.js
function _read(nothing, just, value13) {
  var tag = Object.prototype.toString.call(value13);
  if (tag.indexOf("[object HTML") === 0 && tag.indexOf("Element]") === tag.length - 8) {
    return just(value13);
  } else {
    return nothing;
  }
}

// output/Web.HTML.HTMLElement/index.js
var toNode2 = unsafeCoerce2;
var fromElement = function(x) {
  return _read(Nothing.value, Just.create, x);
};

// output/Web.HTML.Window/foreign.js
function document(window2) {
  return function() {
    return window2.document;
  };
}

// output/Web.HTML.Window/index.js
var toEventTarget = unsafeCoerce2;

// output/Halogen.Aff.Util/index.js
var bind5 = /* @__PURE__ */ bind(bindAff);
var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindEffect);
var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
var pure5 = /* @__PURE__ */ pure(applicativeAff);
var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
var pure1 = /* @__PURE__ */ pure(applicativeEffect);
var map13 = /* @__PURE__ */ map(functorEffect);
var discard2 = /* @__PURE__ */ discard(discardUnit);
var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
var selectElement = function(query2) {
  return bind5(liftEffect4(bindFlipped4(composeKleisliFlipped2(function() {
    var $16 = querySelector(query2);
    return function($17) {
      return $16(toParentNode($17));
    };
  }())(document))(windowImpl)))(function(mel) {
    return pure5(bindFlipped1(fromElement)(mel));
  });
};
var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure1(unit))));
var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
  return function __do4() {
    var rs = bindFlipped4(readyState)(bindFlipped4(document)(windowImpl))();
    if (rs instanceof Loading) {
      var et = map13(toEventTarget)(windowImpl)();
      var listener = eventListener(function(v) {
        return callback(new Right(unit));
      })();
      addEventListener2(domcontentloaded)(listener)(false)(et)();
      return effectCanceler(removeEventListener2(domcontentloaded)(listener)(false)(et));
    }
    ;
    callback(new Right(unit))();
    return nonCanceler;
  };
});
var awaitBody = /* @__PURE__ */ discard2(bindAff)(awaitLoad)(function() {
  return bind5(selectElement("body"))(function(body2) {
    return maybe(throwError2(error("Could not find body")))(pure5)(body2);
  });
});

// output/Control.Monad.Fork.Class/index.js
var monadForkAff = {
  suspend: suspendAff,
  fork: forkAff,
  join: joinFiber,
  Monad0: function() {
    return monadAff;
  },
  Functor1: function() {
    return functorFiber;
  }
};
var fork = function(dict) {
  return dict.fork;
};

// output/Effect.Console/foreign.js
var warn = function(s) {
  return function() {
    console.warn(s);
  };
};

// output/Halogen.Aff.Driver.State/index.js
var unRenderStateX = unsafeCoerce2;
var unDriverStateX = unsafeCoerce2;
var renderStateX_ = function(dictApplicative) {
  var traverse_7 = traverse_(dictApplicative)(foldableMaybe);
  return function(f) {
    return unDriverStateX(function(st) {
      return traverse_7(f)(st.rendering);
    });
  };
};
var mkRenderStateX = unsafeCoerce2;
var renderStateX = function(dictFunctor) {
  return function(f) {
    return unDriverStateX(function(st) {
      return mkRenderStateX(f(st.rendering));
    });
  };
};
var mkDriverStateXRef = unsafeCoerce2;
var mapDriverState = function(f) {
  return function(v) {
    return f(v);
  };
};
var initDriverState = function(component3) {
  return function(input3) {
    return function(handler3) {
      return function(lchs) {
        return function __do4() {
          var selfRef = $$new({})();
          var childrenIn = $$new(empty4)();
          var childrenOut = $$new(empty4)();
          var handlerRef = $$new(handler3)();
          var pendingQueries = $$new(new Just(Nil.value))();
          var pendingOuts = $$new(new Just(Nil.value))();
          var pendingHandlers = $$new(Nothing.value)();
          var fresh2 = $$new(1)();
          var subscriptions = $$new(new Just(empty3))();
          var forks = $$new(empty3)();
          var ds = {
            component: component3,
            state: component3.initialState(input3),
            refs: empty3,
            children: empty4,
            childrenIn,
            childrenOut,
            selfRef,
            handlerRef,
            pendingQueries,
            pendingOuts,
            pendingHandlers,
            rendering: Nothing.value,
            fresh: fresh2,
            subscriptions,
            forks,
            lifecycleHandlers: lchs
          };
          write(ds)(selfRef)();
          return mkDriverStateXRef(selfRef);
        };
      };
    };
  };
};

// output/Halogen.Aff.Driver.Eval/index.js
var traverse_4 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindMaybe);
var lookup4 = /* @__PURE__ */ lookup2(ordSubscriptionId);
var bind12 = /* @__PURE__ */ bind(bindAff);
var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
var discard3 = /* @__PURE__ */ discard(discardUnit);
var discard1 = /* @__PURE__ */ discard3(bindAff);
var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
var fork3 = /* @__PURE__ */ fork(monadForkAff);
var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
var pure6 = /* @__PURE__ */ pure(applicativeAff);
var map15 = /* @__PURE__ */ map(functorCoyoneda);
var parallel3 = /* @__PURE__ */ parallel(parallelAff);
var map16 = /* @__PURE__ */ map(functorAff);
var sequential2 = /* @__PURE__ */ sequential(parallelAff);
var map22 = /* @__PURE__ */ map(functorMaybe);
var insert3 = /* @__PURE__ */ insert(ordSubscriptionId);
var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
var $$delete2 = /* @__PURE__ */ $$delete(ordForkId);
var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
var insert1 = /* @__PURE__ */ insert(ordForkId);
var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
var lookup1 = /* @__PURE__ */ lookup2(ordForkId);
var lookup22 = /* @__PURE__ */ lookup2(ordString);
var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
var alter2 = /* @__PURE__ */ alter(ordString);
var unsubscribe3 = function(sid) {
  return function(ref2) {
    return function __do4() {
      var v = read(ref2)();
      var subs = read(v.subscriptions)();
      return traverse_4(unsubscribe)(bindFlipped5(lookup4(sid))(subs))();
    };
  };
};
var queueOrRun = function(ref2) {
  return function(au) {
    return bind12(liftEffect5(read(ref2)))(function(v) {
      if (v instanceof Nothing) {
        return au;
      }
      ;
      if (v instanceof Just) {
        return liftEffect5(write(new Just(new Cons(au, v.value0)))(ref2));
      }
      ;
      throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
    });
  };
};
var handleLifecycle = function(lchs) {
  return function(f) {
    return discard1(liftEffect5(write({
      initializers: Nil.value,
      finalizers: Nil.value
    })(lchs)))(function() {
      return bind12(liftEffect5(f))(function(result) {
        return bind12(liftEffect5(read(lchs)))(function(v) {
          return discard1(traverse_22(fork3)(v.finalizers))(function() {
            return discard1(parSequence_3(v.initializers))(function() {
              return pure6(result);
            });
          });
        });
      });
    });
  };
};
var handleAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure(applicativeEffect)(unit))));
var fresh = function(f) {
  return function(ref2) {
    return bind12(liftEffect5(read(ref2)))(function(v) {
      return liftEffect5(modify$prime(function(i2) {
        return {
          state: i2 + 1 | 0,
          value: f(i2)
        };
      })(v.fresh));
    });
  };
};
var evalQ = function(render2) {
  return function(ref2) {
    return function(q2) {
      return bind12(liftEffect5(read(ref2)))(function(v) {
        return evalM(render2)(ref2)(v["component"]["eval"](new Query(map15(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
      });
    };
  };
};
var evalM = function(render2) {
  return function(initRef) {
    return function(v) {
      var evalChildQuery = function(ref2) {
        return function(cqb) {
          return bind12(liftEffect5(read(ref2)))(function(v1) {
            return unChildQueryBox(function(v2) {
              var evalChild = function(v3) {
                return parallel3(bind12(liftEffect5(read(v3)))(function(dsx) {
                  return unDriverStateX(function(ds) {
                    return evalQ(render2)(ds.selfRef)(v2.value1);
                  })(dsx);
                }));
              };
              return map16(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
            })(cqb);
          });
        };
      };
      var go2 = function(ref2) {
        return function(v1) {
          if (v1 instanceof State) {
            return bind12(liftEffect5(read(ref2)))(function(v2) {
              var v3 = v1.value0(v2.state);
              if (unsafeRefEq(v2.state)(v3.value1)) {
                return pure6(v3.value0);
              }
              ;
              if (otherwise) {
                return discard1(liftEffect5(write({
                  component: v2.component,
                  refs: v2.refs,
                  children: v2.children,
                  childrenIn: v2.childrenIn,
                  childrenOut: v2.childrenOut,
                  selfRef: v2.selfRef,
                  handlerRef: v2.handlerRef,
                  pendingQueries: v2.pendingQueries,
                  pendingOuts: v2.pendingOuts,
                  pendingHandlers: v2.pendingHandlers,
                  rendering: v2.rendering,
                  fresh: v2.fresh,
                  subscriptions: v2.subscriptions,
                  forks: v2.forks,
                  lifecycleHandlers: v2.lifecycleHandlers,
                  state: v3.value1
                })(ref2)))(function() {
                  return discard1(handleLifecycle(v2.lifecycleHandlers)(render2(v2.lifecycleHandlers)(ref2)))(function() {
                    return pure6(v3.value0);
                  });
                });
              }
              ;
              throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
            });
          }
          ;
          if (v1 instanceof Subscribe) {
            return bind12(fresh(SubscriptionId)(ref2))(function(sid) {
              return bind12(liftEffect5(subscribe(v1.value0(sid))(function(act) {
                return handleAff(evalF(render2)(ref2)(new Action(act)));
              })))(function(finalize) {
                return bind12(liftEffect5(read(ref2)))(function(v2) {
                  return discard1(liftEffect5(modify_(map22(insert3(sid)(finalize)))(v2.subscriptions)))(function() {
                    return pure6(v1.value1(sid));
                  });
                });
              });
            });
          }
          ;
          if (v1 instanceof Unsubscribe) {
            return discard1(liftEffect5(unsubscribe3(v1.value0)(ref2)))(function() {
              return pure6(v1.value1);
            });
          }
          ;
          if (v1 instanceof Lift2) {
            return v1.value0;
          }
          ;
          if (v1 instanceof ChildQuery2) {
            return evalChildQuery(ref2)(v1.value0);
          }
          ;
          if (v1 instanceof Raise) {
            return bind12(liftEffect5(read(ref2)))(function(v2) {
              return bind12(liftEffect5(read(v2.handlerRef)))(function(handler3) {
                return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                  return pure6(v1.value1);
                });
              });
            });
          }
          ;
          if (v1 instanceof Par) {
            return sequential2(retractFreeAp2(hoistFreeAp(function() {
              var $119 = evalM(render2)(ref2);
              return function($120) {
                return parallel3($119($120));
              };
            }())(v1.value0)));
          }
          ;
          if (v1 instanceof Fork) {
            return bind12(fresh(ForkId)(ref2))(function(fid) {
              return bind12(liftEffect5(read(ref2)))(function(v2) {
                return bind12(liftEffect5($$new(false)))(function(doneRef) {
                  return bind12(fork3($$finally(liftEffect5(function __do4() {
                    modify_($$delete2(fid))(v2.forks)();
                    return write(true)(doneRef)();
                  }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                    return discard1(liftEffect5(unlessM2(read(doneRef))(modify_(insert1(fid)(fiber))(v2.forks))))(function() {
                      return pure6(v1.value1(fid));
                    });
                  });
                });
              });
            });
          }
          ;
          if (v1 instanceof Join) {
            return bind12(liftEffect5(read(ref2)))(function(v2) {
              return bind12(liftEffect5(read(v2.forks)))(function(forkMap) {
                return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                  return pure6(v1.value1);
                });
              });
            });
          }
          ;
          if (v1 instanceof Kill) {
            return bind12(liftEffect5(read(ref2)))(function(v2) {
              return bind12(liftEffect5(read(v2.forks)))(function(forkMap) {
                return discard1(traverse_32(killFiber(error("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                  return pure6(v1.value1);
                });
              });
            });
          }
          ;
          if (v1 instanceof GetRef) {
            return bind12(liftEffect5(read(ref2)))(function(v2) {
              return pure6(v1.value1(lookup22(v1.value0)(v2.refs)));
            });
          }
          ;
          throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 83, column 12 - line 139, column 33): " + [v1.constructor.name]);
        };
      };
      return foldFree2(go2(initRef))(v);
    };
  };
};
var evalF = function(render2) {
  return function(ref2) {
    return function(v) {
      if (v instanceof RefUpdate) {
        return liftEffect5(flip(modify_)(ref2)(mapDriverState(function(st) {
          return {
            component: st.component,
            state: st.state,
            children: st.children,
            childrenIn: st.childrenIn,
            childrenOut: st.childrenOut,
            selfRef: st.selfRef,
            handlerRef: st.handlerRef,
            pendingQueries: st.pendingQueries,
            pendingOuts: st.pendingOuts,
            pendingHandlers: st.pendingHandlers,
            rendering: st.rendering,
            fresh: st.fresh,
            subscriptions: st.subscriptions,
            forks: st.forks,
            lifecycleHandlers: st.lifecycleHandlers,
            refs: alter2($$const(v.value1))(v.value0)(st.refs)
          };
        })));
      }
      ;
      if (v instanceof Action) {
        return bind12(liftEffect5(read(ref2)))(function(v1) {
          return evalM(render2)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
        });
      }
      ;
      throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
    };
  };
};

// output/Halogen.Aff.Driver/index.js
var bind6 = /* @__PURE__ */ bind(bindEffect);
var discard4 = /* @__PURE__ */ discard(discardUnit);
var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
var fork4 = /* @__PURE__ */ fork(monadForkAff);
var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindEffect);
var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
var discard22 = /* @__PURE__ */ discard4(bindAff);
var parSequence_4 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
var pure7 = /* @__PURE__ */ pure(applicativeEffect);
var map17 = /* @__PURE__ */ map(functorEffect);
var pure12 = /* @__PURE__ */ pure(applicativeAff);
var when2 = /* @__PURE__ */ when(applicativeEffect);
var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
var $$void5 = /* @__PURE__ */ $$void(functorAff);
var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
var bind13 = /* @__PURE__ */ bind(bindAff);
var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
var newLifecycleHandlers = /* @__PURE__ */ function() {
  return $$new({
    initializers: Nil.value,
    finalizers: Nil.value
  });
}();
var handlePending = function(ref2) {
  return function __do4() {
    var queue = read(ref2)();
    write(Nothing.value)(ref2)();
    return for_2(queue)(function() {
      var $59 = traverse_5(fork4);
      return function($60) {
        return handleAff($59(reverse($60)));
      };
    }())();
  };
};
var cleanupSubscriptionsAndForks = function(v) {
  return function __do4() {
    bindFlipped6(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
    write(Nothing.value)(v.subscriptions)();
    bindFlipped6(traverse_33(function() {
      var $61 = killFiber(error("finalized"));
      return function($62) {
        return handleAff($61($62));
      };
    }()))(read(v.forks))();
    return write(empty3)(v.forks)();
  };
};
var runUI = function(renderSpec2) {
  return function(component3) {
    return function(i2) {
      var squashChildInitializers = function(lchs) {
        return function(preInits) {
          return unDriverStateX(function(st) {
            var parentInitializer = evalM(render2)(st.selfRef)(st["component"]["eval"](new Initialize(unit)));
            return modify_(function(handlers) {
              return {
                initializers: new Cons(discard22(parSequence_4(reverse(handlers.initializers)))(function() {
                  return discard22(parentInitializer)(function() {
                    return liftEffect6(function __do4() {
                      handlePending(st.pendingQueries)();
                      return handlePending(st.pendingOuts)();
                    });
                  });
                }), preInits),
                finalizers: handlers.finalizers
              };
            })(lchs);
          });
        };
      };
      var runComponent = function(lchs) {
        return function(handler3) {
          return function(j) {
            return unComponent(function(c) {
              return function __do4() {
                var lchs$prime = newLifecycleHandlers();
                var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                var pre2 = read(lchs)();
                write({
                  initializers: Nil.value,
                  finalizers: pre2.finalizers
                })(lchs)();
                bindFlipped6(unDriverStateX(function() {
                  var $63 = render2(lchs);
                  return function($64) {
                    return $63(function(v) {
                      return v.selfRef;
                    }($64));
                  };
                }()))(read($$var2))();
                bindFlipped6(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
                return $$var2;
              };
            });
          };
        };
      };
      var renderChild = function(lchs) {
        return function(handler3) {
          return function(childrenInRef) {
            return function(childrenOutRef) {
              return unComponentSlot(function(slot) {
                return function __do4() {
                  var childrenIn = map17(slot.pop)(read(childrenInRef))();
                  var $$var2 = function() {
                    if (childrenIn instanceof Just) {
                      write(childrenIn.value0.value1)(childrenInRef)();
                      var dsx = read(childrenIn.value0.value0)();
                      unDriverStateX(function(st) {
                        return function __do5() {
                          flip(write)(st.handlerRef)(function() {
                            var $65 = maybe(pure12(unit))(handler3);
                            return function($66) {
                              return $65(slot.output($66));
                            };
                          }())();
                          return handleAff(evalM(render2)(st.selfRef)(st["component"]["eval"](new Receive(slot.input, unit))))();
                        };
                      })(dsx)();
                      return childrenIn.value0.value0;
                    }
                    ;
                    if (childrenIn instanceof Nothing) {
                      return runComponent(lchs)(function() {
                        var $67 = maybe(pure12(unit))(handler3);
                        return function($68) {
                          return $67(slot.output($68));
                        };
                      }())(slot.input)(slot.component)();
                    }
                    ;
                    throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                  }();
                  var isDuplicate = map17(function($69) {
                    return isJust(slot.get($69));
                  })(read(childrenOutRef))();
                  when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                  modify_(slot.set($$var2))(childrenOutRef)();
                  return bind6(read($$var2))(renderStateX2(function(v) {
                    if (v instanceof Nothing) {
                      return $$throw("Halogen internal error: child was not initialized in renderChild");
                    }
                    ;
                    if (v instanceof Just) {
                      return pure7(renderSpec2.renderChild(v.value0));
                    }
                    ;
                    throw new Error("Failed pattern match at Halogen.Aff.Driver (line 227, column 37 - line 229, column 50): " + [v.constructor.name]);
                  }))();
                };
              });
            };
          };
        };
      };
      var render2 = function(lchs) {
        return function($$var2) {
          return function __do4() {
            var v = read($$var2)();
            var shouldProcessHandlers = map17(isNothing)(read(v.pendingHandlers))();
            when2(shouldProcessHandlers)(write(new Just(Nil.value))(v.pendingHandlers))();
            write(empty4)(v.childrenOut)();
            write(v.children)(v.childrenIn)();
            var handler3 = function() {
              var $70 = queueOrRun(v.pendingHandlers);
              var $71 = evalF(render2)(v.selfRef);
              return function($72) {
                return $70($$void5($71($72)));
              };
            }();
            var childHandler = function() {
              var $73 = queueOrRun(v.pendingQueries);
              return function($74) {
                return $73(handler3(Action.create($74)));
              };
            }();
            var rendering = renderSpec2.render(function($75) {
              return handleAff(handler3($75));
            })(renderChild(lchs)(childHandler)(v.childrenIn)(v.childrenOut))(v.component.render(v.state))(v.rendering)();
            var children2 = read(v.childrenOut)();
            var childrenIn = read(v.childrenIn)();
            foreachSlot2(childrenIn)(function(v1) {
              return function __do5() {
                var childDS = read(v1)();
                renderStateX_2(renderSpec2.removeChild)(childDS)();
                return finalize(lchs)(childDS)();
              };
            })();
            flip(modify_)(v.selfRef)(mapDriverState(function(ds$prime) {
              return {
                component: ds$prime.component,
                state: ds$prime.state,
                refs: ds$prime.refs,
                childrenIn: ds$prime.childrenIn,
                childrenOut: ds$prime.childrenOut,
                selfRef: ds$prime.selfRef,
                handlerRef: ds$prime.handlerRef,
                pendingQueries: ds$prime.pendingQueries,
                pendingOuts: ds$prime.pendingOuts,
                pendingHandlers: ds$prime.pendingHandlers,
                fresh: ds$prime.fresh,
                subscriptions: ds$prime.subscriptions,
                forks: ds$prime.forks,
                lifecycleHandlers: ds$prime.lifecycleHandlers,
                rendering: new Just(rendering),
                children: children2
              };
            }))();
            return when2(shouldProcessHandlers)(flip(tailRecM3)(unit)(function(v1) {
              return function __do5() {
                var handlers = read(v.pendingHandlers)();
                write(new Just(Nil.value))(v.pendingHandlers)();
                traverse_23(function() {
                  var $76 = traverse_5(fork4);
                  return function($77) {
                    return handleAff($76(reverse($77)));
                  };
                }())(handlers)();
                var mmore = read(v.pendingHandlers)();
                var $52 = maybe(false)($$null)(mmore);
                if ($52) {
                  return voidLeft3(write(Nothing.value)(v.pendingHandlers))(new Done(unit))();
                }
                ;
                return new Loop(unit);
              };
            }))();
          };
        };
      };
      var finalize = function(lchs) {
        return unDriverStateX(function(st) {
          return function __do4() {
            cleanupSubscriptionsAndForks(st)();
            var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
            modify_(function(handlers) {
              return {
                initializers: handlers.initializers,
                finalizers: new Cons(f, handlers.finalizers)
              };
            })(lchs)();
            return foreachSlot2(st.children)(function(v) {
              return function __do5() {
                var dsx = read(v)();
                return finalize(lchs)(dsx)();
              };
            })();
          };
        });
      };
      var evalDriver = function(disposed) {
        return function(ref2) {
          return function(q2) {
            return bind13(liftEffect6(read(disposed)))(function(v) {
              if (v) {
                return pure12(Nothing.value);
              }
              ;
              return evalQ(render2)(ref2)(q2);
            });
          };
        };
      };
      var dispose = function(disposed) {
        return function(lchs) {
          return function(dsx) {
            return handleLifecycle(lchs)(function __do4() {
              var v = read(disposed)();
              if (v) {
                return unit;
              }
              ;
              write(true)(disposed)();
              finalize(lchs)(dsx)();
              return unDriverStateX(function(v1) {
                return function __do5() {
                  var v2 = liftEffect1(read(v1.selfRef))();
                  return for_2(v2.rendering)(renderSpec2.dispose)();
                };
              })(dsx)();
            });
          };
        };
      };
      return bind13(liftEffect6(newLifecycleHandlers))(function(lchs) {
        return bind13(liftEffect6($$new(false)))(function(disposed) {
          return handleLifecycle(lchs)(function __do4() {
            var sio = create();
            var dsx = bindFlipped6(read)(runComponent(lchs)(function() {
              var $78 = notify(sio.listener);
              return function($79) {
                return liftEffect6($78($79));
              };
            }())(i2)(component3))();
            return unDriverStateX(function(st) {
              return pure7({
                query: evalDriver(disposed)(st.selfRef),
                messages: sio.emitter,
                dispose: dispose(disposed)(lchs)(dsx)
              });
            })(dsx)();
          });
        });
      });
    };
  };
};

// output/Web.DOM.Node/foreign.js
var getEffProp2 = function(name15) {
  return function(node) {
    return function() {
      return node[name15];
    };
  };
};
var baseURI = getEffProp2("baseURI");
var _ownerDocument = getEffProp2("ownerDocument");
var _parentNode = getEffProp2("parentNode");
var _parentElement = getEffProp2("parentElement");
var childNodes = getEffProp2("childNodes");
var _firstChild = getEffProp2("firstChild");
var _lastChild = getEffProp2("lastChild");
var _previousSibling = getEffProp2("previousSibling");
var _nextSibling = getEffProp2("nextSibling");
var _nodeValue = getEffProp2("nodeValue");
var textContent = getEffProp2("textContent");
function insertBefore(node1) {
  return function(node2) {
    return function(parent2) {
      return function() {
        parent2.insertBefore(node1, node2);
      };
    };
  };
}
function appendChild(node) {
  return function(parent2) {
    return function() {
      parent2.appendChild(node);
    };
  };
}
function removeChild2(node) {
  return function(parent2) {
    return function() {
      parent2.removeChild(node);
    };
  };
}

// output/Web.DOM.Node/index.js
var map18 = /* @__PURE__ */ map(functorEffect);
var parentNode2 = /* @__PURE__ */ function() {
  var $6 = map18(toMaybe);
  return function($7) {
    return $6(_parentNode($7));
  };
}();
var nextSibling = /* @__PURE__ */ function() {
  var $15 = map18(toMaybe);
  return function($16) {
    return $15(_nextSibling($16));
  };
}();

// output/Halogen.VDom.Driver/index.js
var $runtime_lazy8 = function(name15, moduleName, init2) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name15 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init2();
    state3 = 2;
    return val;
  };
};
var $$void6 = /* @__PURE__ */ $$void(functorEffect);
var pure8 = /* @__PURE__ */ pure(applicativeEffect);
var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
var unwrap4 = /* @__PURE__ */ unwrap();
var when3 = /* @__PURE__ */ when(applicativeEffect);
var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
var identity8 = /* @__PURE__ */ identity(categoryFn);
var bind14 = /* @__PURE__ */ bind(bindAff);
var liftEffect7 = /* @__PURE__ */ liftEffect(monadEffectAff);
var map19 = /* @__PURE__ */ map(functorEffect);
var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
var substInParent = function(v) {
  return function(v1) {
    return function(v2) {
      if (v1 instanceof Just && v2 instanceof Just) {
        return $$void6(insertBefore(v)(v1.value0)(v2.value0));
      }
      ;
      if (v1 instanceof Nothing && v2 instanceof Just) {
        return $$void6(appendChild(v)(v2.value0));
      }
      ;
      return pure8(unit);
    };
  };
};
var removeChild3 = function(v) {
  return function __do4() {
    var npn = parentNode2(v.node)();
    return traverse_6(function(pn) {
      return removeChild2(v.node)(pn);
    })(npn)();
  };
};
var mkSpec = function(handler3) {
  return function(renderChildRef) {
    return function(document2) {
      var getNode = unRenderStateX(function(v) {
        return v.node;
      });
      var done = function(st) {
        if (st instanceof Just) {
          return halt(st.value0);
        }
        ;
        return unit;
      };
      var buildWidget2 = function(spec) {
        var buildThunk2 = buildThunk(unwrap4)(spec);
        var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
          return function(st, slot) {
            if (st instanceof Just) {
              if (slot instanceof ComponentSlot) {
                halt(st.value0);
                return $lazy_renderComponentSlot(100)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step$prime = step(st.value0, slot.value0);
                return mkStep(new Step(extract2(step$prime), new Just(step$prime), $lazy_patch(103), done));
              }
              ;
              throw new Error("Failed pattern match at Halogen.VDom.Driver (line 97, column 22 - line 103, column 79): " + [slot.constructor.name]);
            }
            ;
            return $lazy_render(104)(slot);
          };
        });
        var $lazy_render = $runtime_lazy8("render", "Halogen.VDom.Driver", function() {
          return function(slot) {
            if (slot instanceof ComponentSlot) {
              return $lazy_renderComponentSlot(86)(slot.value0);
            }
            ;
            if (slot instanceof ThunkSlot) {
              var step3 = buildThunk2(slot.value0);
              return mkStep(new Step(extract2(step3), new Just(step3), $lazy_patch(89), done));
            }
            ;
            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 84, column 7 - line 89, column 75): " + [slot.constructor.name]);
          };
        });
        var $lazy_renderComponentSlot = $runtime_lazy8("renderComponentSlot", "Halogen.VDom.Driver", function() {
          return function(cs) {
            var renderChild = read(renderChildRef)();
            var rsx = renderChild(cs)();
            var node = getNode(rsx);
            return mkStep(new Step(node, Nothing.value, $lazy_patch(117), done));
          };
        });
        var patch = $lazy_patch(91);
        var render2 = $lazy_render(82);
        var renderComponentSlot = $lazy_renderComponentSlot(109);
        return render2;
      };
      var buildAttributes = buildProp(handler3);
      return {
        buildWidget: buildWidget2,
        buildAttributes,
        document: document2
      };
    };
  };
};
var renderSpec = function(document2) {
  return function(container) {
    var render2 = function(handler3) {
      return function(child) {
        return function(v) {
          return function(v1) {
            if (v1 instanceof Nothing) {
              return function __do4() {
                var renderChildRef = $$new(child)();
                var spec = mkSpec(handler3)(renderChildRef)(document2);
                var machine = buildVDom(spec)(v);
                var node = extract2(machine);
                $$void6(appendChild(node)(toNode2(container)))();
                return {
                  machine,
                  node,
                  renderChildRef
                };
              };
            }
            ;
            if (v1 instanceof Just) {
              return function __do4() {
                write(child)(v1.value0.renderChildRef)();
                var parent2 = parentNode2(v1.value0.node)();
                var nextSib = nextSibling(v1.value0.node)();
                var machine$prime = step(v1.value0.machine, v);
                var newNode = extract2(machine$prime);
                when3(not2(unsafeRefEq)(v1.value0.node)(newNode))(substInParent(newNode)(nextSib)(parent2))();
                return {
                  machine: machine$prime,
                  node: newNode,
                  renderChildRef: v1.value0.renderChildRef
                };
              };
            }
            ;
            throw new Error("Failed pattern match at Halogen.VDom.Driver (line 157, column 5 - line 173, column 80): " + [v1.constructor.name]);
          };
        };
      };
    };
    return {
      render: render2,
      renderChild: identity8,
      removeChild: removeChild3,
      dispose: removeChild3
    };
  };
};
var runUI2 = function(component3) {
  return function(i2) {
    return function(element3) {
      return bind14(liftEffect7(map19(toDocument)(bindFlipped7(document)(windowImpl))))(function(document2) {
        return runUI(renderSpec(document2)(element3))(component3)(i2);
      });
    };
  };
};

// output/Main/index.js
var bind7 = /* @__PURE__ */ bind(bindAff);
var component2 = /* @__PURE__ */ component(monadAffAff);
var main2 = function __do3() {
  var ws = getAvailableWallets();
  return runHalogenAff(bind7(awaitBody)(function(body2) {
    return runUI2(component2)(ws)(body2);
  }))();
};
export {
  main2 as main
};
