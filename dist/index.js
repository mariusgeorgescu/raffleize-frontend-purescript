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

// output/Data.Boolean/index.js
var otherwise = true;

// output/Data.Function/index.js
var on = function(f) {
  return function(g) {
    return function(x) {
      return function(y) {
        return f(g(x))(g(y));
      };
    };
  };
};
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

// output/Type.Proxy/index.js
var $$Proxy = /* @__PURE__ */ function() {
  function $$Proxy2() {
  }
  ;
  $$Proxy2.value = new $$Proxy2();
  return $$Proxy2;
}();

// output/Data.Functor/index.js
var map = function(dict) {
  return dict.map;
};
var mapFlipped = function(dictFunctor) {
  var map111 = map(dictFunctor);
  return function(fa) {
    return function(f) {
      return map111(f)(fa);
    };
  };
};
var $$void = function(dictFunctor) {
  return map(dictFunctor)($$const(unit));
};
var voidLeft = function(dictFunctor) {
  var map111 = map(dictFunctor);
  return function(f) {
    return function(x) {
      return map111($$const(x))(f);
    };
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

// output/Data.Symbol/index.js
var reflectSymbol = function(dict) {
  return dict.reflectSymbol;
};

// output/Record.Unsafe/foreign.js
var unsafeSet = function(label5) {
  return function(value13) {
    return function(rec) {
      var copy2 = {};
      for (var key in rec) {
        if ({}.hasOwnProperty.call(rec, key)) {
          copy2[key] = rec[key];
        }
      }
      copy2[label5] = value13;
      return copy2;
    };
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

// output/Control.Apply/index.js
var identity2 = /* @__PURE__ */ identity(categoryFn);
var apply = function(dict) {
  return dict.apply;
};
var applySecond = function(dictApply) {
  var apply1 = apply(dictApply);
  var map27 = map(dictApply.Functor0());
  return function(a2) {
    return function(b2) {
      return apply1(map27($$const(identity2))(a2))(b2);
    };
  };
};

// output/Control.Applicative/index.js
var pure = function(dict) {
  return dict.pure;
};
var unless = function(dictApplicative) {
  var pure14 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (!v) {
        return v1;
      }
      ;
      if (v) {
        return pure14(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 68, column 1 - line 68, column 65): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var when = function(dictApplicative) {
  var pure14 = pure(dictApplicative);
  return function(v) {
    return function(v1) {
      if (v) {
        return v1;
      }
      ;
      if (!v) {
        return pure14(unit);
      }
      ;
      throw new Error("Failed pattern match at Control.Applicative (line 63, column 1 - line 63, column 63): " + [v.constructor.name, v1.constructor.name]);
    };
  };
};
var liftA1 = function(dictApplicative) {
  var apply5 = apply(dictApplicative.Apply0());
  var pure14 = pure(dictApplicative);
  return function(f) {
    return function(a2) {
      return apply5(pure14(f))(a2);
    };
  };
};

// output/Data.Bounded/foreign.js
var topChar = String.fromCharCode(65535);
var bottomChar = String.fromCharCode(0);
var topNumber = Number.POSITIVE_INFINITY;
var bottomNumber = Number.NEGATIVE_INFINITY;

// output/Data.Ord/foreign.js
var unsafeCompareImpl = function(lt) {
  return function(eq3) {
    return function(gt) {
      return function(x) {
        return function(y) {
          return x < y ? lt : x === y ? eq3 : gt;
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
var eq = function(dict) {
  return dict.eq;
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
var showIntImpl = function(n) {
  return n.toString();
};
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
var showInt = {
  show: showIntImpl
};
var show = function(dict) {
  return dict.show;
};

// output/Data.Generic.Rep/index.js
var Argument = function(x) {
  return x;
};
var to = function(dict) {
  return dict.to;
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
var applicativeMaybe = /* @__PURE__ */ function() {
  return {
    pure: Just.create,
    Apply0: function() {
      return applyMaybe;
    }
  };
}();

// output/Data.MediaType.Common/index.js
var applicationJSON = "application/json";
var applicationFormURLEncoded = "application/x-www-form-urlencoded";

// output/Affjax.ResponseFormat/index.js
var identity4 = /* @__PURE__ */ identity(categoryFn);
var $$ArrayBuffer = /* @__PURE__ */ function() {
  function $$ArrayBuffer2(value0) {
    this.value0 = value0;
  }
  ;
  $$ArrayBuffer2.create = function(value0) {
    return new $$ArrayBuffer2(value0);
  };
  return $$ArrayBuffer2;
}();
var Blob = /* @__PURE__ */ function() {
  function Blob3(value0) {
    this.value0 = value0;
  }
  ;
  Blob3.create = function(value0) {
    return new Blob3(value0);
  };
  return Blob3;
}();
var Document = /* @__PURE__ */ function() {
  function Document3(value0) {
    this.value0 = value0;
  }
  ;
  Document3.create = function(value0) {
    return new Document3(value0);
  };
  return Document3;
}();
var Json = /* @__PURE__ */ function() {
  function Json3(value0) {
    this.value0 = value0;
  }
  ;
  Json3.create = function(value0) {
    return new Json3(value0);
  };
  return Json3;
}();
var $$String = /* @__PURE__ */ function() {
  function $$String3(value0) {
    this.value0 = value0;
  }
  ;
  $$String3.create = function(value0) {
    return new $$String3(value0);
  };
  return $$String3;
}();
var Ignore = /* @__PURE__ */ function() {
  function Ignore2(value0) {
    this.value0 = value0;
  }
  ;
  Ignore2.create = function(value0) {
    return new Ignore2(value0);
  };
  return Ignore2;
}();
var toResponseType = function(v) {
  if (v instanceof $$ArrayBuffer) {
    return "arraybuffer";
  }
  ;
  if (v instanceof Blob) {
    return "blob";
  }
  ;
  if (v instanceof Document) {
    return "document";
  }
  ;
  if (v instanceof Json) {
    return "text";
  }
  ;
  if (v instanceof $$String) {
    return "text";
  }
  ;
  if (v instanceof Ignore) {
    return "";
  }
  ;
  throw new Error("Failed pattern match at Affjax.ResponseFormat (line 44, column 3 - line 50, column 19): " + [v.constructor.name]);
};
var toMediaType = function(v) {
  if (v instanceof Json) {
    return new Just(applicationJSON);
  }
  ;
  return Nothing.value;
};
var json = /* @__PURE__ */ function() {
  return new Json(identity4);
}();
var ignore = /* @__PURE__ */ function() {
  return new Ignore(identity4);
}();

// output/Affjax.Web/foreign.js
var driver = {
  newXHR: function() {
    return new XMLHttpRequest();
  },
  fixupUrl: function(url) {
    return url || "/";
  }
};

// output/Affjax/foreign.js
function _ajax(platformSpecificDriver, timeoutErrorMessageIdent, requestFailedMessageIdent, mkHeader, options2) {
  return function(errback, callback) {
    var xhr = platformSpecificDriver.newXHR();
    var fixedUrl = platformSpecificDriver.fixupUrl(options2.url, xhr);
    xhr.open(options2.method || "GET", fixedUrl, true, options2.username, options2.password);
    if (options2.headers) {
      try {
        for (var i2 = 0, header2; (header2 = options2.headers[i2]) != null; i2++) {
          xhr.setRequestHeader(header2.field, header2.value);
        }
      } catch (e) {
        errback(e);
      }
    }
    var onerror = function(msgIdent) {
      return function() {
        errback(new Error(msgIdent));
      };
    };
    xhr.onerror = onerror(requestFailedMessageIdent);
    xhr.ontimeout = onerror(timeoutErrorMessageIdent);
    xhr.onload = function() {
      callback({
        status: xhr.status,
        statusText: xhr.statusText,
        headers: xhr.getAllResponseHeaders().split("\r\n").filter(function(header3) {
          return header3.length > 0;
        }).map(function(header3) {
          var i3 = header3.indexOf(":");
          return mkHeader(header3.substring(0, i3))(header3.substring(i3 + 2));
        }),
        body: xhr.response
      });
    };
    xhr.responseType = options2.responseType;
    xhr.withCredentials = options2.withCredentials;
    xhr.timeout = options2.timeout;
    xhr.send(options2.content);
    return function(error4, cancelErrback, cancelCallback) {
      try {
        xhr.abort();
      } catch (e) {
        return cancelErrback(e);
      }
      return cancelCallback();
    };
  };
}

// output/Affjax.RequestBody/index.js
var ArrayView = /* @__PURE__ */ function() {
  function ArrayView2(value0) {
    this.value0 = value0;
  }
  ;
  ArrayView2.create = function(value0) {
    return new ArrayView2(value0);
  };
  return ArrayView2;
}();
var Blob2 = /* @__PURE__ */ function() {
  function Blob3(value0) {
    this.value0 = value0;
  }
  ;
  Blob3.create = function(value0) {
    return new Blob3(value0);
  };
  return Blob3;
}();
var Document2 = /* @__PURE__ */ function() {
  function Document3(value0) {
    this.value0 = value0;
  }
  ;
  Document3.create = function(value0) {
    return new Document3(value0);
  };
  return Document3;
}();
var $$String2 = /* @__PURE__ */ function() {
  function $$String3(value0) {
    this.value0 = value0;
  }
  ;
  $$String3.create = function(value0) {
    return new $$String3(value0);
  };
  return $$String3;
}();
var FormData = /* @__PURE__ */ function() {
  function FormData2(value0) {
    this.value0 = value0;
  }
  ;
  FormData2.create = function(value0) {
    return new FormData2(value0);
  };
  return FormData2;
}();
var FormURLEncoded = /* @__PURE__ */ function() {
  function FormURLEncoded2(value0) {
    this.value0 = value0;
  }
  ;
  FormURLEncoded2.create = function(value0) {
    return new FormURLEncoded2(value0);
  };
  return FormURLEncoded2;
}();
var Json2 = /* @__PURE__ */ function() {
  function Json3(value0) {
    this.value0 = value0;
  }
  ;
  Json3.create = function(value0) {
    return new Json3(value0);
  };
  return Json3;
}();
var toMediaType2 = function(v) {
  if (v instanceof FormURLEncoded) {
    return new Just(applicationFormURLEncoded);
  }
  ;
  if (v instanceof Json2) {
    return new Just(applicationJSON);
  }
  ;
  return Nothing.value;
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
var alaF = function() {
  return function() {
    return function() {
      return function() {
        return function(v) {
          return coerce2;
        };
      };
    };
  };
};

// output/Affjax.RequestHeader/index.js
var unwrap2 = /* @__PURE__ */ unwrap();
var Accept = /* @__PURE__ */ function() {
  function Accept2(value0) {
    this.value0 = value0;
  }
  ;
  Accept2.create = function(value0) {
    return new Accept2(value0);
  };
  return Accept2;
}();
var ContentType = /* @__PURE__ */ function() {
  function ContentType2(value0) {
    this.value0 = value0;
  }
  ;
  ContentType2.create = function(value0) {
    return new ContentType2(value0);
  };
  return ContentType2;
}();
var RequestHeader = /* @__PURE__ */ function() {
  function RequestHeader2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  RequestHeader2.create = function(value0) {
    return function(value1) {
      return new RequestHeader2(value0, value1);
    };
  };
  return RequestHeader2;
}();
var value = function(v) {
  if (v instanceof Accept) {
    return unwrap2(v.value0);
  }
  ;
  if (v instanceof ContentType) {
    return unwrap2(v.value0);
  }
  ;
  if (v instanceof RequestHeader) {
    return v.value1;
  }
  ;
  throw new Error("Failed pattern match at Affjax.RequestHeader (line 26, column 1 - line 26, column 33): " + [v.constructor.name]);
};
var name = function(v) {
  if (v instanceof Accept) {
    return "Accept";
  }
  ;
  if (v instanceof ContentType) {
    return "Content-Type";
  }
  ;
  if (v instanceof RequestHeader) {
    return v.value0;
  }
  ;
  throw new Error("Failed pattern match at Affjax.RequestHeader (line 21, column 1 - line 21, column 32): " + [v.constructor.name]);
};

// output/Affjax.ResponseHeader/index.js
var ResponseHeader = /* @__PURE__ */ function() {
  function ResponseHeader2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ResponseHeader2.create = function(value0) {
    return function(value1) {
      return new ResponseHeader2(value0, value1);
    };
  };
  return ResponseHeader2;
}();

// output/Control.Bind/index.js
var discard = function(dict) {
  return dict.discard;
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
var composeKleisli = function(dictBind) {
  var bind16 = bind(dictBind);
  return function(f) {
    return function(g) {
      return function(a2) {
        return bind16(f(a2))(g);
      };
    };
  };
};
var discardUnit = {
  discard: function(dictBind) {
    return bind(dictBind);
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
var note = function(a2) {
  return maybe(new Left(a2))(Right.create);
};
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
var map3 = /* @__PURE__ */ map(functorEither);
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
var applyEither = {
  apply: function(v) {
    return function(v1) {
      if (v instanceof Left) {
        return new Left(v.value0);
      }
      ;
      if (v instanceof Right) {
        return map3(v.value0)(v1);
      }
      ;
      throw new Error("Failed pattern match at Data.Either (line 70, column 1 - line 72, column 30): " + [v.constructor.name, v1.constructor.name]);
    };
  },
  Functor0: function() {
    return functorEither;
  }
};
var bindEither = {
  bind: /* @__PURE__ */ either(function(e) {
    return function(v) {
      return new Left(e);
    };
  })(function(a2) {
    return function(f) {
      return f(a2);
    };
  }),
  Apply0: function() {
    return applyEither;
  }
};
var applicativeEither = /* @__PURE__ */ function() {
  return {
    pure: Right.create,
    Apply0: function() {
      return applyEither;
    }
  };
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
  var bind10 = bind(dictMonad.Bind1());
  var unless2 = unless(dictMonad.Applicative0());
  return function(mb) {
    return function(m) {
      return bind10(mb)(function(b2) {
        return unless2(b2)(m);
      });
    };
  };
};
var ap = function(dictMonad) {
  var bind10 = bind(dictMonad.Bind1());
  var pure11 = pure(dictMonad.Applicative0());
  return function(f) {
    return function(a2) {
      return bind10(f)(function(f$prime) {
        return bind10(a2)(function(a$prime) {
          return pure11(f$prime(a$prime));
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
var $runtime_lazy = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
function message(e) {
  return e.message;
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
  var map27 = map(Monad0.Bind1().Apply0().Functor0());
  var pure11 = pure(Monad0.Applicative0());
  return function(a2) {
    return catchError1(map27(Right.create)(a2))(function($52) {
      return pure11(Left.create($52));
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
var map4 = /* @__PURE__ */ map(functorEffect);
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
      return function __do2() {
        var r = bindFlipped2($$new)(f(a2))();
        (function() {
          while (!function __do3() {
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
        return map4(fromDone)(read(r))();
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
var map5 = /* @__PURE__ */ map(functorEither);
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
  var map111 = map(dictFunctor);
  return {
    map: function(f) {
      return mapExceptT(map111(map5(f)));
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
  var bind10 = bind(dictMonad.Bind1());
  var pure11 = pure(dictMonad.Applicative0());
  return {
    bind: function(v) {
      return function(k) {
        return bind10(v)(either(function($187) {
          return pure11(Left.create($187));
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
    var bind10 = bind(Bind1);
    var pure11 = pure(dictMonad.Applicative0());
    var functorExceptT1 = functorExceptT(Bind1.Apply0().Functor0());
    return {
      alt: function(v) {
        return function(v1) {
          return bind10(v)(function(rm) {
            if (rm instanceof Right) {
              return pure11(new Right(rm.value0));
            }
            ;
            if (rm instanceof Left) {
              return bind10(v1)(function(rn) {
                if (rn instanceof Right) {
                  return pure11(new Right(rn.value0));
                }
                ;
                if (rn instanceof Left) {
                  return pure11(new Left(append5(rm.value0)(rn.value0)));
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

// output/Control.Monad.Except/index.js
var unwrap3 = /* @__PURE__ */ unwrap();
var runExcept = function($3) {
  return unwrap3(runExceptT($3));
};

// output/Data.Argonaut.Core/foreign.js
function id(x) {
  return x;
}
var jsonNull = null;
function stringify(j) {
  return JSON.stringify(j);
}
function _caseJson(isNull3, isBool, isNum, isStr, isArr, isObj, j) {
  if (j == null)
    return isNull3();
  else if (typeof j === "boolean")
    return isBool(j);
  else if (typeof j === "number")
    return isNum(j);
  else if (typeof j === "string")
    return isStr(j);
  else if (Object.prototype.toString.call(j) === "[object Array]")
    return isArr(j);
  else
    return isObj(j);
}

// output/Foreign.Object/foreign.js
function _copyST(m) {
  return function() {
    var r = {};
    for (var k in m) {
      if (hasOwnProperty.call(m, k)) {
        r[k] = m[k];
      }
    }
    return r;
  };
}
var empty = {};
function runST(f) {
  return f();
}
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
var length = function(xs) {
  return xs.length;
};
var unconsImpl = function(empty7, next, xs) {
  return xs.length === 0 ? empty7({}) : next(xs[0])(xs.slice(1));
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

// output/Data.Array.ST/foreign.js
function unsafeFreezeThawImpl(xs) {
  return xs;
}
var unsafeFreezeImpl = unsafeFreezeThawImpl;
function copyImpl(xs) {
  return xs.slice();
}
var thawImpl = copyImpl;
var pushImpl = function(a2, xs) {
  return xs.push(a2);
};

// output/Control.Monad.ST.Uncurried/foreign.js
var runSTFn1 = function runSTFn12(fn) {
  return function(a2) {
    return function() {
      return fn(a2);
    };
  };
};
var runSTFn2 = function runSTFn22(fn) {
  return function(a2) {
    return function(b2) {
      return function() {
        return fn(a2, b2);
      };
    };
  };
};

// output/Data.Array.ST/index.js
var unsafeFreeze = /* @__PURE__ */ runSTFn1(unsafeFreezeImpl);
var thaw = /* @__PURE__ */ runSTFn1(thawImpl);
var withArray = function(f) {
  return function(xs) {
    return function __do2() {
      var result = thaw(xs)();
      f(result)();
      return unsafeFreeze(result)();
    };
  };
};
var push = /* @__PURE__ */ runSTFn2(pushImpl);

// output/Data.Foldable/foreign.js
var foldrArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i2 = len - 1; i2 >= 0; i2--) {
        acc = f(xs[i2])(acc);
      }
      return acc;
    };
  };
};
var foldlArray = function(f) {
  return function(init3) {
    return function(xs) {
      var acc = init3;
      var len = xs.length;
      for (var i2 = 0; i2 < len; i2++) {
        acc = f(acc)(xs[i2]);
      }
      return acc;
    };
  };
};

// output/Control.Plus/index.js
var empty2 = function(dict) {
  return dict.empty;
};

// output/Data.Bifunctor/index.js
var identity5 = /* @__PURE__ */ identity(categoryFn);
var bimap = function(dict) {
  return dict.bimap;
};
var lmap = function(dictBifunctor) {
  var bimap1 = bimap(dictBifunctor);
  return function(f) {
    return bimap1(f)(identity5);
  };
};
var bifunctorEither = {
  bimap: function(v) {
    return function(v1) {
      return function(v2) {
        if (v2 instanceof Left) {
          return new Left(v(v2.value0));
        }
        ;
        if (v2 instanceof Right) {
          return new Right(v1(v2.value0));
        }
        ;
        throw new Error("Failed pattern match at Data.Bifunctor (line 32, column 1 - line 34, column 36): " + [v.constructor.name, v1.constructor.name, v2.constructor.name]);
      };
    };
  }
};

// output/Data.Monoid.Disj/index.js
var Disj = function(x) {
  return x;
};
var semigroupDisj = function(dictHeytingAlgebra) {
  var disj2 = disj(dictHeytingAlgebra);
  return {
    append: function(v) {
      return function(v1) {
        return disj2(v)(v1);
      };
    }
  };
};
var monoidDisj = function(dictHeytingAlgebra) {
  var semigroupDisj1 = semigroupDisj(dictHeytingAlgebra);
  return {
    mempty: ff(dictHeytingAlgebra),
    Semigroup0: function() {
      return semigroupDisj1;
    }
  };
};

// output/Data.Foldable/index.js
var alaF2 = /* @__PURE__ */ alaF()()()();
var foldr = function(dict) {
  return dict.foldr;
};
var traverse_ = function(dictApplicative) {
  var applySecond2 = applySecond(dictApplicative.Apply0());
  var pure11 = pure(dictApplicative);
  return function(dictFoldable) {
    var foldr22 = foldr(dictFoldable);
    return function(f) {
      return foldr22(function($454) {
        return applySecond2(f($454));
      })(pure11(unit));
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
    var mempty2 = mempty(dictMonoid);
    return function(v) {
      return function(v1) {
        if (v1 instanceof Nothing) {
          return mempty2;
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
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldr22(function(x) {
        return function(acc) {
          return append5(f(x))(acc);
        };
      })(mempty2);
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
var foldMap = function(dict) {
  return dict.foldMap;
};
var any = function(dictFoldable) {
  var foldMap2 = foldMap(dictFoldable);
  return function(dictHeytingAlgebra) {
    return alaF2(Disj)(foldMap2(monoidDisj(dictHeytingAlgebra)));
  };
};

// output/Data.Function.Uncurried/foreign.js
var runFn3 = function(fn) {
  return function(a2) {
    return function(b2) {
      return function(c) {
        return fn(a2, b2, c);
      };
    };
  };
};
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

// output/Data.FunctorWithIndex/foreign.js
var mapWithIndexArray = function(f) {
  return function(xs) {
    var l = xs.length;
    var result = Array(l);
    for (var i2 = 0; i2 < l; i2++) {
      result[i2] = f(i2)(xs[i2]);
    }
    return result;
  };
};

// output/Data.FunctorWithIndex/index.js
var mapWithIndex = function(dict) {
  return dict.mapWithIndex;
};
var functorWithIndexArray = {
  mapWithIndex: mapWithIndexArray,
  Functor0: function() {
    return functorArray;
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
  return function(apply5) {
    return function(map27) {
      return function(pure11) {
        return function(f) {
          return function(array) {
            function go2(bot, top2) {
              switch (top2 - bot) {
                case 0:
                  return pure11([]);
                case 1:
                  return map27(array1)(f(array[bot]));
                case 2:
                  return apply5(map27(array2)(f(array[bot])))(f(array[bot + 1]));
                case 3:
                  return apply5(apply5(map27(array3)(f(array[bot])))(f(array[bot + 1])))(f(array[bot + 2]));
                default:
                  var pivot = bot + Math.floor((top2 - bot) / 4) * 2;
                  return apply5(map27(concat2)(go2(bot, pivot)))(go2(pivot, top2));
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
var identity6 = /* @__PURE__ */ identity(categoryFn);
var traverse = function(dict) {
  return dict.traverse;
};
var sequenceDefault = function(dictTraversable) {
  var traverse22 = traverse(dictTraversable);
  return function(dictApplicative) {
    return traverse22(dictApplicative)(identity6);
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
var sequence = function(dict) {
  return dict.sequence;
};

// output/Data.Array/index.js
var fromJust2 = /* @__PURE__ */ fromJust();
var uncons = /* @__PURE__ */ function() {
  return runFn3(unconsImpl)($$const(Nothing.value))(function(x) {
    return function(xs) {
      return new Just({
        head: x,
        tail: xs
      });
    };
  });
}();
var snoc = function(xs) {
  return function(x) {
    return withArray(push(x))(xs)();
  };
};
var singleton2 = function(a2) {
  return [a2];
};
var findIndex = /* @__PURE__ */ function() {
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
      })(findIndex(v(v1))(v2));
    };
  };
};

// output/Data.FoldableWithIndex/index.js
var foldr8 = /* @__PURE__ */ foldr(foldableArray);
var mapWithIndex2 = /* @__PURE__ */ mapWithIndex(functorWithIndexArray);
var foldl8 = /* @__PURE__ */ foldl(foldableArray);
var foldrWithIndex = function(dict) {
  return dict.foldrWithIndex;
};
var foldMapWithIndexDefaultR = function(dictFoldableWithIndex) {
  var foldrWithIndex1 = foldrWithIndex(dictFoldableWithIndex);
  return function(dictMonoid) {
    var append5 = append(dictMonoid.Semigroup0());
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldrWithIndex1(function(i2) {
        return function(x) {
          return function(acc) {
            return append5(f(i2)(x))(acc);
          };
        };
      })(mempty2);
    };
  };
};
var foldableWithIndexArray = {
  foldrWithIndex: function(f) {
    return function(z) {
      var $291 = foldr8(function(v) {
        return function(y) {
          return f(v.value0)(v.value1)(y);
        };
      })(z);
      var $292 = mapWithIndex2(Tuple.create);
      return function($293) {
        return $291($292($293));
      };
    };
  },
  foldlWithIndex: function(f) {
    return function(z) {
      var $294 = foldl8(function(y) {
        return function(v) {
          return f(v.value0)(y)(v.value1);
        };
      })(z);
      var $295 = mapWithIndex2(Tuple.create);
      return function($296) {
        return $294($295($296));
      };
    };
  },
  foldMapWithIndex: function(dictMonoid) {
    return foldMapWithIndexDefaultR(foldableWithIndexArray)(dictMonoid);
  },
  Foldable0: function() {
    return foldableArray;
  }
};

// output/Data.TraversableWithIndex/index.js
var traverseWithIndexDefault = function(dictTraversableWithIndex) {
  var sequence2 = sequence(dictTraversableWithIndex.Traversable2());
  var mapWithIndex4 = mapWithIndex(dictTraversableWithIndex.FunctorWithIndex0());
  return function(dictApplicative) {
    var sequence12 = sequence2(dictApplicative);
    return function(f) {
      var $174 = mapWithIndex4(f);
      return function($175) {
        return sequence12($174($175));
      };
    };
  };
};
var traverseWithIndex = function(dict) {
  return dict.traverseWithIndex;
};
var traversableWithIndexArray = {
  traverseWithIndex: function(dictApplicative) {
    return traverseWithIndexDefault(traversableWithIndexArray)(dictApplicative);
  },
  FunctorWithIndex0: function() {
    return functorWithIndexArray;
  },
  FoldableWithIndex1: function() {
    return foldableWithIndexArray;
  },
  Traversable2: function() {
    return traversableArray;
  }
};

// output/Foreign.Object.ST/foreign.js
var newImpl = function() {
  return {};
};
var deleteImpl = function(k) {
  return function(m) {
    return function() {
      delete m[k];
      return m;
    };
  };
};

// output/Foreign.Object/index.js
var thawST = _copyST;
var mutate = function(f) {
  return function(m) {
    return runST(function __do2() {
      var s = thawST(m)();
      f(s)();
      return s;
    });
  };
};
var lookup = /* @__PURE__ */ function() {
  return runFn4(_lookup)(Nothing.value)(Just.create);
}();
var $$delete = function(k) {
  return mutate(deleteImpl(k));
};

// output/Data.Argonaut.Core/index.js
var verbJsonType = function(def) {
  return function(f) {
    return function(g) {
      return g(def)(f);
    };
  };
};
var toJsonType = /* @__PURE__ */ function() {
  return verbJsonType(Nothing.value)(Just.create);
}();
var jsonEmptyObject = /* @__PURE__ */ id(empty);
var caseJsonString = function(d) {
  return function(f) {
    return function(j) {
      return _caseJson($$const(d), $$const(d), $$const(d), f, $$const(d), $$const(d), j);
    };
  };
};
var toString = /* @__PURE__ */ toJsonType(caseJsonString);
var caseJsonObject = function(d) {
  return function(f) {
    return function(j) {
      return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), $$const(d), f, j);
    };
  };
};
var toObject = /* @__PURE__ */ toJsonType(caseJsonObject);
var caseJsonNumber = function(d) {
  return function(f) {
    return function(j) {
      return _caseJson($$const(d), $$const(d), f, $$const(d), $$const(d), $$const(d), j);
    };
  };
};
var caseJsonArray = function(d) {
  return function(f) {
    return function(j) {
      return _caseJson($$const(d), $$const(d), $$const(d), $$const(d), f, $$const(d), j);
    };
  };
};
var toArray = /* @__PURE__ */ toJsonType(caseJsonArray);
var caseJson = function(a2) {
  return function(b2) {
    return function(c) {
      return function(d) {
        return function(e) {
          return function(f) {
            return function(json2) {
              return _caseJson(a2, b2, c, d, e, f, json2);
            };
          };
        };
      };
    };
  };
};

// output/Data.Argonaut.Parser/foreign.js
function _jsonParser(fail4, succ2, s) {
  try {
    return succ2(JSON.parse(s));
  } catch (e) {
    return fail4(e.message);
  }
}

// output/Data.Argonaut.Parser/index.js
var jsonParser = function(j) {
  return _jsonParser(Left.create, Right.create, j);
};

// output/Data.String.Common/foreign.js
var joinWith = function(s) {
  return function(xs) {
    return xs.join(s);
  };
};

// output/JSURI/foreign.js
function encodeURIComponent_to_RFC3986(input3) {
  return input3.replace(/[!'()*]/g, function(c) {
    return "%" + c.charCodeAt(0).toString(16);
  });
}
function _encodeFormURLComponent(fail4, succeed, input3) {
  try {
    return succeed(encodeURIComponent_to_RFC3986(encodeURIComponent(input3)).replace(/%20/g, "+"));
  } catch (err) {
    return fail4(err);
  }
}

// output/JSURI/index.js
var encodeFormURLComponent = /* @__PURE__ */ function() {
  return runFn3(_encodeFormURLComponent)($$const(Nothing.value))(Just.create);
}();

// output/Data.FormURLEncoded/index.js
var apply2 = /* @__PURE__ */ apply(applyMaybe);
var map6 = /* @__PURE__ */ map(functorMaybe);
var traverse2 = /* @__PURE__ */ traverse(traversableArray)(applicativeMaybe);
var toArray2 = function(v) {
  return v;
};
var encode = /* @__PURE__ */ function() {
  var encodePart = function(v) {
    if (v.value1 instanceof Nothing) {
      return encodeFormURLComponent(v.value0);
    }
    ;
    if (v.value1 instanceof Just) {
      return apply2(map6(function(key) {
        return function(val) {
          return key + ("=" + val);
        };
      })(encodeFormURLComponent(v.value0)))(encodeFormURLComponent(v.value1.value0));
    }
    ;
    throw new Error("Failed pattern match at Data.FormURLEncoded (line 37, column 16 - line 39, column 114): " + [v.constructor.name]);
  };
  var $37 = map6(joinWith("&"));
  var $38 = traverse2(encodePart);
  return function($39) {
    return $37($38(toArray2($39)));
  };
}();

// output/Data.HTTP.Method/index.js
var OPTIONS = /* @__PURE__ */ function() {
  function OPTIONS2() {
  }
  ;
  OPTIONS2.value = new OPTIONS2();
  return OPTIONS2;
}();
var GET = /* @__PURE__ */ function() {
  function GET3() {
  }
  ;
  GET3.value = new GET3();
  return GET3;
}();
var HEAD = /* @__PURE__ */ function() {
  function HEAD2() {
  }
  ;
  HEAD2.value = new HEAD2();
  return HEAD2;
}();
var POST = /* @__PURE__ */ function() {
  function POST3() {
  }
  ;
  POST3.value = new POST3();
  return POST3;
}();
var PUT = /* @__PURE__ */ function() {
  function PUT2() {
  }
  ;
  PUT2.value = new PUT2();
  return PUT2;
}();
var DELETE = /* @__PURE__ */ function() {
  function DELETE2() {
  }
  ;
  DELETE2.value = new DELETE2();
  return DELETE2;
}();
var TRACE = /* @__PURE__ */ function() {
  function TRACE2() {
  }
  ;
  TRACE2.value = new TRACE2();
  return TRACE2;
}();
var CONNECT = /* @__PURE__ */ function() {
  function CONNECT2() {
  }
  ;
  CONNECT2.value = new CONNECT2();
  return CONNECT2;
}();
var PROPFIND = /* @__PURE__ */ function() {
  function PROPFIND2() {
  }
  ;
  PROPFIND2.value = new PROPFIND2();
  return PROPFIND2;
}();
var PROPPATCH = /* @__PURE__ */ function() {
  function PROPPATCH2() {
  }
  ;
  PROPPATCH2.value = new PROPPATCH2();
  return PROPPATCH2;
}();
var MKCOL = /* @__PURE__ */ function() {
  function MKCOL2() {
  }
  ;
  MKCOL2.value = new MKCOL2();
  return MKCOL2;
}();
var COPY = /* @__PURE__ */ function() {
  function COPY2() {
  }
  ;
  COPY2.value = new COPY2();
  return COPY2;
}();
var MOVE = /* @__PURE__ */ function() {
  function MOVE2() {
  }
  ;
  MOVE2.value = new MOVE2();
  return MOVE2;
}();
var LOCK = /* @__PURE__ */ function() {
  function LOCK2() {
  }
  ;
  LOCK2.value = new LOCK2();
  return LOCK2;
}();
var UNLOCK = /* @__PURE__ */ function() {
  function UNLOCK2() {
  }
  ;
  UNLOCK2.value = new UNLOCK2();
  return UNLOCK2;
}();
var PATCH = /* @__PURE__ */ function() {
  function PATCH2() {
  }
  ;
  PATCH2.value = new PATCH2();
  return PATCH2;
}();
var unCustomMethod = function(v) {
  return v;
};
var showMethod = {
  show: function(v) {
    if (v instanceof OPTIONS) {
      return "OPTIONS";
    }
    ;
    if (v instanceof GET) {
      return "GET";
    }
    ;
    if (v instanceof HEAD) {
      return "HEAD";
    }
    ;
    if (v instanceof POST) {
      return "POST";
    }
    ;
    if (v instanceof PUT) {
      return "PUT";
    }
    ;
    if (v instanceof DELETE) {
      return "DELETE";
    }
    ;
    if (v instanceof TRACE) {
      return "TRACE";
    }
    ;
    if (v instanceof CONNECT) {
      return "CONNECT";
    }
    ;
    if (v instanceof PROPFIND) {
      return "PROPFIND";
    }
    ;
    if (v instanceof PROPPATCH) {
      return "PROPPATCH";
    }
    ;
    if (v instanceof MKCOL) {
      return "MKCOL";
    }
    ;
    if (v instanceof COPY) {
      return "COPY";
    }
    ;
    if (v instanceof MOVE) {
      return "MOVE";
    }
    ;
    if (v instanceof LOCK) {
      return "LOCK";
    }
    ;
    if (v instanceof UNLOCK) {
      return "UNLOCK";
    }
    ;
    if (v instanceof PATCH) {
      return "PATCH";
    }
    ;
    throw new Error("Failed pattern match at Data.HTTP.Method (line 43, column 1 - line 59, column 23): " + [v.constructor.name]);
  }
};
var print = /* @__PURE__ */ either(/* @__PURE__ */ show(showMethod))(unCustomMethod);

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
var singleton4 = function(dictPlus) {
  var empty7 = empty2(dictPlus);
  return function(a2) {
    return new NonEmpty(a2, empty7);
  };
};

// output/Data.List.Types/index.js
var Nil = /* @__PURE__ */ function() {
  function Nil3() {
  }
  ;
  Nil3.value = new Nil3();
  return Nil3;
}();
var Cons = /* @__PURE__ */ function() {
  function Cons3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Cons3.create = function(value0) {
    return function(value1) {
      return new Cons3(value0, value1);
    };
  };
  return Cons3;
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
    var mempty2 = mempty(dictMonoid);
    return function(f) {
      return foldl(foldableList)(function(acc) {
        var $286 = append22(acc);
        return function($287) {
          return $286(f($287));
        };
      })(mempty2);
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

// output/Data.List/index.js
var reverse2 = /* @__PURE__ */ function() {
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

// output/Data.List.NonEmpty/index.js
var singleton5 = /* @__PURE__ */ function() {
  var $200 = singleton4(plusList);
  return function($201) {
    return NonEmptyList($200($201));
  };
}();
var head = function(v) {
  return v.value0;
};
var cons = function(y) {
  return function(v) {
    return new NonEmpty(y, new Cons(v.value0, v.value1));
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
  function runSync(left2, right2, eff) {
    try {
      return right2(eff());
    } catch (error4) {
      return left2(error4);
    }
  }
  function runAsync(left2, eff, k) {
    try {
      return eff(k)();
    } catch (error4) {
      k(left2(error4))();
      return nonCanceler2;
    }
  }
  var Scheduler = function() {
    var limit = 1024;
    var size5 = 0;
    var ix = 0;
    var queue = new Array(limit);
    var draining = false;
    function drain() {
      var thunk;
      draining = true;
      while (size5 !== 0) {
        size5--;
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
        if (size5 === limit) {
          tmp = draining;
          drain();
          draining = tmp;
        }
        queue[(ix + size5) % limit] = cb;
        size5++;
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
    var step4 = aff;
    var fail4 = null;
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
              step4 = bhead(step4);
              if (btail === null) {
                bhead = null;
              } else {
                bhead = btail._1;
                btail = btail._2;
              }
            } catch (e) {
              status = RETURN;
              fail4 = util.left(e);
              step4 = null;
            }
            break;
          case STEP_RESULT:
            if (util.isLeft(step4)) {
              status = RETURN;
              fail4 = step4;
              step4 = null;
            } else if (bhead === null) {
              status = RETURN;
            } else {
              status = STEP_BIND;
              step4 = util.fromRight(step4);
            }
            break;
          case CONTINUE:
            switch (step4.tag) {
              case BIND:
                if (bhead) {
                  btail = new Aff2(CONS, bhead, btail);
                }
                bhead = step4._2;
                status = CONTINUE;
                step4 = step4._1;
                break;
              case PURE:
                if (bhead === null) {
                  status = RETURN;
                  step4 = util.right(step4._1);
                } else {
                  status = STEP_BIND;
                  step4 = step4._1;
                }
                break;
              case SYNC:
                status = STEP_RESULT;
                step4 = runSync(util.left, util.right, step4._1);
                break;
              case ASYNC:
                status = PENDING;
                step4 = runAsync(util.left, step4._1, function(result2) {
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
                      step4 = result2;
                      run3(runTick);
                    });
                  };
                });
                return;
              case THROW:
                status = RETURN;
                fail4 = util.left(step4._1);
                step4 = null;
                break;
              case CATCH:
                if (bhead === null) {
                  attempts = new Aff2(CONS, step4, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status = CONTINUE;
                step4 = step4._1;
                break;
              case BRACKET:
                bracketCount++;
                if (bhead === null) {
                  attempts = new Aff2(CONS, step4, attempts, interrupt);
                } else {
                  attempts = new Aff2(CONS, step4, new Aff2(CONS, new Aff2(RESUME, bhead, btail), attempts, interrupt), interrupt);
                }
                bhead = null;
                btail = null;
                status = CONTINUE;
                step4 = step4._1;
                break;
              case FORK:
                status = STEP_RESULT;
                tmp = Fiber(util, supervisor, step4._2);
                if (supervisor) {
                  supervisor.register(tmp);
                }
                if (step4._1) {
                  tmp.run();
                }
                step4 = util.right(tmp);
                break;
              case SEQ:
                status = CONTINUE;
                step4 = sequential3(util, supervisor, step4._1);
                break;
            }
            break;
          case RETURN:
            bhead = null;
            btail = null;
            if (attempts === null) {
              status = COMPLETED;
              step4 = interrupt || fail4 || step4;
            } else {
              tmp = attempts._3;
              attempt = attempts._1;
              attempts = attempts._2;
              switch (attempt.tag) {
                case CATCH:
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    status = RETURN;
                  } else if (fail4) {
                    status = CONTINUE;
                    step4 = attempt._2(util.fromLeft(fail4));
                    fail4 = null;
                  }
                  break;
                case RESUME:
                  if (interrupt && interrupt !== tmp && bracketCount === 0 || fail4) {
                    status = RETURN;
                  } else {
                    bhead = attempt._1;
                    btail = attempt._2;
                    status = STEP_BIND;
                    step4 = util.fromRight(step4);
                  }
                  break;
                case BRACKET:
                  bracketCount--;
                  if (fail4 === null) {
                    result = util.fromRight(step4);
                    attempts = new Aff2(CONS, new Aff2(RELEASE, attempt._2, result), attempts, tmp);
                    if (interrupt === tmp || bracketCount > 0) {
                      status = CONTINUE;
                      step4 = attempt._3(result);
                    }
                  }
                  break;
                case RELEASE:
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail4), attempts, interrupt);
                  status = CONTINUE;
                  if (interrupt && interrupt !== tmp && bracketCount === 0) {
                    step4 = attempt._1.killed(util.fromLeft(interrupt))(attempt._2);
                  } else if (fail4) {
                    step4 = attempt._1.failed(util.fromLeft(fail4))(attempt._2);
                  } else {
                    step4 = attempt._1.completed(util.fromRight(step4))(attempt._2);
                  }
                  fail4 = null;
                  bracketCount++;
                  break;
                case FINALIZER:
                  bracketCount++;
                  attempts = new Aff2(CONS, new Aff2(FINALIZED, step4, fail4), attempts, interrupt);
                  status = CONTINUE;
                  step4 = attempt._1;
                  break;
                case FINALIZED:
                  bracketCount--;
                  status = RETURN;
                  step4 = attempt._1;
                  fail4 = attempt._2;
                  break;
              }
            }
            break;
          case COMPLETED:
            for (var k in joins) {
              if (joins.hasOwnProperty(k)) {
                rethrow = rethrow && joins[k].rethrow;
                runEff(joins[k].handler(step4));
              }
            }
            joins = null;
            if (interrupt && fail4) {
              setTimeout(function() {
                throw util.fromLeft(fail4);
              }, 0);
            } else if (util.isLeft(step4) && rethrow) {
              setTimeout(function() {
                if (rethrow) {
                  throw util.fromLeft(step4);
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
          join4.handler(step4)();
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
            step4 = interrupt;
            run3(runTick);
            break;
          case PENDING:
            if (interrupt === null) {
              interrupt = util.left(error4);
            }
            if (bracketCount === 0) {
              if (status === PENDING) {
                attempts = new Aff2(CONS, new Aff2(FINALIZER, step4(error4)), attempts, interrupt);
              }
              status = RETURN;
              step4 = null;
              fail4 = null;
              run3(++runTick);
            }
            break;
          default:
            if (interrupt === null) {
              interrupt = util.left(error4);
            }
            if (bracketCount === 0) {
              status = RETURN;
              step4 = null;
              fail4 = null;
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
      var step4 = par2;
      var head6 = null;
      var tail2 = null;
      var count = 0;
      var kills2 = {};
      var tmp, kid;
      loop:
        while (true) {
          tmp = null;
          switch (step4.tag) {
            case FORKED:
              if (step4._3 === EMPTY) {
                tmp = fibers[step4._1];
                kills2[count++] = tmp.kill(error4, function(result) {
                  return function() {
                    count--;
                    if (count === 0) {
                      cb2(result)();
                    }
                  };
                });
              }
              if (head6 === null) {
                break loop;
              }
              step4 = head6._2;
              if (tail2 === null) {
                head6 = null;
              } else {
                head6 = tail2._1;
                tail2 = tail2._2;
              }
              break;
            case MAP:
              step4 = step4._2;
              break;
            case APPLY:
            case ALT:
              if (head6) {
                tail2 = new Aff2(CONS, head6, tail2);
              }
              head6 = step4;
              step4 = step4._1;
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
    function join3(result, head6, tail2) {
      var fail4, step4, lhs, rhs, tmp, kid;
      if (util.isLeft(result)) {
        fail4 = result;
        step4 = null;
      } else {
        step4 = result;
        fail4 = null;
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
          if (head6 === null) {
            cb(fail4 || step4)();
            return;
          }
          if (head6._3 !== EMPTY) {
            return;
          }
          switch (head6.tag) {
            case MAP:
              if (fail4 === null) {
                head6._3 = util.right(head6._1(util.fromRight(step4)));
                step4 = head6._3;
              } else {
                head6._3 = fail4;
              }
              break;
            case APPLY:
              lhs = head6._1._3;
              rhs = head6._2._3;
              if (fail4) {
                head6._3 = fail4;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, fail4 === lhs ? head6._2 : head6._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(fail4, null, null);
                    } else {
                      join3(fail4, tail2._1, tail2._2);
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
                step4 = util.right(util.fromRight(lhs)(util.fromRight(rhs)));
                head6._3 = step4;
              }
              break;
            case ALT:
              lhs = head6._1._3;
              rhs = head6._2._3;
              if (lhs === EMPTY && util.isLeft(rhs) || rhs === EMPTY && util.isLeft(lhs)) {
                return;
              }
              if (lhs !== EMPTY && util.isLeft(lhs) && rhs !== EMPTY && util.isLeft(rhs)) {
                fail4 = step4 === lhs ? rhs : lhs;
                step4 = null;
                head6._3 = fail4;
              } else {
                head6._3 = step4;
                tmp = true;
                kid = killId++;
                kills[kid] = kill2(early, step4 === lhs ? head6._2 : head6._1, function() {
                  return function() {
                    delete kills[kid];
                    if (tmp) {
                      tmp = false;
                    } else if (tail2 === null) {
                      join3(step4, null, null);
                    } else {
                      join3(step4, tail2._1, tail2._2);
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
          if (tail2 === null) {
            head6 = null;
          } else {
            head6 = tail2._1;
            tail2 = tail2._2;
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
      var step4 = par;
      var head6 = null;
      var tail2 = null;
      var tmp, fid;
      loop:
        while (true) {
          tmp = null;
          fid = null;
          switch (status) {
            case CONTINUE:
              switch (step4.tag) {
                case MAP:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(MAP, step4._1, EMPTY, EMPTY);
                  step4 = step4._2;
                  break;
                case APPLY:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(APPLY, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                case ALT:
                  if (head6) {
                    tail2 = new Aff2(CONS, head6, tail2);
                  }
                  head6 = new Aff2(ALT, EMPTY, step4._2, EMPTY);
                  step4 = step4._1;
                  break;
                default:
                  fid = fiberId++;
                  status = RETURN;
                  tmp = step4;
                  step4 = new Aff2(FORKED, fid, new Aff2(CONS, head6, tail2), EMPTY);
                  tmp = Fiber(util, supervisor, tmp);
                  tmp.onComplete({
                    rethrow: false,
                    handler: resolve(step4)
                  })();
                  fibers[fid] = tmp;
                  if (supervisor) {
                    supervisor.register(tmp);
                  }
              }
              break;
            case RETURN:
              if (head6 === null) {
                break loop;
              }
              if (head6._1 === EMPTY) {
                head6._1 = step4;
                status = CONTINUE;
                step4 = head6._2;
                head6._2 = EMPTY;
              } else {
                head6._2 = step4;
                step4 = head6;
                if (tail2 === null) {
                  head6 = null;
                } else {
                  head6 = tail2._1;
                  tail2 = tail2._2;
                }
              }
          }
        }
      root = step4;
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
var identity7 = /* @__PURE__ */ identity(categoryFn);
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
      return parTraverse_2(dictFoldable)(identity7);
    };
  };
};

// output/Effect.Unsafe/foreign.js
var unsafePerformEffect = function(f) {
  return f();
};

// output/Effect.Aff/index.js
var $runtime_lazy2 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
    state3 = 2;
    return val;
  };
};
var pure2 = /* @__PURE__ */ pure(applicativeEffect);
var $$void3 = /* @__PURE__ */ $$void(functorEffect);
var map7 = /* @__PURE__ */ map(functorEffect);
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
  return function __do2() {
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
    return map7(effectCanceler)(v.join(k));
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
        return map7(effectCanceler)(v.kill(e, k));
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

// output/Effect.Aff.Compat/index.js
var fromEffectFnAff = function(v) {
  return makeAff(function(k) {
    return function __do2() {
      var v1 = v(function($9) {
        return k(Left.create($9))();
      }, function($10) {
        return k(Right.create($10))();
      });
      return function(e) {
        return makeAff(function(k2) {
          return function __do3() {
            v1(e, function($11) {
              return k2(Left.create($11))();
            }, function($12) {
              return k2(Right.create($12))();
            });
            return nonCanceler;
          };
        });
      };
    };
  });
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

// output/Data.Int/foreign.js
var fromNumberImpl = function(just) {
  return function(nothing) {
    return function(n) {
      return (n | 0) === n ? just(n) : nothing;
    };
  };
};

// output/Data.Int/index.js
var fromNumber = /* @__PURE__ */ function() {
  return fromNumberImpl(Just.create)(Nothing.value);
}();

// output/Foreign/index.js
var ForeignError = /* @__PURE__ */ function() {
  function ForeignError2(value0) {
    this.value0 = value0;
  }
  ;
  ForeignError2.create = function(value0) {
    return new ForeignError2(value0);
  };
  return ForeignError2;
}();
var TypeMismatch = /* @__PURE__ */ function() {
  function TypeMismatch3(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  TypeMismatch3.create = function(value0) {
    return function(value1) {
      return new TypeMismatch3(value0, value1);
    };
  };
  return TypeMismatch3;
}();
var unsafeToForeign = unsafeCoerce2;
var unsafeFromForeign = unsafeCoerce2;
var fail = function(dictMonad) {
  var $153 = throwError(monadThrowExceptT(dictMonad));
  return function($154) {
    return $153(singleton5($154));
  };
};
var unsafeReadTagged = function(dictMonad) {
  var pure14 = pure(applicativeExceptT(dictMonad));
  var fail1 = fail(dictMonad);
  return function(tag) {
    return function(value13) {
      if (tagOf(value13) === tag) {
        return pure14(unsafeFromForeign(value13));
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

// output/Affjax/index.js
var pure3 = /* @__PURE__ */ pure(/* @__PURE__ */ applicativeExceptT(monadIdentity));
var fail2 = /* @__PURE__ */ fail(monadIdentity);
var unsafeReadTagged2 = /* @__PURE__ */ unsafeReadTagged(monadIdentity);
var alt2 = /* @__PURE__ */ alt(/* @__PURE__ */ altExceptT(semigroupNonEmptyList)(monadIdentity));
var composeKleisliFlipped2 = /* @__PURE__ */ composeKleisliFlipped(/* @__PURE__ */ bindExceptT(monadIdentity));
var map8 = /* @__PURE__ */ map(functorMaybe);
var any2 = /* @__PURE__ */ any(foldableArray)(heytingAlgebraBoolean);
var eq2 = /* @__PURE__ */ eq(eqString);
var bindFlipped4 = /* @__PURE__ */ bindFlipped(bindMaybe);
var map12 = /* @__PURE__ */ map(functorArray);
var mapFlipped2 = /* @__PURE__ */ mapFlipped(functorAff);
var $$try3 = /* @__PURE__ */ $$try(monadErrorAff);
var pure1 = /* @__PURE__ */ pure(applicativeAff);
var RequestContentError = /* @__PURE__ */ function() {
  function RequestContentError2(value0) {
    this.value0 = value0;
  }
  ;
  RequestContentError2.create = function(value0) {
    return new RequestContentError2(value0);
  };
  return RequestContentError2;
}();
var ResponseBodyError = /* @__PURE__ */ function() {
  function ResponseBodyError2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  ResponseBodyError2.create = function(value0) {
    return function(value1) {
      return new ResponseBodyError2(value0, value1);
    };
  };
  return ResponseBodyError2;
}();
var TimeoutError = /* @__PURE__ */ function() {
  function TimeoutError2() {
  }
  ;
  TimeoutError2.value = new TimeoutError2();
  return TimeoutError2;
}();
var RequestFailedError = /* @__PURE__ */ function() {
  function RequestFailedError2() {
  }
  ;
  RequestFailedError2.value = new RequestFailedError2();
  return RequestFailedError2;
}();
var XHROtherError = /* @__PURE__ */ function() {
  function XHROtherError2(value0) {
    this.value0 = value0;
  }
  ;
  XHROtherError2.create = function(value0) {
    return new XHROtherError2(value0);
  };
  return XHROtherError2;
}();
var request = function(driver2) {
  return function(req) {
    var parseJSON = function(v2) {
      if (v2 === "") {
        return pure3(jsonEmptyObject);
      }
      ;
      return either(function($74) {
        return fail2(ForeignError.create($74));
      })(pure3)(jsonParser(v2));
    };
    var fromResponse = function() {
      if (req.responseFormat instanceof $$ArrayBuffer) {
        return unsafeReadTagged2("ArrayBuffer");
      }
      ;
      if (req.responseFormat instanceof Blob) {
        return unsafeReadTagged2("Blob");
      }
      ;
      if (req.responseFormat instanceof Document) {
        return function(x) {
          return alt2(unsafeReadTagged2("Document")(x))(alt2(unsafeReadTagged2("XMLDocument")(x))(unsafeReadTagged2("HTMLDocument")(x)));
        };
      }
      ;
      if (req.responseFormat instanceof Json) {
        return composeKleisliFlipped2(function($75) {
          return req.responseFormat.value0(parseJSON($75));
        })(unsafeReadTagged2("String"));
      }
      ;
      if (req.responseFormat instanceof $$String) {
        return unsafeReadTagged2("String");
      }
      ;
      if (req.responseFormat instanceof Ignore) {
        return $$const(req.responseFormat.value0(pure3(unit)));
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 274, column 18 - line 283, column 57): " + [req.responseFormat.constructor.name]);
    }();
    var extractContent = function(v2) {
      if (v2 instanceof ArrayView) {
        return new Right(v2.value0(unsafeToForeign));
      }
      ;
      if (v2 instanceof Blob2) {
        return new Right(unsafeToForeign(v2.value0));
      }
      ;
      if (v2 instanceof Document2) {
        return new Right(unsafeToForeign(v2.value0));
      }
      ;
      if (v2 instanceof $$String2) {
        return new Right(unsafeToForeign(v2.value0));
      }
      ;
      if (v2 instanceof FormData) {
        return new Right(unsafeToForeign(v2.value0));
      }
      ;
      if (v2 instanceof FormURLEncoded) {
        return note("Body contains values that cannot be encoded as application/x-www-form-urlencoded")(map8(unsafeToForeign)(encode(v2.value0)));
      }
      ;
      if (v2 instanceof Json2) {
        return new Right(unsafeToForeign(stringify(v2.value0)));
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 235, column 20 - line 250, column 69): " + [v2.constructor.name]);
    };
    var addHeader = function(mh) {
      return function(hs) {
        if (mh instanceof Just && !any2(on(eq2)(name)(mh.value0))(hs)) {
          return snoc(hs)(mh.value0);
        }
        ;
        return hs;
      };
    };
    var headers = function(reqContent) {
      return addHeader(map8(ContentType.create)(bindFlipped4(toMediaType2)(reqContent)))(addHeader(map8(Accept.create)(toMediaType(req.responseFormat)))(req.headers));
    };
    var ajaxRequest = function(v2) {
      return {
        method: print(req.method),
        url: req.url,
        headers: map12(function(h) {
          return {
            field: name(h),
            value: value(h)
          };
        })(headers(req.content)),
        content: v2,
        responseType: toResponseType(req.responseFormat),
        username: toNullable(req.username),
        password: toNullable(req.password),
        withCredentials: req.withCredentials,
        timeout: fromMaybe(0)(map8(function(v1) {
          return v1;
        })(req.timeout))
      };
    };
    var send = function(content3) {
      return mapFlipped2($$try3(fromEffectFnAff(_ajax(driver2, "AffjaxTimeoutErrorMessageIdent", "AffjaxRequestFailedMessageIdent", ResponseHeader.create, ajaxRequest(content3)))))(function(v2) {
        if (v2 instanceof Right) {
          var v1 = runExcept(fromResponse(v2.value0.body));
          if (v1 instanceof Left) {
            return new Left(new ResponseBodyError(head(v1.value0), v2.value0));
          }
          ;
          if (v1 instanceof Right) {
            return new Right({
              headers: v2.value0.headers,
              status: v2.value0.status,
              statusText: v2.value0.statusText,
              body: v1.value0
            });
          }
          ;
          throw new Error("Failed pattern match at Affjax (line 209, column 9 - line 211, column 52): " + [v1.constructor.name]);
        }
        ;
        if (v2 instanceof Left) {
          return new Left(function() {
            var message2 = message(v2.value0);
            var $61 = message2 === "AffjaxTimeoutErrorMessageIdent";
            if ($61) {
              return TimeoutError.value;
            }
            ;
            var $62 = message2 === "AffjaxRequestFailedMessageIdent";
            if ($62) {
              return RequestFailedError.value;
            }
            ;
            return new XHROtherError(v2.value0);
          }());
        }
        ;
        throw new Error("Failed pattern match at Affjax (line 207, column 144 - line 219, column 28): " + [v2.constructor.name]);
      });
    };
    if (req.content instanceof Nothing) {
      return send(toNullable(Nothing.value));
    }
    ;
    if (req.content instanceof Just) {
      var v = extractContent(req.content.value0);
      if (v instanceof Right) {
        return send(toNullable(new Just(v.value0)));
      }
      ;
      if (v instanceof Left) {
        return pure1(new Left(new RequestContentError(v.value0)));
      }
      ;
      throw new Error("Failed pattern match at Affjax (line 199, column 7 - line 203, column 48): " + [v.constructor.name]);
    }
    ;
    throw new Error("Failed pattern match at Affjax (line 195, column 3 - line 203, column 48): " + [req.content.constructor.name]);
  };
};
var defaultRequest = /* @__PURE__ */ function() {
  return {
    method: new Left(GET.value),
    url: "/",
    headers: [],
    content: Nothing.value,
    username: Nothing.value,
    password: Nothing.value,
    withCredentials: false,
    responseFormat: ignore,
    timeout: Nothing.value
  };
}();
var get = function(driver2) {
  return function(rf) {
    return function(u2) {
      return request(driver2)({
        method: defaultRequest.method,
        headers: defaultRequest.headers,
        content: defaultRequest.content,
        username: defaultRequest.username,
        password: defaultRequest.password,
        withCredentials: defaultRequest.withCredentials,
        timeout: defaultRequest.timeout,
        url: u2,
        responseFormat: rf
      });
    };
  };
};

// output/Affjax.Web/index.js
var get2 = /* @__PURE__ */ get(driver);

// output/Data.Argonaut.Decode.Error/index.js
var show1 = /* @__PURE__ */ show(showInt);
var TypeMismatch2 = /* @__PURE__ */ function() {
  function TypeMismatch3(value0) {
    this.value0 = value0;
  }
  ;
  TypeMismatch3.create = function(value0) {
    return new TypeMismatch3(value0);
  };
  return TypeMismatch3;
}();
var UnexpectedValue = /* @__PURE__ */ function() {
  function UnexpectedValue2(value0) {
    this.value0 = value0;
  }
  ;
  UnexpectedValue2.create = function(value0) {
    return new UnexpectedValue2(value0);
  };
  return UnexpectedValue2;
}();
var AtIndex = /* @__PURE__ */ function() {
  function AtIndex2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  AtIndex2.create = function(value0) {
    return function(value1) {
      return new AtIndex2(value0, value1);
    };
  };
  return AtIndex2;
}();
var AtKey = /* @__PURE__ */ function() {
  function AtKey2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  AtKey2.create = function(value0) {
    return function(value1) {
      return new AtKey2(value0, value1);
    };
  };
  return AtKey2;
}();
var Named = /* @__PURE__ */ function() {
  function Named2(value0, value1) {
    this.value0 = value0;
    this.value1 = value1;
  }
  ;
  Named2.create = function(value0) {
    return function(value1) {
      return new Named2(value0, value1);
    };
  };
  return Named2;
}();
var MissingValue = /* @__PURE__ */ function() {
  function MissingValue2() {
  }
  ;
  MissingValue2.value = new MissingValue2();
  return MissingValue2;
}();
var printJsonDecodeError = function(err) {
  var go2 = function(v) {
    if (v instanceof TypeMismatch2) {
      return "  Expected value of type '" + (v.value0 + "'.");
    }
    ;
    if (v instanceof UnexpectedValue) {
      return "  Unexpected value " + (stringify(v.value0) + ".");
    }
    ;
    if (v instanceof AtIndex) {
      return "  At array index " + (show1(v.value0) + (":\n" + go2(v.value1)));
    }
    ;
    if (v instanceof AtKey) {
      return "  At object key '" + (v.value0 + ("':\n" + go2(v.value1)));
    }
    ;
    if (v instanceof Named) {
      return "  Under '" + (v.value0 + ("':\n" + go2(v.value1)));
    }
    ;
    if (v instanceof MissingValue) {
      return "  No value was found.";
    }
    ;
    throw new Error("Failed pattern match at Data.Argonaut.Decode.Error (line 37, column 8 - line 43, column 44): " + [v.constructor.name]);
  };
  return "An error occurred while decoding a JSON value:\n" + go2(err);
};

// output/Data.Map.Internal/index.js
var $runtime_lazy3 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
var singleton7 = function(k) {
  return function(v) {
    return new Node(1, 1, k, v, Leaf.value, Leaf.value);
  };
};
var unsafeBalancedNode = /* @__PURE__ */ function() {
  var height9 = function(v) {
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
    if (rl instanceof Node && rl.value0 > height9(rr)) {
      return unsafeNode(rl.value2, rl.value3, unsafeNode(k, v, l, rl.value4), unsafeNode(rk, rv, rl.value5, rr));
    }
    ;
    return unsafeNode(rk, rv, unsafeNode(k, v, l, rl), rr);
  };
  var rotateRight = function(k, v, lk, lv, ll, lr, r) {
    if (lr instanceof Node && height9(ll) <= lr.value0) {
      return unsafeNode(lr.value2, lr.value3, unsafeNode(lk, lv, ll, lr.value4), unsafeNode(k, v, lr.value5, r));
    }
    ;
    return unsafeNode(lk, lv, ll, unsafeNode(k, v, lr, r));
  };
  return function(k, v, l, r) {
    if (l instanceof Leaf) {
      if (r instanceof Leaf) {
        return singleton7(k)(v);
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
var insert2 = function(dictOrd) {
  var compare2 = compare(dictOrd);
  return function(k) {
    return function(v) {
      var go2 = function(v1) {
        if (v1 instanceof Leaf) {
          return singleton7(k)(v);
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
    var mempty2 = mempty(dictMonoid);
    var append12 = append(dictMonoid.Semigroup0());
    return function(f) {
      var go2 = function(v) {
        if (v instanceof Leaf) {
          return mempty2;
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
var $$delete4 = function(dictOrd) {
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

// output/Data.String.CodePoints/foreign.js
var hasArrayFrom = typeof Array.from === "function";
var hasStringIterator = typeof Symbol !== "undefined" && Symbol != null && typeof Symbol.iterator !== "undefined" && typeof String.prototype[Symbol.iterator] === "function";
var hasFromCodePoint = typeof String.prototype.fromCodePoint === "function";
var hasCodePointAt = typeof String.prototype.codePointAt === "function";

// output/Data.Argonaut.Decode.Decoders/index.js
var map9 = /* @__PURE__ */ map(functorEither);
var lmap2 = /* @__PURE__ */ lmap(bifunctorEither);
var composeKleisliFlipped3 = /* @__PURE__ */ composeKleisliFlipped(bindEither);
var bind2 = /* @__PURE__ */ bind(bindEither);
var traverseWithIndex2 = /* @__PURE__ */ traverseWithIndex(traversableWithIndexArray)(applicativeEither);
var apply3 = /* @__PURE__ */ apply(applyEither);
var decodeString = /* @__PURE__ */ function() {
  return caseJsonString(new Left(new TypeMismatch2("String")))(Right.create);
}();
var decodeNumber = /* @__PURE__ */ function() {
  return caseJsonNumber(new Left(new TypeMismatch2("Number")))(Right.create);
}();
var decodeJArray = /* @__PURE__ */ function() {
  var $52 = note(new TypeMismatch2("Array"));
  return function($53) {
    return $52(toArray($53));
  };
}();
var decodeInt = /* @__PURE__ */ composeKleisliFlipped3(/* @__PURE__ */ function() {
  var $84 = note(new TypeMismatch2("Integer"));
  return function($85) {
    return $84(fromNumber($85));
  };
}())(decodeNumber);
var decodeArray = function(decoder) {
  return composeKleisliFlipped3(function() {
    var $89 = lmap2(Named.create("Array"));
    var $90 = traverseWithIndex2(function(i2) {
      var $92 = lmap2(AtIndex.create(i2));
      return function($93) {
        return $92(decoder($93));
      };
    });
    return function($91) {
      return $89($90($91));
    };
  }())(decodeJArray);
};
var decodeTuple = function(decoderA) {
  return function(decoderB) {
    return function(json2) {
      var f = function(v) {
        if (v.length === 2) {
          return apply3(map9(Tuple.create)(decoderA(v[0])))(decoderB(v[1]));
        }
        ;
        return new Left(new TypeMismatch2("Tuple"));
      };
      return bind2(decodeArray(Right.create)(json2))(f);
    };
  };
};

// output/Record/index.js
var insert3 = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  return function() {
    return function() {
      return function(l) {
        return function(a2) {
          return function(r) {
            return unsafeSet(reflectSymbol2(l))(a2)(r);
          };
        };
      };
    };
  };
};

// output/Data.Argonaut.Decode.Class/index.js
var bind3 = /* @__PURE__ */ bind(bindEither);
var lmap3 = /* @__PURE__ */ lmap(bifunctorEither);
var map10 = /* @__PURE__ */ map(functorMaybe);
var gDecodeJsonNil = {
  gDecodeJson: function(v) {
    return function(v1) {
      return new Right({});
    };
  }
};
var gDecodeJson = function(dict) {
  return dict.gDecodeJson;
};
var decodeRecord = function(dictGDecodeJson) {
  var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
  return function() {
    return {
      decodeJson: function(json2) {
        var v = toObject(json2);
        if (v instanceof Just) {
          return gDecodeJson1(v.value0)($$Proxy.value);
        }
        ;
        if (v instanceof Nothing) {
          return new Left(new TypeMismatch2("Object"));
        }
        ;
        throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 103, column 5 - line 105, column 46): " + [v.constructor.name]);
      }
    };
  };
};
var decodeJsonString = {
  decodeJson: decodeString
};
var decodeJsonNumber = {
  decodeJson: decodeNumber
};
var decodeJsonInt = {
  decodeJson: decodeInt
};
var decodeJsonField = function(dict) {
  return dict.decodeJsonField;
};
var gDecodeJsonCons = function(dictDecodeJsonField) {
  var decodeJsonField1 = decodeJsonField(dictDecodeJsonField);
  return function(dictGDecodeJson) {
    var gDecodeJson1 = gDecodeJson(dictGDecodeJson);
    return function(dictIsSymbol) {
      var reflectSymbol2 = reflectSymbol(dictIsSymbol);
      var insert7 = insert3(dictIsSymbol)()();
      return function() {
        return function() {
          return {
            gDecodeJson: function(object2) {
              return function(v) {
                var fieldName = reflectSymbol2($$Proxy.value);
                var fieldValue = lookup(fieldName)(object2);
                var v1 = decodeJsonField1(fieldValue);
                if (v1 instanceof Just) {
                  return bind3(lmap3(AtKey.create(fieldName))(v1.value0))(function(val) {
                    return bind3(gDecodeJson1(object2)($$Proxy.value))(function(rest) {
                      return new Right(insert7($$Proxy.value)(val)(rest));
                    });
                  });
                }
                ;
                if (v1 instanceof Nothing) {
                  return new Left(new AtKey(fieldName, MissingValue.value));
                }
                ;
                throw new Error("Failed pattern match at Data.Argonaut.Decode.Class (line 127, column 5 - line 134, column 44): " + [v1.constructor.name]);
              };
            }
          };
        };
      };
    };
  };
};
var decodeJson = function(dict) {
  return dict.decodeJson;
};
var decodeJsonTuple = function(dictDecodeJson) {
  var decodeJson1 = decodeJson(dictDecodeJson);
  return function(dictDecodeJson1) {
    return {
      decodeJson: decodeTuple(decodeJson1)(decodeJson(dictDecodeJson1))
    };
  };
};
var decodeFieldId = function(dictDecodeJson) {
  var decodeJson1 = decodeJson(dictDecodeJson);
  return {
    decodeJsonField: function(j) {
      return map10(decodeJson1)(j);
    }
  };
};
var decodeArray2 = function(dictDecodeJson) {
  return {
    decodeJson: decodeArray(decodeJson(dictDecodeJson))
  };
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
var step2 = function(v, a2) {
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
var map11 = /* @__PURE__ */ map(functorArray);
var map13 = /* @__PURE__ */ map(functorTuple);
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
      return new Elem(v2.value0, v2.value1, v.value0(v2.value2), map11(go2)(v2.value3));
    }
    ;
    if (v2 instanceof Keyed) {
      return new Keyed(v2.value0, v2.value1, v.value0(v2.value2), map11(map13(go2))(v2.value3));
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
function createElement(ns, name16, doc) {
  if (ns != null) {
    return doc.createElementNS(ns, name16);
  } else {
    return doc.createElement(name16);
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
var getProp = function(name16) {
  return function(doctype) {
    return doctype[name16];
  };
};
var _namespaceURI = getProp("namespaceURI");
var _prefix = getProp("prefix");
var localName = getProp("localName");
var tagName = getProp("tagName");

// output/Web.DOM.ParentNode/foreign.js
var getEffProp = function(name16) {
  return function(node) {
    return function() {
      return node[name16];
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
var map14 = /* @__PURE__ */ map(functorEffect);
var querySelector = function(qs) {
  var $2 = map14(toMaybe);
  var $3 = _querySelector(qs);
  return function($4) {
    return $2($3($4));
  };
};

// output/Web.DOM.Element/index.js
var toNode = unsafeCoerce2;

// output/Halogen.VDom.DOM/index.js
var $runtime_lazy4 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
      var res = step2(state3.widget, vdom.value0);
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
      var v = length(vdom.value3);
      var v1 = length(state3.children);
      if (v1 === 0 && v === 0) {
        var attrs2 = step2(state3.attrs, vdom.value2);
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
        var res = step2(s, v2);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var onThat = function(ix, v2) {
        var res = state3.build(v2);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var children2 = diffWithIxE(state3.children, vdom.value3, onThese, onThis, onThat);
      var attrs2 = step2(state3.attrs, vdom.value2);
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
      var v = length(vdom.value3);
      if (state3.length === 0 && v === 0) {
        var attrs2 = step2(state3.attrs, vdom.value2);
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
        var res = step2(s, v3.value1);
        insertChildIx(ix$prime, extract2(res), state3.node);
        return res;
      };
      var onThat = function(v2, ix, v3) {
        var res = state3.build(v3.value1);
        insertChildIx(ix, extract2(res), state3.node);
        return res;
      };
      var children2 = diffWithKeyAndIxE(state3.children, vdom.value3, fst, onThese, onThis, onThat);
      var attrs2 = step2(state3.attrs, vdom.value2);
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
    length: length(ch1)
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
var $runtime_lazy5 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
var propFromBoolean = unsafeCoerce2;
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
            return function __do2() {
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
var isPropBoolean = {
  toPropValue: propFromBoolean
};
var handler = /* @__PURE__ */ function() {
  return Handler.create;
}();
var element = function(ns) {
  return function(name16) {
    return function(props) {
      return function(children2) {
        return new Elem(ns, name16, props, children2);
      };
    };
  };
};

// output/Control.Applicative.Free/index.js
var identity8 = /* @__PURE__ */ identity(categoryFn);
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
  var pure11 = pure(dictApplicative);
  return function(fStack) {
    return function(valStack) {
      return function(nat) {
        return function(func) {
          return function(count) {
            if (func instanceof Pure) {
              return new Tuple(new Cons({
                func: pure11(func.value0),
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
  var apply5 = apply(dictApplicative.Apply0());
  return function(fStack) {
    return function(vals) {
      return function(gVal) {
        if (fStack instanceof Nil) {
          return new Left(gVal);
        }
        ;
        if (fStack instanceof Cons) {
          var gRes = apply5(fStack.value0.func)(gVal);
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
  var pure11 = pure(dictApplicative);
  var goLeft1 = goLeft(dictApplicative);
  return function(nat) {
    return function(z) {
      var go2 = function($copy_v) {
        var $tco_done = false;
        var $tco_result;
        function $tco_loop(v) {
          if (v.value1.value0 instanceof Pure) {
            var v1 = goApply1(v.value0)(v.value1.value1)(pure11(v.value1.value0.value0));
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
      return go2(new Tuple(Nil.value, singleton5(z)));
    };
  };
};
var retractFreeAp = function(dictApplicative) {
  return foldFreeAp(dictApplicative)(identity8);
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
var uncons4 = function($copy_v) {
  var $tco_done = false;
  var $tco_result;
  function $tco_loop(v) {
    if (v.value0 instanceof Nil && v.value1 instanceof Nil) {
      $tco_done = true;
      return Nothing.value;
    }
    ;
    if (v.value0 instanceof Nil) {
      $copy_v = new CatQueue(reverse2(v.value1), Nil.value);
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
var snoc3 = function(v) {
  return function(a2) {
    return new CatQueue(v.value0, new Cons(a2, v.value1));
  };
};
var $$null4 = function(v) {
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
      return new CatCons(v.value0, snoc3(v.value1)(v1));
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
            var v = uncons4(xs);
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
var uncons5 = function(v) {
  if (v instanceof CatNil) {
    return Nothing.value;
  }
  ;
  if (v instanceof CatCons) {
    return new Just(new Tuple(v.value0, function() {
      var $66 = $$null4(v.value1);
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
var snoc4 = function(cat) {
  return function(a2) {
    return append2(cat)(new CatCons(a2, empty5));
  };
};

// output/Control.Monad.Free/index.js
var $runtime_lazy6 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
      var v2 = uncons5(v.value1);
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
      return new Free(v.value0, snoc4(v.value1)(k));
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
var pure4 = /* @__PURE__ */ pure(freeApplicative);
var liftF = function(f) {
  return fromView(new Bind(f, function($192) {
    return pure4($192);
  }));
};
var foldFree = function(dictMonadRec) {
  var Monad0 = dictMonadRec.Monad0();
  var map111 = map(Monad0.Bind1().Apply0().Functor0());
  var pure14 = pure(Monad0.Applicative0());
  var tailRecM4 = tailRecM(dictMonadRec);
  return function(k) {
    var go2 = function(f) {
      var v = toView(f);
      if (v instanceof Return) {
        return map111(Done.create)(pure14(v.value0));
      }
      ;
      if (v instanceof Bind) {
        return map111(function($199) {
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
var bind4 = /* @__PURE__ */ bind(bindEffect);
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
var create = function __do() {
  var subscribers = $$new([])();
  return {
    emitter: function(k) {
      return function __do2() {
        modify_(function(v) {
          return append4(v)([k]);
        })(subscribers)();
        return modify_(deleteBy(unsafeRefEq)(k))(subscribers);
      };
    },
    listener: function(a2) {
      return bind4(read(subscribers))(traverse_1(function(k) {
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
var $runtime_lazy7 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
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
      var vdom = step2(state3.vdom, toVDom(runThunk(t2)));
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
var map15 = /* @__PURE__ */ map(functorHalogenM);
var pure5 = /* @__PURE__ */ pure(applicativeHalogenM);
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
        var $45 = map15(maybe(v.value1(unit))(g));
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
    handleAction: $$const(pure5(unit)),
    handleQuery: $$const(pure5(Nothing.value)),
    receive: $$const(Nothing.value),
    initialize: Nothing.value,
    finalize: Nothing.value
  };
}();

// output/Halogen.HTML.Elements/index.js
var element2 = /* @__PURE__ */ function() {
  return element(Nothing.value);
}();
var hr = function(props) {
  return element2("hr")(props)([]);
};
var img = function(props) {
  return element2("img")(props)([]);
};
var li = /* @__PURE__ */ element2("li");
var span3 = /* @__PURE__ */ element2("span");
var ul = /* @__PURE__ */ element2("ul");
var div2 = /* @__PURE__ */ element2("div");
var button = /* @__PURE__ */ element2("button");

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
var unwrap4 = /* @__PURE__ */ unwrap();
var prop2 = function(dictIsProp) {
  return prop(dictIsProp);
};
var prop1 = /* @__PURE__ */ prop2(isPropBoolean);
var prop22 = /* @__PURE__ */ prop2(isPropString);
var prop3 = /* @__PURE__ */ prop2(isPropInt);
var src2 = /* @__PURE__ */ prop22("src");
var width2 = /* @__PURE__ */ prop3("width");
var height2 = /* @__PURE__ */ prop3("height");
var disabled2 = /* @__PURE__ */ prop1("disabled");
var classes = /* @__PURE__ */ function() {
  var $32 = prop22("className");
  var $33 = joinWith(" ");
  var $34 = map(functorArray)(unwrap4);
  return function($35) {
    return $32($33($34($35)));
  };
}();

// output/Raffleize.Types/index.js
var map16 = /* @__PURE__ */ map(functorEither);
var apply4 = /* @__PURE__ */ apply(applyEither);
var bind5 = /* @__PURE__ */ bind(bindEither);
var ValueItem = function(x) {
  return x;
};
var decodeJsonValueItem = {
  decodeJson: function(json2) {
    var f = function(v) {
      if (v.length === 3) {
        return map16(ValueItem)(apply4(map16(Tuple.create)(apply4(map16(Tuple.create)(decodeString(v[0])))(decodeString(v[1]))))(decodeInt(v[2])));
      }
      ;
      return new Left(new TypeMismatch2("Value"));
    };
    return bind5(decodeArray(Right.create)(json2))(f);
  }
};

// output/Data.Argonaut.Aeson.Helpers/index.js
var Mode = /* @__PURE__ */ function() {
  function Mode2(value0) {
    this.value0 = value0;
  }
  ;
  Mode2.create = function(value0) {
    return new Mode2(value0);
  };
  return Mode2;
}();
var isSingleConstructor_Constructor = {
  isSingleConstructor: function(v) {
    return true;
  }
};
var isSingleConstructor = function(dict) {
  return dict.isSingleConstructor;
};
var areAllConstructorsNullary_Argument = {
  areAllConstructorsNullary: function(v) {
    return false;
  }
};
var areAllConstructorsNullary = function(dict) {
  return dict.areAllConstructorsNullary;
};
var areAllConstructorsNullary_Constructor = function(dictAreAllConstructorsNullary) {
  var areAllConstructorsNullary1 = areAllConstructorsNullary(dictAreAllConstructorsNullary);
  return {
    areAllConstructorsNullary: function(v) {
      return areAllConstructorsNullary1($$Proxy.value);
    }
  };
};

// output/Data.Argonaut.Decode.Generic/index.js
var bind6 = /* @__PURE__ */ bind(bindEither);
var map17 = /* @__PURE__ */ map(functorEither);
var decodeRepArgsArgument = function(dictDecodeJson) {
  var decodeJson3 = decodeJson(dictDecodeJson);
  return {
    decodeRepArgs: function(js) {
      return bind6(note(new TypeMismatch2("NonEmptyArray"))(uncons(js)))(function(v) {
        return map17(function($104) {
          return function(v1) {
            return {
              init: v1,
              rest: v.tail
            };
          }(Argument($104));
        })(decodeJson3(v.head));
      });
    }
  };
};
var decodeRepArgs = function(dict) {
  return dict.decodeRepArgs;
};

// output/Data.Argonaut.Aeson.Decode.Generic/index.js
var map18 = /* @__PURE__ */ map(functorEither);
var composeKleisli2 = /* @__PURE__ */ composeKleisli(bindEither);
var show2 = /* @__PURE__ */ show(showString);
var lmap4 = /* @__PURE__ */ lmap(bifunctorEither);
var bind7 = /* @__PURE__ */ bind(bindEither);
var pure6 = /* @__PURE__ */ pure(applicativeEither);
var discard2 = /* @__PURE__ */ discard(discardUnit)(bindEither);
var toJsonArray = /* @__PURE__ */ caseJson(/* @__PURE__ */ $$const(/* @__PURE__ */ singleton2(jsonNull)))(function($134) {
  return singleton2(id($134));
})(function($135) {
  return singleton2(id($135));
})(function($136) {
  return singleton2(id($136));
})(function($137) {
  return singleton2(id($137));
})(function($138) {
  return singleton2(id($138));
});
var decodingErr = function(name16) {
  return function(msg) {
    return new Named("When decoding a " + name16, msg);
  };
};
var decodeAeson$prime = function(dict) {
  return dict["decodeAeson'"];
};
var decodeAesonConstructor = function(dictDecodeRepArgs) {
  return function(dictIsSymbol) {
    return function(dictAreAllConstructorsNullary) {
      var areAllConstructorsNullary2 = areAllConstructorsNullary(dictAreAllConstructorsNullary);
      return function(dictIsSingleConstructor) {
        var isSingleConstructor2 = isSingleConstructor(dictIsSingleConstructor);
        return function(dictDecodeAeson$prime) {
          var decodeAeson$prime1 = decodeAeson$prime(dictDecodeAeson$prime);
          return {
            decodeAeson: function(o) {
              return function(thing) {
                var mode = new Mode({
                  "_Mode_ConstructorIsSingle": isSingleConstructor2($$Proxy.value),
                  "_Mode_ConstructorsAreAllNullary": areAllConstructorsNullary2($$Proxy.value)
                });
                return decodeAeson$prime1(mode)(o)(thing);
              };
            }
          };
        };
      };
    };
  };
};
var decodeAeson = function(dict) {
  return dict.decodeAeson;
};
var genericDecodeAeson = function(dictGeneric) {
  var to2 = to(dictGeneric);
  return function(dictDecodeAeson) {
    var decodeAeson1 = decodeAeson(dictDecodeAeson);
    return function(o) {
      var $139 = map18(to2);
      var $140 = decodeAeson1(o);
      return function($141) {
        return $139($140($141));
      };
    };
  };
};
var checkTag = function(tagFieldName) {
  return function(expectedTag) {
    return composeKleisli2(function() {
      var $142 = note(new Named(show2(tagFieldName) + " property is missing", MissingValue.value));
      var $143 = lookup(tagFieldName);
      return function($144) {
        return $142($143($144));
      };
    }())(composeKleisli2(function() {
      var $145 = note(new TypeMismatch2(show2(tagFieldName) + " property is not a string"));
      return function($146) {
        return $145(toString($146));
      };
    }())(function(actualTag) {
      var $75 = actualTag !== expectedTag;
      if ($75) {
        return new Left(new Named("'tag' property has an incorrect value", new TypeMismatch2(actualTag)));
      }
      ;
      return new Right(unit);
    }));
  };
};
var decodeGeneralCase = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  return function(dictDecodeRepArgs) {
    var decodeRepArgs2 = decodeRepArgs(dictDecodeRepArgs);
    return function(mode) {
      return function(options2) {
        return function(json2) {
          var name16 = reflectSymbol2($$Proxy.value);
          var $93 = {
            mode,
            options: options2
          };
          return bind7(note(new TypeMismatch2("expected an object"))(toObject(json2)))(function(objectJson) {
            return discard2(checkTag($93.options.sumEncoding.value0.tagFieldName)(name16)(objectJson))(function() {
              return bind7(function() {
                var v = lookup($93.options.sumEncoding.value0.contentsFieldName)(objectJson);
                if (v instanceof Just) {
                  return decodeRepArgs2(toJsonArray(v.value0));
                }
                ;
                if (v instanceof Nothing) {
                  return decodeRepArgs2(singleton2(id($$delete($93.options.sumEncoding.value0.tagFieldName)(objectJson))));
                }
                ;
                throw new Error("Failed pattern match at Data.Argonaut.Aeson.Decode.Generic (line 166, column 27 - line 170, column 115): " + [v.constructor.name]);
              }())(function(v) {
                return pure6(v.init);
              });
            });
          });
        };
      };
    };
  };
};
var decodeAesonConstructor$prime = function(dictIsSymbol) {
  var reflectSymbol2 = reflectSymbol(dictIsSymbol);
  var decodeGeneralCase1 = decodeGeneralCase(dictIsSymbol);
  return function(dictDecodeRepArgs) {
    var decodeRepArgs2 = decodeRepArgs(dictDecodeRepArgs);
    var decodeGeneralCase2 = decodeGeneralCase1(dictDecodeRepArgs);
    return {
      "decodeAeson'": function(mode) {
        return function(options2) {
          return function(json2) {
            var name16 = reflectSymbol2($$Proxy.value);
            return lmap4(decodingErr(name16))(function() {
              var $102 = {
                mode,
                options: options2
              };
              if ($102["mode"]["value0"]["_Mode_ConstructorsAreAllNullary"]) {
                return unsafeCrashWith("Unreachable: cannot have all nullary constructors and an `Argument` constructor at once.");
              }
              ;
              if ($102["mode"]["value0"]["_Mode_ConstructorIsSingle"] && !$102.options.tagSingleConstructors) {
                return bind7(decodeRepArgs2(caseJsonArray(singleton2(json2))(function($147) {
                  return singleton2(id($147));
                })(json2)))(function(v) {
                  return pure6(v.init);
                });
              }
              ;
              return decodeGeneralCase2(mode)(options2)(json2);
            }());
          };
        };
      }
    };
  };
};

// output/Data.Argonaut.Aeson.Options/index.js
var TaggedObject = /* @__PURE__ */ function() {
  function TaggedObject2(value0) {
    this.value0 = value0;
  }
  ;
  TaggedObject2.create = function(value0) {
    return new TaggedObject2(value0);
  };
  return TaggedObject2;
}();
var defaultOptions = /* @__PURE__ */ function() {
  return {
    sumEncoding: new TaggedObject({
      tagFieldName: "tag",
      contentsFieldName: "contents"
    }),
    tagSingleConstructors: false,
    allNullaryToStringTag: true
  };
}();

// output/RaffleizeDApp.CustomTypes.RaffleTypes/index.js
var rTicketValidatorHashIsSymbol = {
  reflectSymbol: function() {
    return "rTicketValidatorHash";
  }
};
var rTicketCollateralIsSymbol = {
  reflectSymbol: function() {
    return "rTicketCollateral";
  }
};
var rRaffleValidatorHashIsSymbol = {
  reflectSymbol: function() {
    return "rRaffleValidatorHash";
  }
};
var rRaffleCollateralIsSymbol = {
  reflectSymbol: function() {
    return "rRaffleCollateral";
  }
};
var rMinTicketPriceIsSymbol = {
  reflectSymbol: function() {
    return "rMinTicketPrice";
  }
};
var rMinRevealingWindowIsSymbol = {
  reflectSymbol: function() {
    return "rMinRevealingWindow";
  }
};
var rMaxNoOfTicketsIsSymbol = {
  reflectSymbol: function() {
    return "rMaxNoOfTickets";
  }
};
var RaffleParamIsSymbol = {
  reflectSymbol: function() {
    return "RaffleParam";
  }
};
var areAllConstructorsNullary_Constructor2 = /* @__PURE__ */ areAllConstructorsNullary_Constructor(areAllConstructorsNullary_Argument);
var rTicketPriceIsSymbol = {
  reflectSymbol: function() {
    return "rTicketPrice";
  }
};
var rStakeIsSymbol = {
  reflectSymbol: function() {
    return "rStake";
  }
};
var rRevealDDLIsSymbol = {
  reflectSymbol: function() {
    return "rRevealDDL";
  }
};
var rMinTicketsIsSymbol = {
  reflectSymbol: function() {
    return "rMinTickets";
  }
};
var rCommitDDLIsSymbol = {
  reflectSymbol: function() {
    return "rCommitDDL";
  }
};
var RaffleConfigIsSymbol = {
  reflectSymbol: function() {
    return "RaffleConfig";
  }
};
var rSoldTicketsIsSymbol = {
  reflectSymbol: function() {
    return "rSoldTickets";
  }
};
var rRevealedTicketsIsSymbol = {
  reflectSymbol: function() {
    return "rRevealedTickets";
  }
};
var rRefundedTicketsIsSymbol = {
  reflectSymbol: function() {
    return "rRefundedTickets";
  }
};
var rRandomSeedIsSymbol = {
  reflectSymbol: function() {
    return "rRandomSeed";
  }
};
var rRaffleIDIsSymbol = {
  reflectSymbol: function() {
    return "rRaffleID";
  }
};
var rParamIsSymbol = {
  reflectSymbol: function() {
    return "rParam";
  }
};
var rConfigIsSymbol = {
  reflectSymbol: function() {
    return "rConfig";
  }
};
var RaffleStateDataIsSymbol = {
  reflectSymbol: function() {
    return "RaffleStateData";
  }
};
var riValueIsSymbol = {
  reflectSymbol: function() {
    return "riValue";
  }
};
var riStateLabelIsSymbol = {
  reflectSymbol: function() {
    return "riStateLabel";
  }
};
var riRsdIsSymbol = {
  reflectSymbol: function() {
    return "riRsd";
  }
};
var riImageIsSymbol = {
  reflectSymbol: function() {
    return "riImage";
  }
};
var riAvailableActionsIsSymbol = {
  reflectSymbol: function() {
    return "riAvailableActions";
  }
};
var RaffleInfoIsSymbol = {
  reflectSymbol: function() {
    return "RaffleInfo";
  }
};
var gDecodeJsonCons2 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonInt));
var gDecodeJsonCons1 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonString));
var decodeRepArgsArgument2 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons1(gDecodeJsonNil)(rTicketValidatorHashIsSymbol)()())(rTicketCollateralIsSymbol)()())(rRaffleValidatorHashIsSymbol)()())(rRaffleCollateralIsSymbol)()())(rMinTicketPriceIsSymbol)()())(rMinRevealingWindowIsSymbol)()())(rMaxNoOfTicketsIsSymbol)()())());
var gDecodeJsonCons22 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonNumber));
var gDecodeJsonCons3 = /* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(/* @__PURE__ */ decodeArray2(decodeJsonValueItem)));
var gDecodeJsonCons4 = /* @__PURE__ */ gDecodeJsonCons2(gDecodeJsonNil);
var decodeRepArgsArgument1 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons22(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons22(/* @__PURE__ */ gDecodeJsonCons3(/* @__PURE__ */ gDecodeJsonCons4(rTicketPriceIsSymbol)()())(rStakeIsSymbol)()())(rRevealDDLIsSymbol)()())(rMinTicketsIsSymbol)()())(rCommitDDLIsSymbol)()())());
var decodeJsonTuple2 = /* @__PURE__ */ decodeJsonTuple(decodeJsonString)(decodeJsonString);
var genericRaffleStateData = {
  to: function(x) {
    return x;
  },
  from: function(x) {
    return x;
  }
};
var genericRaffleParam = {
  to: function(x) {
    return x;
  },
  from: function(x) {
    return x;
  }
};
var genericRaffleInfo = {
  to: function(x) {
    return x;
  },
  from: function(x) {
    return x;
  }
};
var genericRaffleConfig = {
  to: function(x) {
    return x;
  },
  from: function(x) {
    return x;
  }
};
var decodeJsonRaffleParam = {
  decodeJson: /* @__PURE__ */ genericDecodeAeson(genericRaffleParam)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument2)(RaffleParamIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(RaffleParamIsSymbol)(decodeRepArgsArgument2)))(defaultOptions)
};
var decodeJsonRaffleConfig = {
  decodeJson: /* @__PURE__ */ genericDecodeAeson(genericRaffleConfig)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument1)(RaffleConfigIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(RaffleConfigIsSymbol)(decodeRepArgsArgument1)))(defaultOptions)
};
var decodeRepArgsArgument22 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonRaffleConfig))(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonRaffleParam))(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonTuple2))(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons2(/* @__PURE__ */ gDecodeJsonCons4(rSoldTicketsIsSymbol)()())(rRevealedTicketsIsSymbol)()())(rRefundedTicketsIsSymbol)()())(rRandomSeedIsSymbol)()())(rRaffleIDIsSymbol)()())(rParamIsSymbol)()())(rConfigIsSymbol)()())());
var decodeJsonRaffleStateData = {
  decodeJson: /* @__PURE__ */ genericDecodeAeson(genericRaffleStateData)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument22)(RaffleStateDataIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(RaffleStateDataIsSymbol)(decodeRepArgsArgument22)))(defaultOptions)
};
var decodeRepArgsArgument3 = /* @__PURE__ */ decodeRepArgsArgument(/* @__PURE__ */ decodeRecord(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(/* @__PURE__ */ decodeArray2(decodeJsonTuple2)))(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons(/* @__PURE__ */ decodeFieldId(decodeJsonRaffleStateData))(/* @__PURE__ */ gDecodeJsonCons1(/* @__PURE__ */ gDecodeJsonCons3(gDecodeJsonNil)(riValueIsSymbol)()())(riStateLabelIsSymbol)()())(riRsdIsSymbol)()())(riImageIsSymbol)()())(riAvailableActionsIsSymbol)()())());
var decodeJsonRaffleInfo = {
  decodeJson: /* @__PURE__ */ genericDecodeAeson(genericRaffleInfo)(/* @__PURE__ */ decodeAesonConstructor(decodeRepArgsArgument3)(RaffleInfoIsSymbol)(areAllConstructorsNullary_Constructor2)(isSingleConstructor_Constructor)(/* @__PURE__ */ decodeAesonConstructor$prime(RaffleInfoIsSymbol)(decodeRepArgsArgument3)))(defaultOptions)
};

// output/Components.Raffles/index.js
var discard3 = /* @__PURE__ */ discard(discardUnit)(bindHalogenM);
var modify_3 = /* @__PURE__ */ modify_2(monadStateHalogenM);
var bind12 = /* @__PURE__ */ bind(bindHalogenM);
var lmap5 = /* @__PURE__ */ lmap(bifunctorEither);
var decodeJson2 = /* @__PURE__ */ decodeJson(decodeJsonRaffleInfo);
var MakeRequest = /* @__PURE__ */ function() {
  function MakeRequest2(value0) {
    this.value0 = value0;
  }
  ;
  MakeRequest2.create = function(value0) {
    return new MakeRequest2(value0);
  };
  return MakeRequest2;
}();
var renderRaffleTimeline = function(v) {
  return ul([classes(["timeline"])])([li([])([div2([classes(["timeline-start timeline-box"])])([text("Created")]), div2([classes(["timeline-middle bg-success text-success"])])([text("|")]), hr([classes(["bg-warning"])])]), li([])([hr([classes(["bg-warning"])]), div2([classes(["timeline-end timeline-box"])])([text("Commit Deadline")]), div2([classes(["timeline-middle bg-warning text-warning"])])([text("|")]), hr([classes(["bg-neutral text-neutral"])])]), li([])([hr([classes(["bg-neutral text-neutral"])]), div2([classes(["timeline-start timeline-box"])])([text("Reveal Deadline")]), div2([classes(["timeline-middle bg-neutral text-neutral"])])([text("|")])])]);
};
var renderRaffleInfo = function(v) {
  return div2([classes(["flex flex-col lg:flex-row"])])([div2([classes(["flex flex-col"])])([renderRaffleTimeline(v.riRsd), div2([classes(["divider divider-primary"])])([]), renderRaffleTimeline(v.riRsd)]), div2([classes(["divider divider-horizontal divider-primary"])])([]), img([src2(v.riImage), width2(340), height2(340)])]);
};
var render = function(v) {
  if (v.loading === true) {
    return button([classes(["btn btn-primary"])])([span3([classes(["loading loading-spinner"])])([text("Loading")])]);
  }
  ;
  if (v.result instanceof Nothing) {
    return button([disabled2(v.loading), onClick(function(ev) {
      return new MakeRequest(ev);
    }), classes(["btn btn-primary"])])([text("Update raffle info")]);
  }
  ;
  if (v.result instanceof Just) {
    return renderRaffleInfo(v.result.value0);
  }
  ;
  throw new Error("Failed pattern match at Components.Raffles (line 62, column 1 - line 62, column 57): " + [v.constructor.name]);
};
var initialState = function(v) {
  return {
    loading: false,
    result: Nothing.value
  };
};
var handleAction = function(dictMonadAff) {
  var liftAff2 = liftAff(monadAffHalogenM(dictMonadAff));
  return function(v) {
    return discard3(modify_3(function(v1) {
      var $52 = {};
      for (var $53 in v1) {
        if ({}.hasOwnProperty.call(v1, $53)) {
          $52[$53] = v1[$53];
        }
        ;
      }
      ;
      $52.loading = true;
      return $52;
    }))(function() {
      return bind12(liftAff2(get2(json)("http://localhost:8082/info")))(function(response) {
        var r = function() {
          var v1 = hush(response);
          if (v1 instanceof Nothing) {
            return jsonEmptyObject;
          }
          ;
          if (v1 instanceof Just) {
            return v1.value0.body;
          }
          ;
          throw new Error("Failed pattern match at Components.Raffles (line 236, column 13 - line 238, column 41): " + [v1.constructor.name]);
        }();
        var ri = lmap5(printJsonDecodeError)(decodeJson2(r));
        return modify_3(function(v1) {
          var $57 = {};
          for (var $58 in v1) {
            if ({}.hasOwnProperty.call(v1, $58)) {
              $57[$58] = v1[$58];
            }
            ;
          }
          ;
          $57.loading = false;
          $57.result = hush(ri);
          return $57;
        });
      });
    });
  };
};
var component = function(dictMonadAff) {
  return mkComponent({
    initialState,
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
var map19 = /* @__PURE__ */ map(functorEffect);
var toParentNode = unsafeCoerce2;
var toDocument = unsafeCoerce2;
var readyState = function(doc) {
  return map19(function() {
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
var bind8 = /* @__PURE__ */ bind(bindAff);
var liftEffect3 = /* @__PURE__ */ liftEffect(monadEffectAff);
var bindFlipped5 = /* @__PURE__ */ bindFlipped(bindEffect);
var composeKleisliFlipped4 = /* @__PURE__ */ composeKleisliFlipped(bindEffect);
var pure7 = /* @__PURE__ */ pure(applicativeAff);
var bindFlipped1 = /* @__PURE__ */ bindFlipped(bindMaybe);
var pure12 = /* @__PURE__ */ pure(applicativeEffect);
var map20 = /* @__PURE__ */ map(functorEffect);
var discard4 = /* @__PURE__ */ discard(discardUnit);
var throwError2 = /* @__PURE__ */ throwError(monadThrowAff);
var selectElement = function(query2) {
  return bind8(liftEffect3(bindFlipped5(composeKleisliFlipped4(function() {
    var $16 = querySelector(query2);
    return function($17) {
      return $16(toParentNode($17));
    };
  }())(document))(windowImpl)))(function(mel) {
    return pure7(bindFlipped1(fromElement)(mel));
  });
};
var runHalogenAff = /* @__PURE__ */ runAff_(/* @__PURE__ */ either(throwException)(/* @__PURE__ */ $$const(/* @__PURE__ */ pure12(unit))));
var awaitLoad = /* @__PURE__ */ makeAff(function(callback) {
  return function __do2() {
    var rs = bindFlipped5(readyState)(bindFlipped5(document)(windowImpl))();
    if (rs instanceof Loading) {
      var et = map20(toEventTarget)(windowImpl)();
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
var awaitBody = /* @__PURE__ */ discard4(bindAff)(awaitLoad)(function() {
  return bind8(selectElement("body"))(function(body2) {
    return maybe(throwError2(error("Could not find body")))(pure7)(body2);
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
        return function __do2() {
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
var bindFlipped6 = /* @__PURE__ */ bindFlipped(bindMaybe);
var lookup4 = /* @__PURE__ */ lookup2(ordSubscriptionId);
var bind13 = /* @__PURE__ */ bind(bindAff);
var liftEffect4 = /* @__PURE__ */ liftEffect(monadEffectAff);
var discard5 = /* @__PURE__ */ discard(discardUnit);
var discard1 = /* @__PURE__ */ discard5(bindAff);
var traverse_12 = /* @__PURE__ */ traverse_(applicativeAff);
var traverse_22 = /* @__PURE__ */ traverse_12(foldableList);
var fork3 = /* @__PURE__ */ fork(monadForkAff);
var parSequence_2 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
var pure8 = /* @__PURE__ */ pure(applicativeAff);
var map22 = /* @__PURE__ */ map(functorCoyoneda);
var parallel3 = /* @__PURE__ */ parallel(parallelAff);
var map110 = /* @__PURE__ */ map(functorAff);
var sequential2 = /* @__PURE__ */ sequential(parallelAff);
var map23 = /* @__PURE__ */ map(functorMaybe);
var insert6 = /* @__PURE__ */ insert2(ordSubscriptionId);
var retractFreeAp2 = /* @__PURE__ */ retractFreeAp(applicativeParAff);
var $$delete5 = /* @__PURE__ */ $$delete4(ordForkId);
var unlessM2 = /* @__PURE__ */ unlessM(monadEffect);
var insert1 = /* @__PURE__ */ insert2(ordForkId);
var traverse_32 = /* @__PURE__ */ traverse_12(foldableMaybe);
var lookup1 = /* @__PURE__ */ lookup2(ordForkId);
var lookup22 = /* @__PURE__ */ lookup2(ordString);
var foldFree2 = /* @__PURE__ */ foldFree(monadRecAff);
var alter2 = /* @__PURE__ */ alter(ordString);
var unsubscribe3 = function(sid) {
  return function(ref2) {
    return function __do2() {
      var v = read(ref2)();
      var subs = read(v.subscriptions)();
      return traverse_4(unsubscribe)(bindFlipped6(lookup4(sid))(subs))();
    };
  };
};
var queueOrRun = function(ref2) {
  return function(au) {
    return bind13(liftEffect4(read(ref2)))(function(v) {
      if (v instanceof Nothing) {
        return au;
      }
      ;
      if (v instanceof Just) {
        return liftEffect4(write(new Just(new Cons(au, v.value0)))(ref2));
      }
      ;
      throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 188, column 33 - line 190, column 57): " + [v.constructor.name]);
    });
  };
};
var handleLifecycle = function(lchs) {
  return function(f) {
    return discard1(liftEffect4(write({
      initializers: Nil.value,
      finalizers: Nil.value
    })(lchs)))(function() {
      return bind13(liftEffect4(f))(function(result) {
        return bind13(liftEffect4(read(lchs)))(function(v) {
          return discard1(traverse_22(fork3)(v.finalizers))(function() {
            return discard1(parSequence_2(v.initializers))(function() {
              return pure8(result);
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
    return bind13(liftEffect4(read(ref2)))(function(v) {
      return liftEffect4(modify$prime(function(i2) {
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
      return bind13(liftEffect4(read(ref2)))(function(v) {
        return evalM(render2)(ref2)(v["component"]["eval"](new Query(map22(Just.create)(liftCoyoneda(q2)), $$const(Nothing.value))));
      });
    };
  };
};
var evalM = function(render2) {
  return function(initRef) {
    return function(v) {
      var evalChildQuery = function(ref2) {
        return function(cqb) {
          return bind13(liftEffect4(read(ref2)))(function(v1) {
            return unChildQueryBox(function(v2) {
              var evalChild = function(v3) {
                return parallel3(bind13(liftEffect4(read(v3)))(function(dsx) {
                  return unDriverStateX(function(ds) {
                    return evalQ(render2)(ds.selfRef)(v2.value1);
                  })(dsx);
                }));
              };
              return map110(v2.value2)(sequential2(v2.value0(applicativeParAff)(evalChild)(v1.children)));
            })(cqb);
          });
        };
      };
      var go2 = function(ref2) {
        return function(v1) {
          if (v1 instanceof State) {
            return bind13(liftEffect4(read(ref2)))(function(v2) {
              var v3 = v1.value0(v2.state);
              if (unsafeRefEq(v2.state)(v3.value1)) {
                return pure8(v3.value0);
              }
              ;
              if (otherwise) {
                return discard1(liftEffect4(write({
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
                    return pure8(v3.value0);
                  });
                });
              }
              ;
              throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 86, column 7 - line 92, column 21): " + [v3.constructor.name]);
            });
          }
          ;
          if (v1 instanceof Subscribe) {
            return bind13(fresh(SubscriptionId)(ref2))(function(sid) {
              return bind13(liftEffect4(subscribe(v1.value0(sid))(function(act) {
                return handleAff(evalF(render2)(ref2)(new Action(act)));
              })))(function(finalize) {
                return bind13(liftEffect4(read(ref2)))(function(v2) {
                  return discard1(liftEffect4(modify_(map23(insert6(sid)(finalize)))(v2.subscriptions)))(function() {
                    return pure8(v1.value1(sid));
                  });
                });
              });
            });
          }
          ;
          if (v1 instanceof Unsubscribe) {
            return discard1(liftEffect4(unsubscribe3(v1.value0)(ref2)))(function() {
              return pure8(v1.value1);
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
            return bind13(liftEffect4(read(ref2)))(function(v2) {
              return bind13(liftEffect4(read(v2.handlerRef)))(function(handler3) {
                return discard1(queueOrRun(v2.pendingOuts)(handler3(v1.value0)))(function() {
                  return pure8(v1.value1);
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
            return bind13(fresh(ForkId)(ref2))(function(fid) {
              return bind13(liftEffect4(read(ref2)))(function(v2) {
                return bind13(liftEffect4($$new(false)))(function(doneRef) {
                  return bind13(fork3($$finally(liftEffect4(function __do2() {
                    modify_($$delete5(fid))(v2.forks)();
                    return write(true)(doneRef)();
                  }))(evalM(render2)(ref2)(v1.value0))))(function(fiber) {
                    return discard1(liftEffect4(unlessM2(read(doneRef))(modify_(insert1(fid)(fiber))(v2.forks))))(function() {
                      return pure8(v1.value1(fid));
                    });
                  });
                });
              });
            });
          }
          ;
          if (v1 instanceof Join) {
            return bind13(liftEffect4(read(ref2)))(function(v2) {
              return bind13(liftEffect4(read(v2.forks)))(function(forkMap) {
                return discard1(traverse_32(joinFiber)(lookup1(v1.value0)(forkMap)))(function() {
                  return pure8(v1.value1);
                });
              });
            });
          }
          ;
          if (v1 instanceof Kill) {
            return bind13(liftEffect4(read(ref2)))(function(v2) {
              return bind13(liftEffect4(read(v2.forks)))(function(forkMap) {
                return discard1(traverse_32(killFiber(error("Cancelled")))(lookup1(v1.value0)(forkMap)))(function() {
                  return pure8(v1.value1);
                });
              });
            });
          }
          ;
          if (v1 instanceof GetRef) {
            return bind13(liftEffect4(read(ref2)))(function(v2) {
              return pure8(v1.value1(lookup22(v1.value0)(v2.refs)));
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
        return liftEffect4(flip(modify_)(ref2)(mapDriverState(function(st) {
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
        return bind13(liftEffect4(read(ref2)))(function(v1) {
          return evalM(render2)(ref2)(v1["component"]["eval"](new Action2(v.value0, unit)));
        });
      }
      ;
      throw new Error("Failed pattern match at Halogen.Aff.Driver.Eval (line 52, column 20 - line 58, column 62): " + [v.constructor.name]);
    };
  };
};

// output/Halogen.Aff.Driver/index.js
var bind9 = /* @__PURE__ */ bind(bindEffect);
var discard6 = /* @__PURE__ */ discard(discardUnit);
var for_2 = /* @__PURE__ */ for_(applicativeEffect)(foldableMaybe);
var traverse_5 = /* @__PURE__ */ traverse_(applicativeAff)(foldableList);
var fork4 = /* @__PURE__ */ fork(monadForkAff);
var bindFlipped7 = /* @__PURE__ */ bindFlipped(bindEffect);
var traverse_13 = /* @__PURE__ */ traverse_(applicativeEffect);
var traverse_23 = /* @__PURE__ */ traverse_13(foldableMaybe);
var traverse_33 = /* @__PURE__ */ traverse_13(foldableMap);
var discard22 = /* @__PURE__ */ discard6(bindAff);
var parSequence_3 = /* @__PURE__ */ parSequence_(parallelAff)(applicativeParAff)(foldableList);
var liftEffect5 = /* @__PURE__ */ liftEffect(monadEffectAff);
var pure9 = /* @__PURE__ */ pure(applicativeEffect);
var map24 = /* @__PURE__ */ map(functorEffect);
var pure13 = /* @__PURE__ */ pure(applicativeAff);
var when2 = /* @__PURE__ */ when(applicativeEffect);
var renderStateX2 = /* @__PURE__ */ renderStateX(functorEffect);
var $$void5 = /* @__PURE__ */ $$void(functorAff);
var foreachSlot2 = /* @__PURE__ */ foreachSlot(applicativeEffect);
var renderStateX_2 = /* @__PURE__ */ renderStateX_(applicativeEffect);
var tailRecM3 = /* @__PURE__ */ tailRecM(monadRecEffect);
var voidLeft3 = /* @__PURE__ */ voidLeft(functorEffect);
var bind14 = /* @__PURE__ */ bind(bindAff);
var liftEffect1 = /* @__PURE__ */ liftEffect(monadEffectEffect);
var newLifecycleHandlers = /* @__PURE__ */ function() {
  return $$new({
    initializers: Nil.value,
    finalizers: Nil.value
  });
}();
var handlePending = function(ref2) {
  return function __do2() {
    var queue = read(ref2)();
    write(Nothing.value)(ref2)();
    return for_2(queue)(function() {
      var $59 = traverse_5(fork4);
      return function($60) {
        return handleAff($59(reverse2($60)));
      };
    }())();
  };
};
var cleanupSubscriptionsAndForks = function(v) {
  return function __do2() {
    bindFlipped7(traverse_23(traverse_33(unsubscribe)))(read(v.subscriptions))();
    write(Nothing.value)(v.subscriptions)();
    bindFlipped7(traverse_33(function() {
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
                initializers: new Cons(discard22(parSequence_3(reverse2(handlers.initializers)))(function() {
                  return discard22(parentInitializer)(function() {
                    return liftEffect5(function __do2() {
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
              return function __do2() {
                var lchs$prime = newLifecycleHandlers();
                var $$var2 = initDriverState(c)(j)(handler3)(lchs$prime)();
                var pre2 = read(lchs)();
                write({
                  initializers: Nil.value,
                  finalizers: pre2.finalizers
                })(lchs)();
                bindFlipped7(unDriverStateX(function() {
                  var $63 = render2(lchs);
                  return function($64) {
                    return $63(function(v) {
                      return v.selfRef;
                    }($64));
                  };
                }()))(read($$var2))();
                bindFlipped7(squashChildInitializers(lchs)(pre2.initializers))(read($$var2))();
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
                return function __do2() {
                  var childrenIn = map24(slot.pop)(read(childrenInRef))();
                  var $$var2 = function() {
                    if (childrenIn instanceof Just) {
                      write(childrenIn.value0.value1)(childrenInRef)();
                      var dsx = read(childrenIn.value0.value0)();
                      unDriverStateX(function(st) {
                        return function __do3() {
                          flip(write)(st.handlerRef)(function() {
                            var $65 = maybe(pure13(unit))(handler3);
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
                        var $67 = maybe(pure13(unit))(handler3);
                        return function($68) {
                          return $67(slot.output($68));
                        };
                      }())(slot.input)(slot.component)();
                    }
                    ;
                    throw new Error("Failed pattern match at Halogen.Aff.Driver (line 213, column 14 - line 222, column 98): " + [childrenIn.constructor.name]);
                  }();
                  var isDuplicate = map24(function($69) {
                    return isJust(slot.get($69));
                  })(read(childrenOutRef))();
                  when2(isDuplicate)(warn("Halogen: Duplicate slot address was detected during rendering, unexpected results may occur"))();
                  modify_(slot.set($$var2))(childrenOutRef)();
                  return bind9(read($$var2))(renderStateX2(function(v) {
                    if (v instanceof Nothing) {
                      return $$throw("Halogen internal error: child was not initialized in renderChild");
                    }
                    ;
                    if (v instanceof Just) {
                      return pure9(renderSpec2.renderChild(v.value0));
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
          return function __do2() {
            var v = read($$var2)();
            var shouldProcessHandlers = map24(isNothing)(read(v.pendingHandlers))();
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
              return function __do3() {
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
              return function __do3() {
                var handlers = read(v.pendingHandlers)();
                write(new Just(Nil.value))(v.pendingHandlers)();
                traverse_23(function() {
                  var $76 = traverse_5(fork4);
                  return function($77) {
                    return handleAff($76(reverse2($77)));
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
          return function __do2() {
            cleanupSubscriptionsAndForks(st)();
            var f = evalM(render2)(st.selfRef)(st["component"]["eval"](new Finalize(unit)));
            modify_(function(handlers) {
              return {
                initializers: handlers.initializers,
                finalizers: new Cons(f, handlers.finalizers)
              };
            })(lchs)();
            return foreachSlot2(st.children)(function(v) {
              return function __do3() {
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
            return bind14(liftEffect5(read(disposed)))(function(v) {
              if (v) {
                return pure13(Nothing.value);
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
            return handleLifecycle(lchs)(function __do2() {
              var v = read(disposed)();
              if (v) {
                return unit;
              }
              ;
              write(true)(disposed)();
              finalize(lchs)(dsx)();
              return unDriverStateX(function(v1) {
                return function __do3() {
                  var v2 = liftEffect1(read(v1.selfRef))();
                  return for_2(v2.rendering)(renderSpec2.dispose)();
                };
              })(dsx)();
            });
          };
        };
      };
      return bind14(liftEffect5(newLifecycleHandlers))(function(lchs) {
        return bind14(liftEffect5($$new(false)))(function(disposed) {
          return handleLifecycle(lchs)(function __do2() {
            var sio = create();
            var dsx = bindFlipped7(read)(runComponent(lchs)(function() {
              var $78 = notify(sio.listener);
              return function($79) {
                return liftEffect5($78($79));
              };
            }())(i2)(component3))();
            return unDriverStateX(function(st) {
              return pure9({
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
var getEffProp2 = function(name16) {
  return function(node) {
    return function() {
      return node[name16];
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
var map25 = /* @__PURE__ */ map(functorEffect);
var parentNode2 = /* @__PURE__ */ function() {
  var $6 = map25(toMaybe);
  return function($7) {
    return $6(_parentNode($7));
  };
}();
var nextSibling = /* @__PURE__ */ function() {
  var $15 = map25(toMaybe);
  return function($16) {
    return $15(_nextSibling($16));
  };
}();

// output/Halogen.VDom.Driver/index.js
var $runtime_lazy8 = function(name16, moduleName, init3) {
  var state3 = 0;
  var val;
  return function(lineNumber) {
    if (state3 === 2)
      return val;
    if (state3 === 1)
      throw new ReferenceError(name16 + " was needed before it finished initializing (module " + moduleName + ", line " + lineNumber + ")", moduleName, lineNumber);
    state3 = 1;
    val = init3();
    state3 = 2;
    return val;
  };
};
var $$void6 = /* @__PURE__ */ $$void(functorEffect);
var pure10 = /* @__PURE__ */ pure(applicativeEffect);
var traverse_6 = /* @__PURE__ */ traverse_(applicativeEffect)(foldableMaybe);
var unwrap5 = /* @__PURE__ */ unwrap();
var when3 = /* @__PURE__ */ when(applicativeEffect);
var not2 = /* @__PURE__ */ not(/* @__PURE__ */ heytingAlgebraFunction(/* @__PURE__ */ heytingAlgebraFunction(heytingAlgebraBoolean)));
var identity9 = /* @__PURE__ */ identity(categoryFn);
var bind15 = /* @__PURE__ */ bind(bindAff);
var liftEffect6 = /* @__PURE__ */ liftEffect(monadEffectAff);
var map26 = /* @__PURE__ */ map(functorEffect);
var bindFlipped8 = /* @__PURE__ */ bindFlipped(bindEffect);
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
      return pure10(unit);
    };
  };
};
var removeChild3 = function(v) {
  return function __do2() {
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
        var buildThunk2 = buildThunk(unwrap5)(spec);
        var $lazy_patch = $runtime_lazy8("patch", "Halogen.VDom.Driver", function() {
          return function(st, slot) {
            if (st instanceof Just) {
              if (slot instanceof ComponentSlot) {
                halt(st.value0);
                return $lazy_renderComponentSlot(100)(slot.value0);
              }
              ;
              if (slot instanceof ThunkSlot) {
                var step$prime = step2(st.value0, slot.value0);
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
              var step4 = buildThunk2(slot.value0);
              return mkStep(new Step(extract2(step4), new Just(step4), $lazy_patch(89), done));
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
        var patch2 = $lazy_patch(91);
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
              return function __do2() {
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
              return function __do2() {
                write(child)(v1.value0.renderChildRef)();
                var parent2 = parentNode2(v1.value0.node)();
                var nextSib = nextSibling(v1.value0.node)();
                var machine$prime = step2(v1.value0.machine, v);
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
      renderChild: identity9,
      removeChild: removeChild3,
      dispose: removeChild3
    };
  };
};
var runUI2 = function(component3) {
  return function(i2) {
    return function(element3) {
      return bind15(liftEffect6(map26(toDocument)(bindFlipped8(document)(windowImpl))))(function(document2) {
        return runUI(renderSpec(document2)(element3))(component3)(i2);
      });
    };
  };
};

// output/Main/index.js
var component2 = /* @__PURE__ */ component(monadAffAff);
var main2 = /* @__PURE__ */ runHalogenAff(/* @__PURE__ */ bind(bindAff)(awaitBody)(function(body2) {
  return runUI2(component2)(unit)(body2);
}));
export {
  main2 as main
};
