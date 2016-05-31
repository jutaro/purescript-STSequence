/* global exports */
"use strict";

// module Data.STSequence

exports.peekSTArrayImplUnsafe = function (xs,i) {
    return function () {
      return xs[i];
    };
};

exports.pokeSTArray = function (xs,i,a) {
  return function () {
    var ret = i >= 0 && i < xs.length;
    if (ret) xs[i] = a;
    return ret;
  };
};

exports.pokeAllSTArray = function (xs,i,a) {
  return function () {
    var length = a.length;
    var ret = i >= 0 && i + a.length <= xs.length;
    if (ret)
        for (var i2 = 0; i2 < length; i2++) {
            xs[i2+i] = a[i2];
        }
    return ret;
  };
};

exports.pushAllSTArray = function (xs,as) {
    return function () {
        return xs.push.apply(xs, as);
    };
};

exports.spliceSTArray = function (xs,i,howMany,bs) {
    return function () {
        return xs.splice.apply(xs, [i, howMany].concat(bs));
    };
};

exports.copyImpl = function (xs) {
  return function () {
    return xs.slice();
  };
};
