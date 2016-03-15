/* global exports */
"use strict";

// module Data.STSequence

exports.runSTArray = function (f) {
  return f;
};

exports.emptySTArray = function () {
  return [];
};

exports.peekSTArrayImpl = function (just,nothing,xs,i) {
    return function () {
      return i >= 0 && i < xs.length ? just(xs[i]) : nothing;
    };
};

exports.pokeSTArray = function (xs,i,a) {
  return function () {
    var ret = i >= 0 && i < xs.length;
    if (ret) xs[i] = a;
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
