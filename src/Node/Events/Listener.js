/* global exports */
"use strict";

// module Node.Events.Listener

// wrap a multivariate function for equality's sake
exports.mkListener = function(fn) {
  return function() {
    return function() {
      return fn.apply(null, arguments);
    }
  }
}

exports.listenerEq = function(a) {
  return function(b) {
    return a === b;
  }
}
