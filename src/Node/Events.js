/* global exports */
"use strict";

// module Node.Events

exports.newEventEmitter = function() {
  var EE = require('events').EventEmitter;
  return new EE();
}

exports.getDefaultMaxListeners = function() {
  return require('events').EventEmitter.defaultMaxListeners;
}

exports.setDefaultMaxListeners = function(n) {
  return function() {
    require('events').EventEmitter.defaultMaxListeners = n;
  }
}

exports.listenerCount = function(event) {
  return function(emitter) {
    require('events').EventEmiter.listenerCount(emitter, event);
  }
}

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
