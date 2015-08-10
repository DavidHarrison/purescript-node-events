/* global exports */
"use strict";

// module Node.Events

// Class Methods

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
