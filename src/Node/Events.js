/* global exports */
"use strict";

// module Node.Events

exports.emitter = function() {
  var EE = require('events').EventEmitter;
  return new EE();
}

exports.emitterHelper1 = function(__dict) {
  return function(method) {
    return function(event) {
      return function(emitter) {
        return function() {
          return emitter[method](event);
        }
      }
    }
  }
}

exports.emitterHelper2 = function(__emitter) {
  return function(__variadic) {
    return function(method) {
      return function(event) {
        return function(cb) {
          return function(emitter) {
            return function() {
              return emitter[method](event, function() {
                return cb.apply(this, arguments)();
              }.bind(this));
            }
          }
        }
      }
    }
  }
}

exports.emit = function(__dict) {
  return function(event) {
    return function(arg) {
      return function(emitter) {
        return function() {
          return emitter.emit(event, arg);
        }
      }
    }
  }
}
