"use strict";

// module Timer

exports.timeout = function(millis) {
    return function(action) {
        return function() {
            return setTimeout(action, millis);
        };
    };
};

exports.clearTimeout = function(timer) {
    return function() {
        clearTimeout(timer);
    };
};

exports.nowEpochMilliseconds = function () {
  return Date.now();
};
