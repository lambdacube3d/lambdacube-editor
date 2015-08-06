/* global exports */
"use strict";

// module WebSocket

exports.webSocketImpl =
  function webSocketImpl(uri) {
   return function(h) {
    return function(ok) {
     return function(err) {
      return function() {
        try {
          var ws = new WebSocket(uri);
          ws.onerror = function(event) {
            h.onError(ws)(event.data)();
          };
  
          ws.onopen = function() {
            h.onOpen(ws)();
          };
  
          ws.onclose = function() {
            h.onClose();
          };
  
          ws.onmessage = function(event) {
            h.onMessage(ws)(event.data)();
          };
          return ok(ws);
        } catch (e) {
          console.log("exception");
          console.log(e);
          console.log(e.type);
          return err(e.type);
        }
      };
     };
    };
   };
  };

exports.send =
  function send (socket) {
    return function (data) {
      return function () {
        socket.send(data);
      };
    };
  };
