/* global exports */
"use strict";

// module Main

exports.getMousePos =
  function getMousePos(e) {
      return function() {
        var mouseX, mouseY;

        if(e.offsetX) {
            mouseX = e.offsetX;
            mouseY = e.offsetY;
        }
        else if(e.layerX) {
            mouseX = e.layerX;
            mouseY = e.layerY;
        }
        return [mouseX,mouseY];
      };
  };

exports.getJSON =
  function getJSON(uri) {
    return function(act) {
      return function() {
        $.getJSON(uri, function(data) {
          act(data)();
        });
      };
    };
  };

exports.addCommand =
  function addCommand(editor) {
    return function(cmdName) {
      return function(winKey) {
        return function(macKey) {
          return function(cmd) {
            return function() {
              editor.commands.addCommand({
                name: cmdName,
                bindKey: {win: winKey,  mac: macKey},
                exec: function(editor) {
                  cmd(editor)();
                }
              });
            };
          };
        };
      };
    };
  };

exports.tokenTooltip =
  function tokenTooltip(editor) {
    return function(getTy) {
      return function() {
          editor.tokenTooltip = new myTokenTooltip(editor,getTy);
      };
    };
  };

exports.addMarkerImpl =
  function addMarkerImpl(range, clazz, type, inFront, self) {
    return function() {
      return self.addMarker(range, clazz, type, inFront);
    };
  };

exports.getUrlParameter =
  function getUrlParameter(sParam) {
    return function () {
      var sPageURL = decodeURIComponent(window.location.search.substring(1)),
          sURLVariables = sPageURL.split('&'),
          sParameterName,
          i;
  
      for (i = 0; i < sURLVariables.length; i++) {
          sParameterName = sURLVariables[i].split('=');
  
          if (sParameterName[0] === sParam) {
              return sParameterName[1] === undefined ? true : sParameterName[1];
          }
      }
      return "";
    };
  };
