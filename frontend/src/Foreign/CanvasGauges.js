"use strict";

var CanvasGauges = require('canvas-gauges');

exports.destroyGaugeInstance = function (gaugeJsInstance) {
  return function () {
    gaugeJsInstance.destroy();
  };
};

exports.radialGauge = function (element) {
  return function (options) {
    return function () {
      options.renderTo = element;
      var gauge = new CanvasGauges.RadialGauge(options);
      gauge.draw();
      return gauge;
    };
  };
};


