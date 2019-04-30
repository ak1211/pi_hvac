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
      var value = options.value;
      options.value = options.minValue;
      var gauge = new CanvasGauges.RadialGauge(options);
      gauge.draw();
      gauge.value = value;
      return gauge;
    };
  };
};


