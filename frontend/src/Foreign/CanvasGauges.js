"use strict";

var CanvasGauges = require('canvas-gauges');

exports.destroyGaugeInstance = function (gaugeJsInstance) {
  return function () {
    gaugeJsInstance.destroy();
  };
};

exports.createRadialGauge = function (element) {
  return function (options) {
    return function () {
      options.renderTo = element;
      var gauge = new CanvasGauges.RadialGauge(options);
      gauge.draw();
      return gauge;
    };
  };
};

exports.updateRadialGauge = function (gaugeJsInstance) {
  return function (options) {
    return function () {
      for (var optname in options) {
        gaugeJsInstance[optname] = options[optname];
      }
      gaugeJsInstance.update();
    };
  };
};
