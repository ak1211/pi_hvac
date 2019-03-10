"use strict";

var Chart = require('chart.js');

exports.destroyLineChartJs = function (chartJsInstance) {
  return function () {
    chartJsInstance.destroy();
  };
};

exports.lineChartJs = function (context) {
  return function (datasets) {
    return function (options) {
      return function () {
        return new Chart(context, {
          type: 'line',
          data: datasets,
          options: options
        });
      };
    };
  };
};
