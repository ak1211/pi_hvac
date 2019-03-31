"use strict";

exports.showToastJs = function () {
  $('.toast').toast({ autohide: false }).toast('show');
};

exports.enablePopoverJs = function () {
  $('[data-toggle="popover"]').popover('enable');
};

exports.disablePopoverJs = function () {
  $('[data-toggle="popover"]').popover('disable');
};
exports.disposePopoverJs = function () {
  $('[data-toggle="popover"]').popover('dispose');
};
