exports.scrollIntoView = function (el) {
  return function (alignToTop) {
    return function () {
      el.scrollIntoView(el, alignToTop);
    }
  }
};
