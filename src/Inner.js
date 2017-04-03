exports.setInnerHTML = function (html) {
  return function (el) {
    return function () {
      el.innerHTML = html;
    };
  };
};
exports.getInnerHTML = function (el) {
  return function () {
    return el.innerHTML;
  };
};

exports.getInnerText = function (el) {
  return function () {
    return el.innerText;
  };
};
exports.setInnerText = function (el) {
  return function (text) {
    function () {
      el.innerText = text;
    }
  }
};
