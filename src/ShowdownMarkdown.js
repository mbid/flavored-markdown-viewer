exports.markdownToHtml = function (markdown) {
  var converter = new showdown.Converter();
  converter.setFlavor('github');
  return converter.makeHtml(markdown);
}
