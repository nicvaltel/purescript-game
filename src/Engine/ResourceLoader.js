export const _getHtmlElenentById = function (name) {
  return function () {
    return document.getElementById(name);
  }
}