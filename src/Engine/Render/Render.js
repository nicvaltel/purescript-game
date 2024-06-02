export const _renderObject = function (o) {
  return function () {
    var el = document.getElementById(o.id);
    el.setAttribute('class', o.css);
    el.setAttribute('style', 'left: '
      + (o.baseX + (o.x | 0)) + 'px; top: '
      + (o.baseY + (o.y | 0)) + 'px');
  }
}