// export const _renderObject = function (o) {
//   return function () {
//     // var el = document.getElementById(o.id);
//     // o.elem.setAttribute('class', o.cssClass);
//     const attr = 
//       'left: ' + (o.baseX + (o.x | 0)) + 'px' + 
//       '; top: ' + (o.baseY + (o.y | 0)) + 'px' +
//       '; z-index: ' + (o.z | 0) + 
//       '; transform: rotate(' + o.angle + 'deg)'
//       ;
//     o.elem.setAttribute('style', attr);
//   }
// }


export function _renderObject(o) {
  return function(){
    o.elem.style.left = (o.baseX + (o.x | 0)) + 'px';
    o.elem.style.top = (o.baseY + (o.y | 0)) + 'px';
    // o.elem.style.zIndex = (o.z | 0);
    o.elem.style.transform = 'rotate(' + (o.angle | 0) + 'deg)';
  }
}