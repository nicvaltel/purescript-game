export function _requestAnimationFrame(fn) {
  // return function(window) {
    return function() {
      return window.requestAnimationFrame(fn);
    };
  // };
}

// export function _requestAnimationFrame(fn) {
//   return window.requestAnimationFrame(fn) || window.webkitRequestAnimationFrame(fn) || window.mozRequestAnimationFrame(fn) || window.oRequestAnimationFrame(fn) || window.msRequestAnimationFrame(fn) ||
// 	function() {
// 		window.setTimeout(fn, 1000 / 60);
// 	};
// }