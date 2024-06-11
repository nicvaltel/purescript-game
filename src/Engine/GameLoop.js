export function _requestAnimationFrame(fn) {
  // return function(window) {
    return function() {
      return window.requestAnimationFrame(fn);
    };
  // };
}

export function _removeElementById(elementId) {
  return function () {
    var element = document.getElementById(elementId);
    if (element && element.parentNode) {
      element.parentNode.removeChild(element);
    }
  };
};

// // o :: {canvasElem : HTMLElement, x,y : Number, imageSource : string}
// export function _createImageElement (o) {
//   return function () {
//     var img = document.createElement("img");
//     img.src = o.imageSource;
//     img.style.position = "absolute";
//     img.style.left = o.x + "px";
//     img.style.top = o.y + "px";
//     img.setAttribute('class',o.css);
//     o.canvasElem.appendChild(img);
//     return img;
//   };
// };


// o :: {canvasElem : HTMLElement, x,y : Number, imageSource : string, divId : string, css : string}
export function _createImageElement(o) {
    return function(){
      var newDiv = document.createElement("div");
      newDiv.id = o.divId;
      // newDiv.style.backgroundImage = "url('" + o.imageSource + "')"; 
      newDiv.style.position = "absolute";
      newDiv.style.left = o.x + "px";
      newDiv.style.top = o.y + "px";
      newDiv.className = o.cssClass;
      // newDiv.setAttribute('class','ball');
      // newDiv.style.backgroundColor = "red";
      o.canvasElem.appendChild(newDiv);
      return newDiv;
    }
}



// export function _requestAnimationFrame(fn) {
//   return window.requestAnimationFrame(fn) || window.webkitRequestAnimationFrame(fn) || window.mozRequestAnimationFrame(fn) || window.oRequestAnimationFrame(fn) || window.msRequestAnimationFrame(fn) ||
// 	function() {
// 		window.setTimeout(fn, 1000 / 60);
// 	};
// }