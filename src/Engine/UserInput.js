function getOffset(el) {
  const rect = el.getBoundingClientRect();
  return {
    x: rect.left + window.scrollX,
    y: rect.top + window.scrollY
  };
}

export const _getElementCoord = function (elementName){
  const el = document.getElementById(elementName);
  return function() {
    if (el){
    const pos = getOffset(el);
    console.log(pos);
    getOffset(el);
    } else {
      return {};
    }
  }
}

const _addEventListenerMouseRelativeMove = function (signalFunc) {
  return function(canvasName){
    const box = document.getElementById(canvasName);
    return function () {
      box.addEventListener("mousemove", (event) => {
        console.log({x : event.pageX, y : event.pageY});
        return signalFunc({x : event.pageX, y : event.pageY
        })();}, false);
    }
  }
};
