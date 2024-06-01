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

// foreign import data MouseEvent :: Type
// foreign import _addEventListenerMouseMove :: forall a. (MouseEvent -> Effect a) -> Effect a


// const box = document.getElementById("canvas");
// const pageX = document.getElementById("x");
// const pageY = document.getElementById("y");

// function updateDisplay(event) {
//   pageX.innerText = event.pageX;
//   pageY.innerText = event.pageY;
// }

// box.addEventListener("mousemove", updateDisplay, false);
// box.addEventListener("mouseenter", updateDisplay, false);
// box.addEventListener("mouseleave", updateDisplay, false);
