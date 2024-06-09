const pressedKeys = {};
const pressedButtons = {};
let mouseX = 0;
let mouseY = 0;


window.addEventListener('keydown', (event) => {
    pressedKeys[event.key] = true;
});

window.addEventListener('keyup', (event) => {
    delete pressedKeys[event.key];
});


window.addEventListener('mousedown', (event) => {
    pressedButtons[event.button] = true;
});

window.addEventListener('mouseup', (event) => {
    delete pressedButtons[event.button];
});

window.addEventListener('mousemove', (event) => {
    mouseX = event.clientX;
    mouseY = event.clientY;
});

// Function to return an array of currently pressed keys
export function _getPressedKeys() {
    return Object.keys(pressedKeys);
}

// Function to return an array of currently pressed mouse buttons
export function _getMousePressedButtons() {
    return Object.keys(pressedButtons);
}

// Function to return the current mouse coordinates
export function _getMouseCoordinates() {
    return { x: mouseX, y: mouseY };
}


export function _requestAnimationFrame(fn) {
      return function() {
        return window.requestAnimationFrame(fn);
      };
  }