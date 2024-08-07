"use strict";

var wsConnectionIsOpen = false;

// init web-socket connection
export const _wsocket = function (url) {
    return function () {
        return (new WebSocket(url));
    };
};

// Event listener for when the WebSocket connection is opened
export const _addEventListenerConnectionIsOpen = function (socket) {
    return function () {
        socket.addEventListener('open', (event) => {
            wsConnectionIsOpen = true;
            console.log('WebSocket connection opened:', event);
            // Send a welcome message or perform any initial setup here
        });
    }
};

export const _webSocketConnectionStatusIsOpen = function (socket) {
    return function () {
        return (wsConnectionIsOpen && socket.readyState !== WebSocket.CLOSED);
    }
};

// Event listener to make signal at input message. signalFunc :: String -> Effect Unit
export const _addEventListenerMessageRecieved = function (socket) {
    return function (signalFunc) {
        return function () {
            socket.addEventListener('message', (event) => {
                signalFunc(event.data)();
            });
        }
    }
};

// Event listener for when the WebSocket connection is closed
export const _addEventListenerConnectionIsClose = function (socket) {
    return function () {
        socket.addEventListener('close', (event) => {
            wsConnectionIsOpen = false;
            console.log('WebSocket connection closed:', event);
            // You can handle reconnection or other actions here
        });
    }
};


export const _sendMessage = function(socket) {
    return function (message) {
        return function(){
            if (socket && socket.readyState == WebSocket.OPEN && socket.readyState !== WebSocket.CLOSED){ // check not null or undefined
                // socket.send('Hello, server!');
                socket.send(message);
            }
        }
    }
};