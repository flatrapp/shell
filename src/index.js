'use strict';

require('bootstrap');
require('bootstrap/dist/css/bootstrap.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

// .embed() can take an optional second argument. This would be an object describing the data we need to start a program, i.e. a userID or some token



//var toastNotificationHandler = function(n) {
//  alert(JSON.stringify(n));
//};
//app.ports.toastNotification.subscribe(toastNotificationHandler);

var currentAuthString = localStorage.getItem("auth");
var currentAuth = null;
var clearAuth = function () { localStorage.setItem("auth", JSON.stringify(null)); currentAuth = null; };

try {
    currentAuth = JSON.parse(currentAuthString);
} catch (e) {
    clearAuth();
}

if (typeof currentAuth === "object" && currentAuth !== null) {
    if (typeof currentAuth.token !== "string") clearAuth();
    if (typeof currentAuth.tokenId !== "string") clearAuth();
    if (typeof currentAuth.validUntil !== "number") clearAuth();
} else {
    clearAuth();
}

console.log(currentAuth);
var app = Elm.Main.embed(mountNode, { auth: currentAuth });

app.ports.sendAlert.subscribe(function (msg) {
  alert(msg);
});


app.ports.saveAuthLocalStorage.subscribe(function (auth) {
    localStorage.setItem("auth", JSON.stringify(auth));
});

app.ports.clearAuthLocalStorage.subscribe(function () {
    clearAuth();
});
