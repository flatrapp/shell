'use strict';

require('bootstrap');
require('bootstrap/dist/css/bootstrap.css');
require('font-awesome/css/font-awesome.css');

// Require index.html so it gets copied to dist
require('./index.html');

var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');

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

const timezoneOffset = (new Date).getTimezoneOffset() * -1;

var app = Elm.Main.embed(mountNode, { timezoneOffset: timezoneOffset, auth: currentAuth });

app.ports.sendAlert.subscribe(function (msg) {
  alert(msg);
});


app.ports.saveAuthLocalStorage.subscribe(function (auth) {
    localStorage.setItem("auth", JSON.stringify(auth));
});

app.ports.clearAuthLocalStorage.subscribe(function () {
    clearAuth();
});
