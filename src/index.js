'use strict';

require('bootstrap');
require('bootstrap/dist/css/bootstrap.css');
require('font-awesome/css/font-awesome.css');
var iziToast = require('izitoast/dist/js/iziToast.min.js');
require('izitoast/dist/css/iziToast.min.css');

// Require index.html so it gets copied to dist
require('./index.html');

function getUrlParameter(name) {
    name = name.replace(/[\[]/, '\\[').replace(/[\]]/, '\\]');
    var regex = new RegExp('[\\?&]' + name + '=([^&#]*)');
    var results = regex.exec(location.search);
    return results === null ? '' : decodeURIComponent(results[1].replace(/\+/g, ' '));
}

// Check if there is an authentication object stored in localStorage and enforce correct types
// We don't want out Elm app crashing
var currentAuthString = localStorage.getItem("auth");
var currentAuth = null;
var clearAuth = function () { localStorage.setItem("auth", JSON.stringify(null)); currentAuth = null; };

try {
    currentAuth = JSON.parse(currentAuthString);
} catch (e) {
    clearAuth();
}
try {
if (typeof currentAuth === "object" && currentAuth !== null) {
    if (typeof currentAuth.serverUrl !== "string") clearAuth();
    if (typeof currentAuth.token !== "string") clearAuth();
    if (typeof currentAuth.tokenId !== "string") clearAuth();
    if (typeof currentAuth.validUntil !== "number") clearAuth();
} else {
    clearAuth();
}
} catch (e) {
    console.log("An error occured while reading data from the local storage, resetting...");
    clearAuth();
}

// Try to read the serverInput value from localStorage and overwrite it by ""
// in case it's invalid
var serverInput = localStorage.getItem("serverInput");
if (typeof serverInput !== "string" || serverInput.length < 1) serverInput = "";

// If there is a valid serverUrl passed as a GET parameter, use this one instead
var serverUrl = getUrlParameter("serverUrl");
if (typeof serverUrl === "string" && serverUrl.length > 1) serverInput = serverUrl;

// If there is a valid invitationCode passed as a GET paramter, pass it to the application
// Else, pass null (implemented as Maybe String)
var invitationCode = getUrlParameter("invitationCode");
if (typeof invitationCode !== "string" || invitationCode.length < 1) invitationCode = null;

// Get the timezone offset in minutes
// Needs to be multiplied by -1 to get the standard UTC+${minutes} format
var timezoneOffset = (new Date).getTimezoneOffset() * -1;


// Mount the app
var Elm = require('./Main.elm');
var mountNode = document.getElementById('main');
var app = Elm.Main.embed(mountNode, { timezoneOffset: timezoneOffset, auth: currentAuth, serverInput: serverInput, invitationCode: invitationCode });

app.ports.sendToastObject.subscribe(function (toast) {
    iziToast.show({
        title: toast.title,
        message: toast.message,
        color: toast.color,
        position: 'topRight',
        animateInside: false
    });
});


app.ports.saveAuthLocalStorage.subscribe(function (auth) {
    localStorage.setItem("auth", JSON.stringify(auth));
});

app.ports.clearAuthLocalStorage.subscribe(function () {
    clearAuth();
});

app.ports.saveServerInput.subscribe(function (serverInput) {
    localStorage.setItem("serverInput", serverInput);
});

app.ports.clearServerInput.subscribe(function () {
    localStorage.setItem("");
});
