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
    if (typeof currentAuth.serverUrl !== "string") clearAuth();
    if (typeof currentAuth.token !== "string") clearAuth();
    if (typeof currentAuth.tokenId !== "string") clearAuth();
    if (typeof currentAuth.validUntil !== "number") clearAuth();
} else {
    clearAuth();
}

var serverUrl = getUrlParameter("serverUrl");
if (typeof serverUrl !== "string") serverUrl = "";

var invitationCode = getUrlParameter("invitationCode");
if (typeof invitationCode !== "string" || invitationCode.length < 1) invitationCode = null;

var timezoneOffset = (new Date).getTimezoneOffset() * -1;

var app = Elm.Main.embed(mountNode, { timezoneOffset: timezoneOffset, auth: currentAuth, serverInput: serverUrl, invitationCode: invitationCode });

app.ports.sendAlert.subscribe(function (msg) {
    iziToast.show({
        title: 'Alert!',
        message: msg,
        color: 'red',
        position: 'topRight',
        animateInside: false,
        balloon: true
    });
});

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
