// ==UserScript==
// @name Pradnyesh's script for removing the left panel of twiki.cyc
// @namespace http://www.spradnyesh.org
// @match http://*wiki.corp.yahoo.com/view/*
// @description remove '#patternLeftBar'
// ==/UserScript==

var wrap = document.getElementById('patternFloatWrap');
wrap.removeChild(document.getElementById('patternLeftBar'));

var outer = document.getElementById('patternOuter');
outer.style.marginLeft="0";
