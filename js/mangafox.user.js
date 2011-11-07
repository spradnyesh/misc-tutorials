// ==UserScript==
// @name Pradnyesh's script for mangafox image resizing
// @namespace http://www.spradnyesh.org
// @match http://www.mangafox.com/manga/*/v*/c*/*.html
// @description remove 'width: 740px' of 'viewer'; 'width="728"' of 'image'; enable 'prev' button for vimperator
// ==/UserScript==

// remove 'width: 740px' of 'viewer'
var viewer = document.getElementById('viewer')
viewer.style.width="1456px";
viewer.style.background="none repeat scroll 0 50% transparent";

// 'width="728"' of 'image'
var image = document.getElementById('image'), img = new Image(), a = image.parentNode;
img.src = image.src;
a.removeChild(image);
a.appendChild(img);

// enable 'prev' button for vimperator
var prevs = document.getElementsByClassName('prev_page');
for (var i = 0; i < prevs.length; i++) {
    prevs[i].innerHTML = '<span></span>previous page';
}
