//http://html5demos.com/js/h5utils.js
var addEvent = (function () {
    if (document.addEventListener) {
        return function (el, type, fn) {
            if (el && el.nodeName || el === window) {
                el.addEventListener(type, fn, false);
            } else if (el && el.length) {
                for (var i = 0; i < el.length; i++) {
                    addEvent(el[i], type, fn);
                }
            }
        };
    } else {
        return function (el, type, fn) {
            if (el && el.nodeName || el === window) {
                el.attachEvent('on' + type, function () { return fn.call(el, window.event); });
            } else if (el && el.length) {
                for (var i = 0; i < el.length; i++) {
                    addEvent(el[i], type, fn);
                }
            }
        };
    }
})();

//http://starikovs.com/2010/03/10/test-for-empty-js-object/
function empty(o) {
    for(var i in o)
        if(o.hasOwnProperty(i))
            return false;

    return true;
}

function length(o) {
    var l = 0;
    for (var i in o)
        l++;
    return l;
}
