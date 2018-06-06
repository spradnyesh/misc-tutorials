// Exercise - 1
// console.log("HELLO WORLD");

// Exercise - 2
// console.log(process
//             .argv
//             .splice(2)
//             .map(function (n) { return parseInt(n); })
//             .reduce(function (a, b) { return a + b; }, 0));

// Exercise - 3
// var fs = require('fs');
// var data = fs.readFileSync(process.argv[2]).toString();
// var newLineCount = 0;
// for (var i = 0; i < data.length; i++) {
//     if (data[i] === '\n') newLineCount++;
// }
// console.log(newLineCount);

// Exercise - 4
// var fs = require('fs');
// fs.readFile(process.argv[2], function (err, data) {
//     var content = data.toString();
//     var newLineCount = 0;
//     for (var i = 0; i < content.length; i++) {
//         if (content[i] === '\n') newLineCount++;
//     }
//     console.log(newLineCount);
// });

// Exercise - 5
// var fs = require('fs');
// var dir = process.argv[2];
// var extn = process.argv[3];
// fs.readdir(dir, function (err, list) {
//     for (var i = 0; i < list.length; i++) {
//         if (list[i].split('.')[1] === extn) console.log(list[i]);
//     }
// });

// Exercise - 6
// var modules = require('./modules.js');
// modules.lister(process.argv[2], process.argv[3], function (err, data) {
//     if (err)
//         console.log(err);
//     for (var i = 0; i < data.length; i++) {
//         console.log(data[i]);
//     }
// });

// Exercise - 7
// var http = require('http');
// http.get(process.argv[2], function (resp) {
//     resp.setEncoding('utf8');
//     resp.on('data', console.log);
//     resp.on('error', console.error);
//     resp.on('end', function () {});
// });

// Exercise - 8.1
// var http = require('http');
// var rslt = '';
// http.get(process.argv[2], function (resp) {
//     resp.setEncoding('utf8');
//     resp.on('data', function (data) { rslt += data; })
//     resp.on('error', console.error);
//     resp.on('end', function () {
//         console.log(rslt.length);
//         console.log(rslt);
//     });
// });

// Exercise - 8.2
// var http = require('http')
// var bl = require('bl')
// http.get(process.argv[2], function (resp) {
//     resp.pipe(bl(function (err, data) {
//         if (err) return console.error(err)
//         data = data.toString()
//         console.log(data.length)
//         console.log(data)
//     }))
// })

// Exercise 9
// *** NOT COMPLETED ***
// var http = require('http')
// var urls = process.argv.splice(2, 3)
// var rslt = {}
// function callback () {
//     if (Object.keys(rslt).length === urls.length) {
//         for (var i = 0; i < urls.length; i++) {
//             console.log(rslt[urls[i]])
//         }
//     }
// }
// function makeClosure (url) {
//     // url is the closed variable
//     return function (resp) {
//         resp.setEncoding('utf8')
//         resp.on('data', function (data) {
//             rslt[url] = data // .split('\n')[0]
//             callback()
//         })
//     }
// }
// for (var i = 0; i < urls.length; i++) {
//     http.get(urls[i], makeClosure(urls[i]))
// }
