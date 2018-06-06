// // Exercise - 6
// var fs = require('fs');
// exports.lister = function (dir, extn, callback) {
//     fs.readdir(dir, function (err, list) {
//         if (err) {
//             callback(err);
//         } else {
//             var rslt = list.filter(function (fileName) {
//                 if (fileName.split('.')[1] === extn)
//                     return true;
//             });
//             callback(null, rslt);
//         }
//     });
// };
