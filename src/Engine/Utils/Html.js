// export const _getElementById = function (elementName){
//   return function() {
//     document.getElementById(elementName);
//     }
// }


export function _getElementById(id) {
  return function (node) {
    return function () {
      return node.getElementById(id);
    };
  };
}