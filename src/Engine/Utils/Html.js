// export const _getElementById = function (elementName){
//   return function() {
//     document.getElementById(elementName);
//     }
// }


export function _getNodeElementById(id) {
  return function (node) {
    return function () {
      return node.getElementById(id);
    };
  };
}