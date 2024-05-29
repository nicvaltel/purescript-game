const canvas = document.getElementById("canvas");
const ctx = canvas.getContext("2d");
const image = document.getElementById("jupiter");

export const _drawJupiter = function(img) {
  return function(x){
    return function(y){
      return function () {
        ctx.drawImage(img, x, y);
      }
    }
  }
};

export const _getElementById = function(id) {
  return function () {
    return document.getElementById(id);
  }
};

// export const _getElementById = function(id) {
//   return document.getElementById(id).value;
// };

export const _setElementById = function(id) {
  return function(value) {
  document.getElementById(id).value = value;
  };
};


    // // В отличие от изображений, у текстур нет определённой ширины и высоты,
    // // поэтому мы сами установим ширину и высоту текстуры
    // function drawImage1(tex, texWidth, texHeight, dstX, dstY) {
    //     gl.bindTexture(gl.TEXTURE_2D, tex);
       
    //     // Указываем нашу шейдерную программу для WebGL
    //     gl.useProgram(program);
       
    //     // Настраиваем атрибуты для получения данных из буферов
    //     gl.bindBuf2fer(gl.ARRAY_BUFFER, positionBuffer);
    //     gl.enableVertexAttribArray(positionLocation);
    //     gl.vertexAttribPointer(positionLocation, 2, gl.FLOAT, false, 0, 0);
    //     gl.bindBuffer(gl.ARRAY_BUFFER, texcoordBuffer);
    //     gl.enableVertexAttribArray(texcoordLocation);
    //     gl.vertexAttribPointer(texcoordLocation, 2, gl.FLOAT, false, 0, 0);
       
    //     // матрица для конвертации из пикселей в пространство отсечения
    //     var matrix = m4.orthographic(0, gl.canvas.width, gl.canvas.height, 0, -1, 1);
       
    //     // матрица переноса квадранта в координаты dstX, dstY
    //     matrix = m4.translate(matrix, dstX, dstY, 0);
       
    //     // эта матрица растянет наш единичный квадрант
    //     // до размеров texWidth, texHeight
    //     matrix = m4.scale(matrix, texWidth, texHeight, 1);
       
    //     // устанавливаем матрицу
    //     gl.uniformMatrix4fv(matrixLocation, false, matrix);
       
    //     // указываем шейдеру, что текстуры нужно брать из блока 0
    //     gl.uniform1i(textureLocation, 0);
       
    //     // отрисовка квадранта (2 треугольника, 6 вершин)
    //     gl.drawArrays(gl.TRIANGLES, 0, 6);
    //   }