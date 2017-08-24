require('./Main.purs').main();
console.log("df");

var x = new Float32Array([1.0, 2.0, 3.0, 4.0]);
var y = new Float32Array([1.0, 2.0, 3.0, 4.0]);

var v1 = { x: 1.0, y: 2.0, z: 3.0, w: 4.0 };
var v2 = { x: 1.0, y: 2.0, z: 3.0, w: 4.0 };

var m1 = [
  1.0, 2.0, 3.0,
  4.0, 5.0, 6.0,
  7.0, 8.0, 9.0
];

var m2 = [
  4.0, 5.0, 6.0,
  1.0, 2.0, 3.0,  
  7.0, 8.0, 9.0
];

var fm1 = new Float32Array(m1);
var fm2 = new Float32Array(m2);
var fm3 = new Float32Array(m1);

window.addEventListener("DOMContentLoaded", function () {
  


  new Benchmark.Suite()
  
    .add("FVector4 Add", function () {
      require('./Main.purs').add(x)(y)();
    })

    .add("Vector4 Add", function () {
      require('./Main.purs').addV(v1)(v2);
    })

    .on('complete', function () {
      var b1 = this[0];
      var b2 = this[1];

      console.log("FVector4 Add", b1.hz);
      console.log("Vector4 add", b2.hz);
      console.log("D = ", b1.hz - b2.hz);
    })
  
    .run();
  
  new Benchmark.Suite()

    .add("FMatrix3 mul", function () {
      require('./Main.purs').mulFM3(fm1)(fm2)(fm3)();
    })
  
    .add("Matrix3 mul", function () {
      require('./Main.purs').mulM3(m1)(m2);
    })

    .on('complete', function () {
      var b1 = this[0];
      var b2 = this[1];
      
      console.log("FMatrix3 mul", b1.hz);
      console.log("Matrix3 mul", b2.hz);
      console.log("D = ", b1.hz - b2.hz);
    }).run();

});
// for (var i = 0; i < 1000; i++) {
//   require('./Main.purs')
//     .add(x)(y)();
//   //console.log(x, y);
// }

// console.log(x, y);